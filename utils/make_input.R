###################################################################################################
# This program takes a shapefile glacier outline and up to additional 2 outlines                  #
# (firn, debris), and produces grids which can be used in the mass balance model:                 #
# DHM, surface type and optionally daily incoming solar radiation.                                #
# The 2 additional shapefiles are optional: in case they are not provided, the output grid        #
# only has ice and rock (no firn and no debris).                                                  #
# Author: Enrico Mattea (University of Fribourg)                                                  #
###################################################################################################

suppressPackageStartupMessages(library(lwgeom))
suppressPackageStartupMessages(library(insol2))
suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tools))

debug_verbose <- TRUE

#### Functions called by the app ####
# This function does its best to match a malformed UTM coordinate system
# (i.e. one which is not automatically recognized as a UTM) with the corresponding known one.
# wkt_malformed is the output of st_crs(object)$wkt.
# To find a suitable candidate, we look at the first line of the WKT.
# If the first line contains:
# "UTM" and <NN>( ){0,2}[N,S] or various combinations thereof,
# then we interpret the projection as UTM.
# Else we return NA and we will throw an error.
func_recover_utm_crs <- function(wkt_malformed) {
  
  # Remove extra whitespaces.
  wkt_malformed_v2 <- gsub("( ){2,}", " ", wkt_malformed)
  
  wkt_split <- strsplit(wkt_malformed_v2, "\n")[[1]]
  wkt_line_first <- wkt_split[[1]]
  
  # This regexp tenaciously creates 5 capture groups:
  # U(...)
  # T(...)
  # M(...)
  # <zone number>
  # N or S
  regexp_utm <- "((?:universal)|U){1}[ _-]{0,2}((?:transverse)|T){1}[ _-]{0,2}((?:mercator)|M){1}(?:[^0-9])*([0-9]{1,2})[ _-]*([NS]{1})"
  
  utm_match <- regmatches(wkt_line_first,regexec(regexp_utm, wkt_line_first, ignore.case = TRUE))
  if (length(utm_match) == 1) {
    utm_match <- utm_match[[1]][2:length(utm_match[[1]])]
    utm_zone <- as.integer(utm_match[4])
    utm_ns <- utm_match[5]
    utm_code <- 32600 + utm_zone + c(0,100)[2 - (utm_ns == "N")]
    return(utm_code)
  } else {
    return(NA_integer_)
  }
  
}

# This function checks whether the CRS of a raster is recognized or not.
# If not, it calls a UTM repair function to try to deduce the CRS.
func_repair_rast_crs <- function(rast_cur) {
  if (is.na(terra::crs(rast_cur, describe = T)$code)) {
    dem_crs_tentative_code <- func_recover_utm_crs(st_crs(rast_cur)$wkt)
    if (!is.na(dem_crs_tentative_code)) {
      dem_crs_epsg <- paste0("EPSG:", dem_crs_tentative_code)
      terra::crs(rast_cur) <- terra::crs(dem_crs_epsg)
      message("WARNING! Coordinates system of a grid was malformed, but I was able to fix it as ", dem_crs_epsg, ". I will continue.")
    } else {
      stop(NULL)
    }
  } else {
    return(rast_cur)
  }
}

# This function checks whether the CRS of a vector is recognized or not.
# If not, it calls a UTM repair function to try to deduce the CRS.
func_repair_vect_crs <- function(vect_cur) {
  if (is.na(terra::crs(vect_cur, describe = T)$code)) {
    outl_crs_tentative_code <- func_recover_utm_crs(st_crs(vect_cur)$wkt)
    if (!is.na(outl_crs_tentative_code)) {
      outl_crs_epsg <- paste0("EPSG:", outl_crs_tentative_code)
      st_crs(vect_cur) <- st_crs(outl_crs_tentative_code)
      message("WARNING! Coordinates system of the outline was malformed, but I was able to fix it as ", outl_crs_epsg, ". I will continue.")
      return(vect_cur)
    } else {
      return(NULL)
    }
  } else {
    return(vect_cur)
  }
}


# This function computes gridded total potential incoming solar radiation for one specific day.
# norm_mat: matrix with surface normals
# lat, lon: DEM center, as reference to compute day length
# ele_ref: elevation to use for the calculation. We use the mean of the DEM.
# delta_t: time-step of the computation, in hours
func_compute_day_rad <- function(dem_mat,
                                 norm_mat,
                                 dem_res,
                                 lat,
                                 lon,
                                 ele_ref,
                                 year_cur,
                                 doy_cur,
                                 delta_t) {
  
  date_cur <- as.Date(paste(year_cur, doy_cur), format = "%Y %j")
  month_cur <- as.integer(format(date_cur, "%m"))
  day_cur <- as.integer(format(date_cur, "%d"))
  jd_cur <- JDymd(year_cur, month_cur, day_cur)
  
  Iglobal <- array(0, dim = dim(dem_mat))
  
  dayl <- daylength(lat, lon, jd_cur, 0)
  
  for (hour_cur in seq(dayl[1], dayl[2], delta_t)) {
    
    jd_cur <- JDymd(year_cur, month_cur, day_cur, hour_cur)
    sun_vec <- sunvector(jd_cur, lat, lon, 0)
    hillshade_cur <- hillshading(norm_mat, sun_vec)
    shaded <- doshade(dem_mat, sun_vec, dem_res)
    sun_zenith <- degrees(acos(sun_vec[,3]))
    # Compute direct radiation modified by terrain + diffuse irradiation (sky-view factor ignored).
    Idirdif = insolation(sun_zenith, jd_cur, ele_ref, visibility, rh, tempK, O3, alphag)
    Iglobal = Iglobal + (Idirdif[,1] * hillshade_cur + Idirdif[,2] ) * delta_t / 24 # Values in W m^-2
    
  }
  
  return(Iglobal)
}


# This function computes 365 daily radiation grids and stores them on disk.
# It calls the previous function.
# delta_t: time-step for the calculation, in hours.
func_compute_all_daily_pisr <- function(dem,
                                        year_cur,
                                        delta_t,
                                        outpath_base) {
  
  dir.create(file.path(outpath_base, "radiation"), showWarnings = FALSE)
  
  # Setup useful DEM variables.
  dem_mat <- as.matrix(dem, wide = TRUE)
  norm_mat <- cgrad(dem_mat, xres(dem), yres(dem))
  
  dem_res <- xres(dem)
  dem_crs <- crs(dem)
  dem_ext <- ext(dem)
  
  # Get lat/lon extent, to compute midpoint lat/lon.
  xmid = (dem_ext[1] + dem_ext[2]) / 2
  ymid = (dem_ext[3] + dem_ext[4]) / 2
  lonlat <- terra::project(cbind(xmid, ymid), from = dem_crs, to = "EPSG:4326")
  lon <- lonlat[,1]
  lat <- lonlat[,2]
  
  # Reference altitude for irradiance calculation: the mean of the DEM.
  ele_ref <- as.numeric(global(dem, mean))
  
  # Irradiance model constants.
  visibility <<- 50   # [km]
  rh         <<- 60    # [%]
  tempK      <<- 280   # [K]
  O3         <<- 0.002 # [cm]
  alphag     <<- 0.5   # [-]
  
  for (doy_cur in 1:365) {
    
    cat("\r", doy_cur, "/", "365...")
    
    # Compute daily radiation.
    rad_cur_mat <- func_compute_day_rad(dem_mat,
                                        norm_mat,
                                        dem_res,
                                        lat,
                                        lon,
                                        ele_ref,
                                        year_cur,
                                        doy_cur,
                                        delta_t)
    
    # Convert matrix to SpatRaster.
    rad_cur_ras <- round(rast(rad_cur_mat, crs = dem_crs))
    ext(rad_cur_ras) <- dem_ext
    NAflag(rad_cur_ras) <- -9999
    
    # Write file to geotiff.
    rad_out_filepath_cur <- file.path(outpath_base, "radiation", paste0("dir", sprintf("%03d", doy_cur), "24.tif"))
    terra::writeRaster(rad_cur_ras, rad_out_filepath_cur, overwrite = TRUE, datatype = "FLT4S")
    file.rename(rad_out_filepath_cur, paste0(file_path_sans_ext(rad_out_filepath_cur), ".tif"))
  }
  
  cat("\n")
}


# This function converts from longitude to UTM zone number.
func_long2utmzonenumber <- function(long) { # long is longitude in decimal degrees.
  return(ceiling((long + 180) / 6))
}


# This function does the entire app processing when the user presses the button.
func_do_processing <- function(dem_filepath,
                               outline_filepath,
                               firn_filepath,
                               debris_filepath,
                               reference_filepath,
                               dem_buffer,
                               cell_size,
                               compute_radiation_bool,
                               outpath_base) {
  
  has_firn      <- !is.na(firn_filepath)
  has_debris    <- !is.na(debris_filepath)
  has_reference <- !is.na(reference_filepath)
  
  #### Load input ####
  # If multiple dems: first merge.
  # Else: just load.
  ndems <- length(dem_filepath)
  if (ndems > 1) {
    cat("You provided more than one DEM, I am merging them before proceeding...")
    dems <- sapply(as.list(dem_filepath), rast)
    dem_l1 <- do.call(terra::merge, dems)
    cat(" Done.\n")
  } else {
    dem_l1 <- rast(dem_filepath)
  }
  
  cat("Reading glacier outline...\n")
  outline_l1 <- st_zm(st_read(outline_filepath, quiet = TRUE))
  if (any(!st_is_valid(outline_l1))) {
    cat("Glacier outline has one or more invalid geometries. I am fixing it automatically, but you should investigate.\n")
    st_geometry(outline_l1) <- lwgeom_make_valid(st_geometry(outline_l1))
  }
  
  if (has_firn) {
    firn_l1 <- st_zm(st_read(firn_filepath, quiet = TRUE))
    if (any(!st_is_valid(firn_l1))) {
      cat("Firn shapefile has one or more invalid geometries. I am fixing it automatically, but you should investigate.\n")
      st_geometry(firn_l1) <- lwgeom_make_valid(st_geometry(firn_l1))
    }
  }
  
  if (has_debris) {
    debris_l1 <- st_zm(st_read(debris_filepath, quiet = TRUE))
    if (any(!st_is_valid(debris_l1))) {
      cat("Debris shapefile has one or more invalid geometries. I am fixing it automatically, but you should investigate.\n")
      st_geometry(debris_l1) <- lwgeom_make_valid(st_geometry(debris_l1))
    }
  }
  
  if (has_reference) reference_l1 <- rast(reference_filepath)
  gc()
  
  
  #### Fix coordinate systems ####
  # First of all, repair any malformed (not-recognized) CRS.
  # We support repairing UTM CRS whose WKT definition includes
  # (in the first line) the zone number and N/S.
  dem_l1 <- func_repair_rast_crs(dem_l1)
  if (is.null(dem_l1)) {
    err_msg <- "Coordinates system of the DEM is not recognized. Please FIX IT MANUALLY and run the program again."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  outline_l1 <- func_repair_vect_crs(outline_l1)
  if (is.null(outline_l1)) {
    err_msg <- "Coordinates system of the outline shapefile is not recognized. Please FIX IT MANUALLY and run the program again."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  if (has_firn) {
    firn_l1 <- func_repair_vect_crs(firn_l1)
    if (is.null(firn_l1)) {
      err_msg <- "Coordinates system of the firn shapefile is not recognized. Please FIX IT MANUALLY and run the program again."
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
  }
  if (has_debris) {
    debris_l1 <- func_repair_vect_crs(debris_l1)
    if (is.null(debris_l1)) {
      err_msg <- "Coordinates system of the debris shapefile is not recognized. Please FIX IT MANUALLY and run the program again."
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
  }
  
  # If reference grid is given: use its CRS.
  # Else check CRS of both DEM and outline.
  # If both have same CRS and it is not 4326: leave as is and proceed.
  # If both have same CRS and it is 4326: project both to UTM, then proceed.
  # If CRS is not the same:
  # Check whether either DEM or shapefile is UTM (allow for a 1-zone tolerance for glaciers spanning UTM zone borders)
  # If yes: project only the other to the same UTM and proceed (preferentially reproject outline as it is faster)
  # If no: project both to UTM, then proceed.
  dem_crs            <- terra::crs(dem_l1, proj = TRUE)
  outline_crs        <- terra::crs(outline_l1, proj = TRUE)
  wgs84_crs          <- terra::crs("EPSG:4326", proj = TRUE)
  
  
  # Flags which are set according to the following logic routine.
  reproj_dem         <- FALSE
  reproj_outline     <- FALSE
  
  cat("\nCoordinate system is checked...\n")
  
  # Find which UTM zone we should be using here in principle.
  # Also works if the outline is in some weird CRS.
  if (outline_crs == wgs84_crs) {
    outline_centroid   <- suppressWarnings(st_coordinates(st_centroid(outline_l1)))
  } else {
    outline_wgs84      <- st_transform(outline_l1, "EPSG:4326")
    outline_centroid   <- suppressWarnings(st_coordinates(st_centroid(outline_wgs84)))
  }
  utm_crs_number       <- func_long2utmzonenumber(outline_centroid[1])
  utm_ns_id            <- 2 - as.integer(outline_centroid[2] > 0) # 1 for North, 2 for South.
  utm_offset           <- c(0,100)[utm_ns_id] # Zones below the Equator start at 32700.
  utm_ns               <- c("N", "S")[utm_ns_id]
  utm_crs              <- terra::crs(paste0("EPSG:", 32600 + utm_crs_number + utm_offset), proj = TRUE)
  
  if (has_reference) {
    cat("Reference grid is available. I am reprojecting as needed...\n")
    
    reference_crs <- crs(reference_l1, proj = TRUE)
    
    # If the reference is a .grid file
    # (e.g. which we have just produced),
    # it has no CRS! So in that case we
    # assume that the grid uses the UTM CRS
    # of our choice.
    if (reference_crs == "") {
      crs(reference_l1) <- utm_crs
    } else {
      reference_l1 <- func_repair_rast_crs(reference_l1)
      if (is.null(reference_l1)) {
        err_msg <- "Coordinates system of the reference grid is not recognized. Please FIX IT MANUALLY and run the program again."
        cat("\n*** ERROR:", err_msg, "***\n")
        return(err_msg)
      }
    }
    
    target_crs    <- reference_crs
    
    if (dem_crs     != reference_crs) reproj_dem     <- TRUE
    if (outline_crs != reference_crs) reproj_outline <- TRUE
    
    # We also want square cells.
    if (abs(xres(dem_l1) - yres(dem_l1)) > 1e-5)        reproj_dem     <- TRUE
    
    # If there is no reference grid supplied for alignment.
  } else {
    
    if ((dem_crs == outline_crs) && (dem_crs != wgs84_crs)) {
      
      cat("DEM and shapefile are already in the same projected coordinates.\n")
      target_crs <- dem_crs
      
      if ((abs(xres(dem_l1) - yres(dem_l1)) > 1e-5)) {
        message("DEM cells are not square! I will have to resample the DEM, but I will keep the same coordinate system.")
        reproj_dem <- TRUE
        target_crs <- dem_crs
      }
      
    } else if ((dem_crs == outline_crs) && (dem_crs == wgs84_crs)) {
      
      message(paste0("DEM and shapefile are both in WGS84 (EPSG:4326). I am reprojecting them to UTM (zone ", utm_crs_number, utm_ns, ") before proceeding."))
      # message("This can take some minutes if the DEM is big.\n")
      
      reproj_dem       <- TRUE
      reproj_outline   <- TRUE
      target_crs       <- utm_crs 
      
    } else if (dem_crs != outline_crs) {
      
      message("DEM and shapefile do not have the same coordinate system.")
      utm_crs_allowed  <- sapply(paste0("EPSG:", 32600 + utm_crs_number + utm_offset + -1:1), function(x) terra::crs(x, proj = TRUE)) # Allow a 1-zone tolerance, for glaciers near the UTM zone borders.
      
      if (dem_crs %in% utm_crs_allowed) { # Reproject outline.
        message("DEM coordinate system is good, I am reprojecting the shapefile.")
        reproj_outline <- TRUE
        target_crs     <- dem_crs
        
        if ((abs(xres(dem_l1) - yres(dem_l1)) > 1e-5)) {
          message("But DEM cells are not square! I will have to also resample the DEM, but I will keep the same coordinate system.")
          reproj_dem <- TRUE
          target_crs <- dem_crs
        }
        
      } else if (outline_crs %in% utm_crs_allowed) { # Reproject DEM.
        
        message("Shapefile coordinate system is good, I am reprojecting the DEM.") # This can take some minutes if the DEM is big.")
        reproj_dem     <- TRUE
        target_crs     <- outline_crs
        
      } else { # Reproject both.
        
        message(paste0("I am reprojecting both DEM and shapefile to UTM (zone ", utm_crs_number, utm_ns, ")."))# This can take some minutes if the DEM is big."))
        reproj_dem     <- TRUE
        reproj_outline <- TRUE
        target_crs     <- utm_crs
        
      } # End of "Reproject both".
    } # End of "DEM and shapefile do not have the same coordinate system".
  } # End of "if (has_reference)".
  
  # Now do the reprojections which we have decided above.
  # To compute the cell size for the DEM reprojection,
  # we need to first reproject the outline
  # so that its extent is in meters, then
  # compute cell size.
  dem_l2             <- dem_l1
  outline_l2         <- outline_l1
  if (reproj_outline) outline_l2  <- st_transform(outline_l1, target_crs)
  
  # If a reference grid is given, just use its
  # resolution as cell size. Else:
  # if cell size is not supplied by the user,
  # determine it automatically from the extent of the outline bbox.
  # We aim for 50000 total (DHM) cells, we allow cell sizes of
  # 10, 20, 50, 100, 200, 500 and 1000 m.
  if (has_reference) {
    cat("Reference grid supplied. Overriding cell size with reference cell size...\n")
    resolution_proj_raster <- xres(reference_l1)
  } else {
    if (is.na(cell_size)) {
      cat("Cell size not supplied. Automatically computing cell size...\n")
      outline_extent <- st_bbox(outline_l2)
      outline_extent_area <- (outline_extent[3] - outline_extent[1]) * (outline_extent[4] - outline_extent[2])
      cellsizes_allowed <- c(10, 20, 50, 100, 200, 500, 1000)
      ncells_target <- 50000
      resolution_proj_raster <- cellsizes_allowed[which.min(abs(((outline_extent_area / (cellsizes_allowed^2)) / ncells_target) - 1))]
      cat("Cell size selected:", resolution_proj_raster, "m\n")
    } else {
      resolution_proj_raster <- cell_size
      cat("Cell size supplied:", resolution_proj_raster, "m\n")
    }
  }
  
  
  
  # Always reproject firn and debris shapefiles. Easier than doing all comparisons of the projection.
  if (has_firn)   firn_l2   <- st_transform(firn_l1, terra::crs(outline_l2, proj = TRUE))
  if (has_debris) debris_l2 <- st_transform(debris_l1, terra::crs(outline_l2, proj = TRUE))
  gc()
  
  
  # Now project / crop DEM.
  # If reference grid not available: generate one based on computed
  # extent, target CRS and resolution, 
  # and use it as template, reprojecting DEM if needed.
  if (!(has_reference)) {
    
    ext_out <- ext(st_bbox(outline_l2) + dem_buffer * c(-1,-1,1,1))
    reference_l1 <- rast(crs = target_crs,
                         resolution = resolution_proj_raster,
                         extent = ext_out)
    has_reference <- TRUE
  }
  
  if (reproj_dem) {
    ref_ext_proj <- project(ext(reference_l1),
                            terra::crs(reference_l1, proj = TRUE),
                            terra::crs(dem_l1, proj = TRUE))
    dem_l2 <- crop(dem_l1, ref_ext_proj, snap = "out")
    dem_l2 <- terra::project(dem_l2, reference_l1, method = "bilinear")
    dhm_out <- dem_l2
  }
  
  # In practice this if will be true only if the
  # previous one (reproj_dem) was false, since
  # terra::project always matches extent and nrow/ncol.
  # In that case, the supplied DEM only has to be resampled
  # but not reprojected.
  if ((nrow(dem_l2)   != nrow(reference_l1))   ||
      (ncol(dem_l2)   != ncol(reference_l1))   ||
      (ext(dem_l2)    != ext(reference_l1))) {
    
    dhm_out <- terra::resample(dem_l2, reference_l1, method = "bilinear")
    
    # If the dem_l1 was already matching the reference exactly,
    # with no reprojection/crop/resample involved,
    # all we have to do is create dhm_out.
  } else {
    dhm_out <- dem_l2
  }
  
  na_cells_n <- length(which(values(is.na(dhm_out))[,1]))
  if (na_cells_n > 0) {
    err_msg <- "There are NA values in the DEM file! Please FIX THEM MANUALLY and run the program again."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  
  
  
  #### Extract grids with buffer ####
  # If we just give the buffer size, we just extract the DHM region.
  # Instead, if we give a reference DEM we may have to resample (bilinear filter) ours, because
  # resolution/origin/extent could be different (even after adjusting projection, which we have done above).
  cat("\nPreparing output...\n")
  
  dem_out <- mask(dhm_out, outline_l2)
  surftype_out <- 4*is.na(dem_out) # This is the base rock/ice mask.
  if (has_firn)   surftype_out <- mask(surftype_out, firn_l2, inverse = TRUE, updatevalue = 1)   # Add firn if we have it.
  if (has_debris) surftype_out <- mask(surftype_out, debris_l2, inverse = TRUE, updatevalue = 5) # Add debris if we have them.
  
  
  #### Write grids to output ####
  NAflag(dhm_out)      <- -9999
  NAflag(surftype_out) <- -9999
  dir.create(file.path(outpath_base, "dhm"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(outpath_base, "surftype"), showWarnings = FALSE)
  dir.create(file.path(outpath_base, "outline"), showWarnings = FALSE)
  writeRaster(dhm_out, file.path(outpath_base, "dhm", "dhm_glacier.tif"), overwrite = TRUE)
  writeRaster(surftype_out, file.path(outpath_base, "surftype", "surface_type_glacier.tif"), overwrite = TRUE)
  st_write(outline_l2, file.path(outpath_base, "outline", "outline_glacier.shp"), append = FALSE, quiet = TRUE)
  
  #### Compute radiation if asked to do so ####
  if (compute_radiation_bool) {
    cat("Computing daily solar radiation. This can take a few minutes...\n")
    func_compute_all_daily_pisr(dhm_out,
                                2020,
                                0.1,
                                outpath_base)
  }
  
  # Errors? Show them!
  if (any(is.na(values(dhm_out)))) {
    err_msg <- "There are NA values in the ouput DEM. Please check that the input DEMs cover the full area of interest. Also check the parameter \"margin size\"."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  if (any(is.na(values(surftype_out)))) {
    err_msg <- "There are NA values in the ouput surface type grid. Please check the input shapefiles."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  
  
  #### Finish messages ####
  cat("Program finished succesfully!\n")
  message("Your new files are located here:")
  cat(normalizePath(file.path(getwd())), "\n")
  cat("Before you run the mass balance model, move them to the right place (input folder).\n")
  message("Now you can close the program.")
  
  return("0")
  
} # End of function definition.


#### Definition of shiny app to use the above function ####
# Define UI for app ----
ui <- fluidPage(useShinyjs(),
                
                # . App title and description ----
                titlePanel("Mass balance model assistant"),
                
                
                checkboxInput("checkbox_show_help_text", "Show help text", FALSE),
                
                # . . Help text which can be shown by ticking a checkbox.
                conditionalPanel(condition = "input.checkbox_show_help_text == 1",
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("This program generates the grids of DEM, surface type and radiation which are used in the glacier mass balance model.")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("As"), strong(" INPUT DATA "), em("please provide:")),
                                 tags$div(tags$ul(
                                   tags$li(em("the ", strong("glacier name "), "with no whitespaces")),
                                   tags$li(em("the ", strong("modeled year,"), "used to set the file names")),
                                   tags$li(em("one or more ", strong("elevation grids "), "of the region of interest (for example .tif or .hgt, from EarthExplorer, SRTM, ASTER or any other). If you provide ", strong("more than one grid,"), "all grids", strong("will be merged"), "(mosaic) before processing.")),
                                   tags$li(em("a ", strong("glacier outline, "), "for example as shapefile (.shp)")),
                                   tags$li("(OPTIONAL): ", em("a shapefile with the ", strong("firn area"))),
                                   tags$li("(OPTIONAL): ", em("a shapefile with the ", strong("debris cover"))),
                                   tags$li("(OPTIONAL): ", em("a ", strong("reference grid file, to align"), " the output grids (useful to create input for multi-year simulations)")),
                                   tags$li("(OPTIONAL): ", em("the ", strong("margin distance"), " around the outline, in meters.", strong("This is ignored if you provide the reference grid file."))),
                                   tags$li("(OPTIONAL): ", em("the ", strong("cell size of the grids,"), " in meters. If you don't provide this it is estimated automatically.", strong("This is ignored if you provide the reference grid file."))),
                                   style = "margin-top: 0px; margin-bottom: 5px; text-align: justify;")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("As"), strong(" OUTPUT "), em("the model will create several grids:")),
                                 tags$div(tags$ul(
                                   tags$li(em("a ", strong("DHM"), " (altitude grid, as a full rectangle around the glacier)")),
                                   # tags$li(em("a ", strong("DEM"), " (altitude grid, only where there is ice, with no data outside the glacier)")),
                                   tags$li(em("a grid of ", strong("surface type"), " (rock/ice/firn/debris, important for albedo)")),
                                   tags$li(em("the input", strong("outline shapefile, processed"), "and ready to be used in the mass balance model")),
                                   tags$li("(OPTIONAL): ", em("365 grids of ", strong("daily potential solar radiation."))),
                                   style = "margin-top: 0px; margin-bottom: 5px;")),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 30px; text-align: justify;",
                                    em("The ", strong("coordinate system"), " (UTM / WGS84) is adjusted automatically."))),
                
                # . UI layout below the help text ----
                p(),
                
                # .. Input: choose glacier name (with inline CSS modifier to have label and field on same row),
                # DEM, outline file, and (optionally) firn and debris shapefiles, as well as reference grid for alignment ----
                tags$head(
                  tags$style(type="text/css", "#inline1 label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 10px; } 
                #inline1 .form-group { display: table-row;}"),
                  tags$style(type="text/css", "#inline2 label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 22px; } 
                #inline2 .form-group { display: table-row;}")
                ),
                tags$div(id = "inline1", textInput("choose_glacier_name", "Choose glacier name:", placeholder = "Glacier name")),
                p(),
                tags$div(id = "inline2", numericInput("choose_model_year", "Choose model year:", value = NA, min = 0, max = 3000, step = 1)),
                p(),
                shinyFilesButton("choose_dem_file", strong("Choose one or more input DEM files"), "Choose one or more input DEM files", multiple = TRUE, style = "width: 60%;"),
                p(),
                shinyFilesButton("choose_shp_file", strong("Choose input glacier shapefile"), "Choose input glacier shapefile", FALSE, style = "width: 60%;"),
                p(),
                shinyFilesButton("choose_firn_file", strong("Choose input firn shapefile (optional)"), "Choose input firn shapefile (optional)", FALSE, style = "width: 60%;"),
                p(),
                shinyFilesButton("choose_debris_file", strong("Choose input debris shapefile (optional)"), "Choose input debris shapefile (optional)", FALSE, style = "width: 60%;"),
                p(),
                shinyFilesButton("choose_reference_file", strong("Choose reference grid file (optional)"), "Choose reference grid file (optional)", FALSE, style = "width: 60%; margin-bottom: 20px"),
                
                
                # .. Input: choose margin in meters ----
                numericInput(inputId = "buffersize",
                             label = "Choose margin size around the outline, in meters:",
                             value = 500,
                             min = 1,
                             max = 10000,
                             width = "60%"),
                p(),
                
                # .. Input: choose grid cell size in meters ----
                numericInput(inputId = "cellsize",
                             label = "Choose grid cell size in meters (leave blank for automatic cell size):",
                             value = NA,
                             min = 1,
                             max = 10000,
                             width = "60%"),
                p(),
                
                
                # .. Input: should we also compute radiation files? ----
                checkboxInput("checkbox_compute_radiation", strong("Compute daily potential solar radiation (SLOW!)"), FALSE, width = "100%"),
                p(),
                
                # .. Text fields: show the full path of the chosen DEM and outline file ----
                htmlOutput("glaciername_chosen_string"),
                htmlOutput("modelyear_chosen_string"),
                htmlOutput("dem_chosen_string"),
                htmlOutput("shp_chosen_string"),
                htmlOutput("firn_chosen_string"),
                htmlOutput("debris_chosen_string"),
                htmlOutput("reference_chosen_string"),
                p(),
                
                # .. Input: do-it button ----
                actionButton(inputId = "startprocessing",
                             label = strong("RUN!")),
                p()
                
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  volumes <- c(getVolumes()(), setNames(dirname(getwd()), basename(dirname(getwd()))), setNames(dirname(dirname(getwd())), basename(dirname(dirname(getwd())))))
  shinyFileChoose(input, "choose_dem_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_shp_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_firn_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_debris_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_reference_file", roots=volumes, session=session)
  
  glaciername       <- reactive(input$choose_glacier_name)
  modelyear         <- reactive(input$choose_model_year)
  demfilepath       <- reactive(as.character(parseFilePaths(volumes, input$choose_dem_file)$datapath))
  shpfilepath       <- reactive(as.character(parseFilePaths(volumes, input$choose_shp_file)$datapath))
  firnfilepath      <- reactive(as.character(parseFilePaths(volumes, input$choose_firn_file)$datapath))
  debrisfilepath    <- reactive(as.character(parseFilePaths(volumes, input$choose_debris_file)$datapath))
  referencefilepath <- reactive(as.character(parseFilePaths(volumes, input$choose_reference_file)$datapath))
  
  # Disable "RUN!" button if the required input is missing.
  observe({
    toggleState("startprocessing", isTruthy(glaciername()) && isTruthy(modelyear()) && isTruthy(demfilepath()) && isTruthy(shpfilepath()))
  })
  
  # Show the user which input has been provided and which is still missing.
  output$glaciername_chosen_string <- renderText({
    ifelse(isTruthy(glaciername()),
           "<font color=\"#00C000\"><b>Glacier name selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Glacier name not yet selected.</b></font color>")
  })
  output$modelyear_chosen_string <- renderText({
    ifelse(isTruthy(modelyear()),
           "<font color=\"#00C000\"><b>Model year selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Model year not yet selected.</b></font color>")
  })
  output$dem_chosen_string <- renderText({
    ifelse(isTruthy(demfilepath()),
           "<font color=\"#00C000\"><b>Input DEM file(s) selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Input DEM file(s) not yet selected.</b></font color>")
  })
  output$shp_chosen_string <- renderText({
    ifelse(isTruthy(shpfilepath()),
           "<font color=\"#00C000\"><b>Input outline file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Input outline file not yet selected.</b></font color>")
  })
  output$firn_chosen_string <- renderText({
    ifelse(isTruthy(firnfilepath()),
           "<font color=\"#00C000\"><b>Input firn shapefile selected.</b></font color>",
           "<font color=\"#FF8000\"><b>Input firn shapefile (optional) not yet selected.</b></font color>")
  })
  output$debris_chosen_string <- renderText({
    ifelse(isTruthy(debrisfilepath()),
           "<font color=\"#00C000\"><b>Input debris shapefile selected.</b></font color>",
           "<font color=\"#FF8000\"><b>Input debris shapefile (optional) not yet selected.</b></font color>")
  })
  output$reference_chosen_string <- renderText({
    ifelse(isTruthy(referencefilepath()),
           "<font color=\"#00C000\"><b>Input reference grid file selected. <i>NOTE: margin distance and cell size will be ignored.</i></b></font color>",
           "<font color=\"#FF8000\"><b>Input reference grid file (optional) not yet selected.</b></font color>")
  })
  
  
  # Button to start processing.
  observeEvent(input$startprocessing, {
    
    if (debug_verbose == TRUE) {
      sink("make_input.log",
           split = TRUE)
    }
    
    # These 4 below are probably not needed since the RUN! button is
    # disabled by shinyjs, but we keep them anyway since they make sense.
    req(input$choose_glacier_name)
    req(input$choose_model_year)
    req(input$choose_dem_file)
    req(input$choose_shp_file)
    firnfilepath_sel      <- ifelse(isTruthy(firnfilepath()), firnfilepath(), NA)
    debrisfilepath_sel    <- ifelse(isTruthy(debrisfilepath()), debrisfilepath(), NA)
    referencefilepath_sel <- ifelse(isTruthy(referencefilepath()), referencefilepath(), NA)
    showModal(modalDialog(h3("Processing... See RStudio console for progress."), footer=NULL))
    processing_output <- func_do_processing(demfilepath(), shpfilepath(), firnfilepath_sel, debrisfilepath_sel, referencefilepath_sel, input$buffersize, input$cellsize, input$checkbox_compute_radiation, file.path(glaciername()))
    if (processing_output == "0") {
      file.rename(file.path(getwd(), glaciername(), "dhm", "dhm_glacier.tif"), file.path(getwd(), glaciername(), "dhm", paste0("dhm_", glaciername(), "_", modelyear(), ".tif")))
      file.rename(file.path(getwd(), glaciername(), "surftype", "surface_type_glacier.tif"), file.path(getwd(), glaciername(), "surftype", paste0("surface_type_", glaciername(), "_", modelyear(), ".tif")))
      file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.shp"), file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".shp")))
      file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.shx"), file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".shx")))
      file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.prj"), file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".prj")))
      file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.dbf"), file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".dbf")))
      removeModal()
      showModal(modalDialog(h3("Processing finished -", strong(style="color: #00C000", "SUCCESS!")),
                            h3("Your new files are located here:"),
                            h5(em(normalizePath(file.path(getwd())))),
                            h3("Before you run the mass balance model, move them to the right place (", em("input"), "folder)."),
                            h3("Now you can close this program."), footer = NULL))
      
    } else {
      unlink(file.path(getwd(), glaciername()), recursive = TRUE)
      showModal(modalDialog(h3("Processing ", strong(style="color: #FF0000", "FAILED!"), " Information about the error:"),
                            h4(processing_output),
                            h3("Please CORRECT THE ERROR and run the program again."), footer=NULL))
    }
    if (debug_verbose == TRUE) {
      sink()
    }
  })
  
}
# Run the app.
shinyApp(ui, server)
