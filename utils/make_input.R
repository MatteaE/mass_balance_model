###################################################################################################
# This program takes a shapefile glacier outline and up to additional 2 outlines                  #
# (firn, debris), and produces grids which can be used in the mass balance model:                 #
# DHM, surface type and optionally daily incoming solar radiation.                                #
# The 2 additional shapefiles are optional: in case they are not provided, the output grid        #
# only has ice and rock (no firn and no debris).                                                  #
# Author: Enrico Mattea (University of Fribourg)                                                  #
###################################################################################################

suppressPackageStartupMessages(library(insol2))
suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(tools))

debug_verbose <- TRUE

#### Functions called by the app ####

# This function returns either a vector with suitable geometries
# (a single multipolygon with the outline info),
# or a character error message in case such
# a vector cannot be obtained from the input
# (i.e., empty input, non-fixable input, etc.).
# NOTE: if the provided input is structurally invalid (e.g.,
# a polygon with not enough vertices), terra::is.valid() throws
# an error rather than returning FALSE.
# This function takes care of that.
# vect_type is one of "glacier outline", "firn", and "debris"; used only to print messages/errors.
func_validate_vect <- function(vect_cur,
                               vect_type) {
  
  if (nrow(vect_cur) == 0) {
    return(paste0("The ", vect_type, " shapefile is empty (zero geometries)."))
  }
  
  if (!is.polygons(vect_cur)) {
    return(paste0("The ", vect_type, " shapefile has the wrong type of geometry: '", geomtype(vect_cur), "'. This should be: 'polygons'."))
  }
  
  # This block returns 0 if any geometries need fixing and can be fixed,
  # 1 if all geometries are already valid, and
  # 2 if there are any geometries which are crashing the is.valid check
  # (e.g., structurally invalid polygons with not enough vertices)
  valid_geom <- tryCatch({
    as.integer(all(is.valid(vect_cur)))
  }, error = function(e) {
    structure(2L, err_msg = conditionMessage(e)) # This is a 2 (tests TRUE for ==2) but also exports the error message, to be printed later.
  })
  
  # All valid.
  if (valid_geom == 1) {
    
    return(terra::aggregate(vect_cur))
    
    # Invalid, but repair can be attempted (is.valid and makeValid do not throw error).
  } else if (valid_geom == 0) {
    cat("The", vect_type, "shapefile has one or more invalid geometries. I am trying to fix it automatically, but you should investigate.\n")
    vect_cur_fix <- makeValid(vect_cur)
    
    # Repaired successfully.
    if (all(is.valid(vect_cur_fix))) {
      return(terra::aggregate(vect_cur_fix))
      
      # Failed to repair.
    } else {
      return(paste0("The ", vect_type, " shapefile has one or more invalid geometries that could not be automatically fixed."))
    }
    
  } # End if invalid but repair could be attempted
  
  # If we are here, there are geometries which are crashing is.valid().
  # Then they are unrecoverable.
  return(paste0("There is an error in the ", vect_type, " shapefile: ", attr(valid_geom, "err_msg"), "."))
  
}


# This function checks whether a raster grid is too large.
# The threshold is set at 100 million cells - bad idea to
# run DMBSim with such a grid.
func_check_ncell <- function(rast_cur, ncell_max = 1e8) {
  if (ncell(rast_cur) > ncell_max) {
    return(paste0("The output grids would have ", format(ncell(rast_cur), scientific = FALSE),
                  " cells - this is too many (maximum: ", format(ncell_max/1e6, scientific = FALSE), " million), please check the input data or increase the cell size (current value: ", xres(rast_cur), " m)."))
  }
  return(NULL)
}


# This function is a little worker called by the next function
# to extract and return UTM code from an input line according to a regexp.
func_utm_grep <- function(input_line,
                          regexp_utm) {
  
  utm_matches <- regmatches(input_line, regexec(regexp_utm, input_line, ignore.case = TRUE))
  if (length(utm_matches[[1]]) == 6) {
    utm_match <- utm_matches[[1]][2:length(utm_matches[[1]])]
    utm_zone  <- as.integer(utm_match[4])
    utm_ns    <- utm_match[5]
    utm_code  <- 32600 + utm_zone + c(0,100)[2 - (utm_ns == "N")]
    return(utm_code)
    
  } else {
    return(NA)
  }
  
}

# This function does its best to match a malformed UTM coordinate system
# (i.e. one which is not automatically recognized as a UTM) with the corresponding known one.
# wkt_malformed is the output of terra::crs().
# To find a suitable candidate:
# first look at the first line of the WKT, if it contains:
# "UTM" and <NN>( ){0,2}[N,S] or various combinations thereof,
# then the projection is interpreted as the respective UTM.
# Otherwise, look for a line with "CONVERSION" in it and check for the same elements.
# Else return NA and (eventually) throw an error.
func_recover_utm_crs <- function(wkt_malformed) {
  
  # Is the wkt_malformed empty or NA? Then there is nothing to recover.
  if (!nzchar(wkt_malformed) ||
      is.na(wkt_malformed) ||
      (wkt_malformed == "NA")) {
    return(NA)
  }
  
  # We initialize the output as NA, it will be
  # updated to an actual UTM code if this is possible,
  # otherwise returned as is (signalling error).
  utm_code <- NA
  
  # Remove extra whitespaces, split into lines.
  wkt_malformed_v2 <- gsub("( ){2,}", " ", wkt_malformed)
  wkt_split <- strsplit(wkt_malformed_v2, "\n")[[1]]
  
  
  # This regexp tenaciously creates 5 capture groups:
  # U(...)
  # T(...)
  # M(...)
  # <zone number>
  # N or S
  # However, regmatches() will create 6 outputs (first the full match, then the groups).
  regexp_utm <- "((?:universal)|U){1}[ _-]{0,2}((?:transverse)|T){1}[ _-]{0,2}((?:mercator)|M){1}(?:[^0-9])*([0-9]{1,2})[ _-]*([NS]{1})"
  
  # Test regexp on first line of split WKT (some
  # GIS programs put a malformed UTM string there).
  wkt_line_first <- wkt_split[[1]]
  utm_code <- func_utm_grep(wkt_line_first,
                            regexp_utm)
  
  # If that failed, look for line with CONVERSION
  if (is.na(utm_code)) {
    
    wkt_conversion_id <- grep("CONVERSION", wkt_split)
    if (length(wkt_conversion_id) > 0) {
      wkt_line_conversion <- wkt_split[[wkt_conversion_id[1]]]
      utm_code <- func_utm_grep(wkt_line_conversion,
                                regexp_utm)
    } # End look for CONVERSION line
    
  } # End if utm_code is still NA after looking at the first WKT line
  
  # This is NA unless a suitable UTM code was recovered.
  return(utm_code)
}



# This function checks whether the CRS of a raster is recognized or not.
# If not, it calls a UTM repair function to try to deduce the CRS.
func_repair_rast_crs <- function(rast_cur) {
  if (is.na(terra::crs(rast_cur, describe = T)$code)) {
    dem_crs_tentative_code <- func_recover_utm_crs(terra::crs(rast_cur))
    if (!is.na(dem_crs_tentative_code)) {
      dem_crs_epsg <- paste0("EPSG:", dem_crs_tentative_code)
      terra::crs(rast_cur) <- terra::crs(dem_crs_epsg)
      message("WARNING! Coordinates system of a grid was malformed, but I was able to fix it as ", dem_crs_epsg, ". I will continue.")
      return(rast_cur)
    } else {
      return(NULL)
    }
  } else {
    return(rast_cur)
  }
}

# This function checks whether the CRS of a vector is recognized or not.
# If not, it calls a UTM repair function to try to deduce the CRS.
func_repair_vect_crs <- function(vect_cur) {
  if (is.na(terra::crs(vect_cur, describe = T)$code)) {
    outl_crs_tentative_code <- func_recover_utm_crs(terra::crs(vect_cur))
    if (!is.na(outl_crs_tentative_code)) {
      outl_crs_epsg <- paste0("EPSG:", outl_crs_tentative_code)
      terra::crs(vect_cur) <- terra::crs(outl_crs_epsg)
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
  
  # This is c(sunrise h, sunset h, duration h)
  # if there are sunrise and sunset.
  # If there is no sunrise nor sunset (polar night),
  # this is c(NA, NA, 0).
  # If there is no sunrise nor sunset (midnight sun),
  # this is c(NaN, NaN, NaN).
  dayl <- daylength(lat, lon, jd_cur, 0)
  
  # Always sunny - just run a full day.
  # This is very slightly inaccurate because it does not
  # consider the actual 24 hour window of the appropriate time zone
  # (which could be e.g. -6 to 18), but the difference is tiny and
  # self-limited (radiation sum over several days of full sun will
  # converge towards correct values.
  if (!is.finite(dayl[3])) {
    dayl <- c(0, 24, 24)
  }
  
  # If no sun at all, keep Iglobal at 0.0.
  if (dayl[3] > 0) {
    
    for (hour_cur in seq(dayl[1], dayl[2], delta_t)) {
      
      jd_cur <- JDymd(year_cur, month_cur, day_cur, hour_cur)
      sun_vec <- sunvector(jd_cur, lat, lon, 0)
      hillshade_cur <- hillshading(norm_mat, sun_vec)
      shaded <- doshade(dem_mat, sun_vec, dem_res)
      sun_zenith <- degrees(acos(sun_vec[,3]))
      # Compute direct radiation modified by terrain + diffuse irradiation (sky-view factor ignored).
      Idirdif = insolation(sun_zenith, jd_cur, ele_ref, visibility, rh, tempK, O3, alphag)
      Iglobal = Iglobal + (Idirdif[,1] * hillshade_cur + Idirdif[,2] ) * delta_t / 24 # Values in W m^-2
      
    } # End loop on the timesteps
    
  } # End if day length is > 0
  
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
  dem_crs <- terra::crs(dem)
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


# We re-implement shinyFiles' getVolumes() function, because
# it can fail with Cyrillic names on R >= 4.3.
# We add a call to a new function which tries to
# repair the encoding of the strings.
func_getvolumes <- function(exclude = NULL) {
  osSystem <- Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    volumes <- fs::dir_ls("/Volumes")
    names(volumes) <- basename(volumes)
  }
  else if (osSystem == "Linux") {
    volumes <- c(Computer = "/")
    if (isTRUE(fs::dir_exists("/media"))) {
      media <- fs::dir_ls("/media")
      names(media) <- basename(media)
      volumes <- c(volumes, media)
    }
  }
  else if (osSystem == "Windows") {
    wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
    if (!file.exists(wmic)) {
      volumes_info <- system2("powershell", "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                              stdout = TRUE)
      num = as.integer(volumes_info[1])
      if (num == 0)
        return(NULL)
      mat <- matrix(volumes_info[-1], nrow = num, ncol = 2)
      mat[, 1] <- gsub(":\\\\$", ":/", mat[, 1])
      sel <- mat[, 2] == ""
      mat[sel, 2] <- mat[sel, 1]
      volumes <- mat[, 1]
      volNames <- mat[, 2]
      volNames <- paste0(volNames, " (", gsub(":/$", ":",
                                              volumes), ")")
    } else {
      volumes  <- system(paste(wmic, "logicaldisk get Caption"),
                         intern = TRUE, ignore.stderr = TRUE)
      volumes  <- sub(" *\\r$", "", volumes)
      keep     <- !tolower(volumes) %in% c("caption", "")
      volumes  <- volumes[keep]
      volNames <- system(paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
                         intern = TRUE, ignore.stderr = TRUE)
      
      for (vn_id in 1:length(volNames)) {
        volNames[vn_id] <- func_process_volname(volNames[vn_id])
      }
      
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "",
                                          "", " "))
      volNames <- paste0(volNames, "(", volumes, ")")
    }
    names(volumes) <- volNames
    volumes <- gsub(":$", ":/", volumes)
  }
  else {
    stop("unsupported OS")
  }
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  volumes
}




# This function tries to repair the encoding of a volName string, as it
# could unexpectedly crash on Cyrillic Windows if just taken from wmic.exe.
func_process_volname <- function(volName) {
  # A helper function to attempt sub() with fallback iconv()
  try_sub <- function(volName) {
    tryCatch({
      # Try to clean up the string with sub() function
      sub(" *\\r$", "", volName)
    }, error = function(e) {
      # If an error occurs, return NULL to signal a failure
      return(NULL)
    })
  }
  
  # Try sub() first
  volName <- try_sub(volName)
  
  # If sub() fails, attempt iconv with different encoding fallbacks
  if (is.null(volName)) {
    encodings <- c("CP866", "windows-1251", "ISO-8859-5", "KOI8-R")  # List of Russian-like encodings to try
    for (enc in encodings) {
      volName <- tryCatch({
        # Attempt the conversion with the current encoding in the list
        iconv(volName, from = enc, to = "UTF-8")
      }, error = function(e) {
        # If the iconv fails, return NULL to indicate failure and move to the next encoding
        return(NULL)
      })
      
      # If iconv succeeds, try to clean up the string after the conversion
      if (!is.null(volName)) {
        volName <- try_sub(volName)
        break  # Stop once we've succeeded
      }
    }
  }
  
  # If we could not reconstruct the string, use a dummy value.
  if (is.null(volName)) {
    volName <- "NAME_UNKNOWN"
  }
  
  # Return the processed volName
  return(volName)
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
  
  # Load input ------------------------------------------------------------------------------------
  # . Load DEM(s) ---------------------------------------------------------------------------------
  # If multiple dems: first merge.
  # Else: just load.
  ndems <- length(dem_filepath)
  if (ndems > 1) {
    cat("You provided more than one DEM, I am merging them before proceeding...")
    
    # Attempt loading, stop with informative message if error.
    loading_result <- tryCatch({
      dems <- lapply(dem_filepath, rast)
      NULL
    }, error = function(e) {
      return(paste0("Error loading the DEM file(s): ", conditionMessage(e), "."))
    })
    if (!is.null(loading_result)) {
      cat("\n*** ERROR:", loading_result, "***\n")
      return(loading_result)
    }
    
    # Require same CRS for all provided DEMs.
    dems_crs      <- sapply(dems, terra::crs)
    crs_same_logi <- sapply(dems_crs, same.crs, dems_crs[1])
    if (any(!crs_same_logi)) {
      err_msg <- paste0("The DEM files must all have the same coordinates system.")
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
    
    # Attempt merging, stop with informative message if error.
    merging_result <- tryCatch({
      # Construct call so that it works with any number of DEMs.
      dem_l1 <- do.call(terra::merge, args = c(dems, list(algo = 1, resample = TRUE, method = "bilinear")))
      NULL
    }, error = function(e) {
      return(paste0("Error merging the DEM file(s): ", conditionMessage(e), "."))
    })
    if (!is.null(merging_result)) {
      cat("\n*** ERROR:", merging_result, "***\n")
      return(merging_result)
    }
    
    cat(" Done.\n")
    
    # Else there is a single DEM.
  } else {
    
    loading_result <- tryCatch({
      dem_l1 <- rast(dem_filepath)
      NULL
    }, error = function(e) {
      return(paste0("Error loading the DEM file: ", conditionMessage(e), "."))
    })
    if (!is.null(loading_result)) {
      cat("\n*** ERROR:", loading_result, "***\n")
      return(loading_result)
    }
    
  } # End else there is a single DEM.
  
  
  # . Load glacier outline ------------------------------------------------------------------------
  cat("Reading glacier outline...\n")
  outl_result <- tryCatch({
    outline_l1 <- vect(outline_filepath)
    NULL
  }, error = function(e) {
    return(paste0("Error loading the outline shapefile: ", conditionMessage(e), "."))
  })
  if (!is.null(outl_result)) {
    cat("\n*** ERROR:", outl_result, "***\n")
    return(outl_result)
  }
  
  
  # Validate glacier outline.
  outline_l1 <- func_validate_vect(outline_l1,
                                   "glacier outline")
  
  if (class(outline_l1) == "character") {
    cat("\n*** ERROR:", outline_l1, "***\n")
    return(outline_l1)
  }
  
  
  # . Load firn shapefile -------------------------------------------------------------------------
  if (has_firn) {
    firn_result <- tryCatch({
      firn_l1 <- vect(firn_filepath)
      NULL
    }, error = function(e) {
      return(paste0("Error loading the firn shapefile: ", conditionMessage(e), "."))
    })
    if (!is.null(firn_result)) {
      cat("\n*** ERROR:", firn_result, "***\n")
      return(firn_result)
    }
    
    # Validate firn shapefile.
    firn_l1 <- func_validate_vect(firn_l1,
                                  "firn")
    
    if (class(firn_l1) == "character") {
      cat("\n*** ERROR:", firn_l1, "***\n")
      return(firn_l1)
    }
    
  } # End if has firn
  
  
  # . Load debris shapefile -----------------------------------------------------------------------
  if (has_debris) {
    
    
    debris_result <- tryCatch({
      debris_l1 <- vect(debris_filepath)
      NULL
    }, error = function(e) {
      return(paste0("Error loading the debris shapefile: ", conditionMessage(e), "."))
    })
    if (!is.null(debris_result)) {
      cat("\n*** ERROR:", debris_result, "***\n")
      return(debris_result)
    }
    
    # Validate debris shapefile.
    debris_l1 <- func_validate_vect(debris_l1,
                                    "debris")
    
    if (class(debris_l1) == "character") {
      cat("\n*** ERROR:", debris_l1, "***\n")
      return(debris_l1)
    }
    
  } # End if has debris
  
  
  # . Load reference grid -------------------------------------------------------------------------
  if (has_reference) {
    
    reference_result <- tryCatch({
      reference_l1 <- rast(reference_filepath)
      NULL
    }, error = function(e) {
      return(paste0("Error loading the reference grid file: ", conditionMessage(e), "."))
    })
    if (!is.null(reference_result)) {
      cat("\n*** ERROR:", reference_result, "***\n")
      return(reference_result)
    }
    
  }
  
  gc()
  
  
  # Fix coordinate systems ------------------------------------------------------------------------
  # . First of all repair any malformed CRS -------------------------------------------------------
  # We support repairing UTM CRS whose WKT definition includes
  # (in the first line) the zone number and N/S.
  dem_l1 <- func_repair_rast_crs(dem_l1)
  if (is.null(dem_l1)) {
    err_msg <- "Coordinates system of the DEM is not recognized."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  outline_l1 <- func_repair_vect_crs(outline_l1)
  if (is.null(outline_l1)) {
    err_msg <- "Coordinates system of the outline shapefile is not recognized."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  if (has_firn) {
    firn_l1 <- func_repair_vect_crs(firn_l1)
    if (is.null(firn_l1)) {
      err_msg <- "Coordinates system of the firn shapefile is not recognized."
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
  }
  if (has_debris) {
    debris_l1 <- func_repair_vect_crs(debris_l1)
    if (is.null(debris_l1)) {
      err_msg <- "Coordinates system of the debris shapefile is not recognized."
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
  }
  
  # . Now run the logic to decide output CRS ------------------------------------------------------
  # If reference grid is given: use its CRS.
  # Else check CRS of both DEM and outline.
  # If both have same CRS and it is not 4326: leave as is and proceed - it is supported! (e.g., EPSG:3413 or EPSG:2056)
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
  outline_centroid     <- suppressWarnings(crds(terra::project(terra::centroids(outline_l1), "EPSG:4326")))
  utm_crs_number       <- func_long2utmzonenumber(outline_centroid[1])
  utm_ns_id            <- 2 - as.integer(outline_centroid[2] > 0) # 1 for North, 2 for South.
  utm_offset           <- c(0,100)[utm_ns_id] # Zones below the Equator start at 32700.
  utm_ns               <- c("N", "S")[utm_ns_id]
  utm_crs              <- terra::crs(paste0("EPSG:", 32600 + utm_crs_number + utm_offset), proj = TRUE)
  
  if (has_reference) {
    cat("Reference grid is available. I am reprojecting as needed...\n")
    
    reference_crs <- terra::crs(reference_l1, proj = TRUE)
    
    # If the reference is a .grid file
    # (e.g. which we have just produced),
    # it has no CRS! So in that case we
    # assume that the grid uses the UTM CRS
    # of our choice.
    if (reference_crs == "") {
      terra::crs(reference_l1) <- utm_crs
    } else {
      if (is.lonlat(reference_l1,
                    perhaps = TRUE)) {
        err_msg <- "The provided reference grid uses a longitude/latitude coordinate system. This is not supported, the grid should use projected metric coordinates."
        cat("\n*** ERROR:", err_msg, "***\n")
        return(err_msg)
      }
      
      reference_l1 <- func_repair_rast_crs(reference_l1)
      if (is.null(reference_l1)) {
        err_msg <- "The coordinate system of the provided reference grid is not recognized."
        cat("\n*** ERROR:", err_msg, "***\n")
        return(err_msg)
      }
    }
    
    if (abs(xres(reference_l1) - yres(reference_l1)) > 1e-5) {
      err_msg <- "The provided reference grid has non-square cells. This is not supported."
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
    
    target_crs    <- reference_crs
    
    if (!(same.crs(dem_crs, reference_crs))) reproj_dem         <- TRUE
    if (!(same.crs(outline_crs, reference_crs))) reproj_outline <- TRUE
    
    
    # We also want square cells.
    if (abs(xres(dem_l1) - yres(dem_l1)) > 1e-5) reproj_dem     <- TRUE
    
    # Else: there is no reference grid supplied for alignment.
  } else {
    
    if (same.crs(dem_crs, outline_crs) && !same.crs(dem_crs, wgs84_crs)) {
      
      cat("DEM and shapefile are already in the same projected coordinates.\n")
      target_crs <- dem_crs
      
      if ((abs(xres(dem_l1) - yres(dem_l1)) > 1e-5)) {
        message("DEM cells are not square! I will have to resample the DEM, but I will keep the same coordinate system.")
        reproj_dem <- TRUE
        target_crs <- dem_crs
      }
      
    } else if (same.crs(dem_crs, outline_crs) && same.crs(dem_crs, wgs84_crs)) {
      
      message(paste0("DEM and shapefile are both in WGS84 (EPSG:4326). I am reprojecting them to UTM (zone ", utm_crs_number, utm_ns, ") before proceeding."))
      # message("This can take some minutes if the DEM is big.\n")
      
      reproj_dem       <- TRUE
      reproj_outline   <- TRUE
      target_crs       <- utm_crs 
      
    } else if (!same.crs(dem_crs, outline_crs)) {
      
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
  
  # . Now do the reprojections decided above ------------------------------------------------------
  # To compute the cell size for the DEM reprojection,
  # we need to first reproject the outline
  # so that its extent is in meters, then
  # compute cell size.
  dem_l2                          <- dem_l1
  outline_l2                      <- outline_l1
  if (reproj_outline) outline_l2  <- terra::project(outline_l1, target_crs)
  
  
  # If a reference grid was supplied, check that it has enough distance
  # from the glacier margin. DMBSim requires at least a one-cell glacier-free
  # margin on all sides.
  # First check that the extent of the outline is fully within the extent of the reference
  # (e.g. if the reference only covers the top part of a glacier broken in two).
  # Then check if the glacier touches the border of the reference, stop with error now.
  if (has_reference) {
    
    # Check if the provided reference is by mistake too big (> 100 million cells).
    # Then we directly skip the outline checks.
    ncell_err <- func_check_ncell(reference_l1)
    if (!is.null(ncell_err)) {
      cat("\n*** ERROR:", ncell_err, "***\n")
      return(ncell_err)
    }
    
    if (!(relate(ext(outline_l2), ext(reference_l1), "within")[1,1])) {
      err_msg <- paste0("The glacier outline is not fully contained in the provided reference grid. Please check the outline or enlarge the extent of the reference.")
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
    
    # Now check the border.
    ref_gl <- terra::mask(setValues(reference_l1, 0), outline_l2, updatevalue = 1, inverse = TRUE)
    val_border <- ref_gl[c(1:ncol(ref_gl),
                           ncell(ref_gl) - ncol(ref_gl) + 1:ncol(ref_gl),
                           seq(1,ncell(ref_gl),ncol(ref_gl)),
                           seq(ncol(ref_gl),ncell(ref_gl),ncol(ref_gl)))][,1]
    if (any(val_border == 1)) {
      err_msg <- paste0("The glacier outline touches the border of the provided reference grid. Please enlarge the extent of the reference.")
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
  }
  
  
  # If a reference grid is given, just use its
  # resolution as cell size. Else:
  # if cell size is not supplied by the user,
  # determine it automatically from the extent of the outline bbox.
  # We aim for 50000 total (DHM) cells, we allow cell sizes of
  # 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, and 10000 m.
  if (has_reference) {
    cat("Reference grid supplied. Overriding cell size with reference cell size...\n")
    resolution_proj_raster <- xres(reference_l1)
  } else {
    if (is.na(cell_size)) {
      cat("Cell size not supplied. Automatically computing cell size...\n")
      outline_extent      <- ext(outline_l2)
      outline_extent_area <- (outline_extent[2] - outline_extent[1]) * (outline_extent[4] - outline_extent[3])
      cellsizes_allowed   <- c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
      ncells_target <- 50000
      resolution_proj_raster <- cellsizes_allowed[which.min(abs(((outline_extent_area / (cellsizes_allowed^2)) / ncells_target) - 1))]
      cat("Cell size selected:", resolution_proj_raster, "m\n")
      
      # Else: cell size was supplied, check it.
    } else {
      
      if (!(is.finite(cell_size) &&
            (cell_size >= 1) &&
            (cell_size <= 10000))) {
        err_msg <- paste0("Invalid value supplied for the cell size. Please use a valid number in meters (1 to 10000) or leave blank for automatic estimation.")
        cat("\n*** ERROR:", err_msg, "***\n")
        return(err_msg)
      }
      
      # Round cell size to millimeters - too many decimals can mess with extents.
      # User-supplied cell size should be integer in general!
      cell_size <- round(cell_size, 3)
      
      resolution_proj_raster <- cell_size
      cat("Cell size supplied:", resolution_proj_raster, "m\n")
    } # End else cell size was supplied
  } # End else has no reference
  
  
  
  # Always reproject firn and debris shapefiles. Easier than doing all comparisons of the projection.
  if (has_firn)   firn_l2   <- terra::project(firn_l1, terra::crs(outline_l2, proj = TRUE))
  if (has_debris) debris_l2 <- terra::project(debris_l1, terra::crs(outline_l2, proj = TRUE))
  gc()
  
  
  # . Check intersection of firn and debris with outline. -----------------------------------------
  # If no intersection, the result will be unexpected, so stop with error.
  if (has_firn) {
    firn_l3 <- terra::intersect(firn_l2, outline_l2)
    if ((nrow(firn_l3) == 0) ||
        (!is.polygons(firn_l3))) {
      err_msg <- paste0("Firn shapefile does not intersect the glacier outline.")
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
  }
  if (has_debris) {
    debris_l3 <- terra::intersect(debris_l2, outline_l2)
    if ((nrow(debris_l3) == 0) ||
        (!is.polygons(debris_l3))) {
      err_msg <- paste0("Debris shapefile does not intersect the glacier outline.")
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
  }
  
  
  # Now project / crop the DEM --------------------------------------------------------------------
  # If reference grid not available: generate one based on computed
  # extent, target CRS, buffer and resolution, and use it as template, reprojecting DEM if needed.
  if (!(has_reference)) {
    if (!(is.finite(dem_buffer) &&
          (dem_buffer >= ceiling(resolution_proj_raster*3)) &&
          (dem_buffer <= 100000))) {
      err_msg <- paste0("Invalid value for the margin size around the outline. Please use a valid number in meters: between ", ceiling(resolution_proj_raster*3), " and 100000; recommended here: ", round(resolution_proj_raster*10))
      cat("\n*** ERROR:", err_msg, "***\n")
      return(err_msg)
    }
    
    # Ensure output extent has integer limits and is not rounded with a too
    # tight crop (especially when it is not a multiple of the cell size).
    ext_out      <- ext(c(floor(ext(outline_l2)[c(1,3)]), ceiling(ext(outline_l2)[c(2,4)]))[c(1,3,2,4)]) + ceiling(dem_buffer/resolution_proj_raster)*resolution_proj_raster
    reference_l1 <- rast(crs        = target_crs,
                         resolution = resolution_proj_raster,
                         extent     = ext_out)
    has_reference <- TRUE
  } # End if we had no reference - now we always do.
  
  
  # Check if the calculated reference grid is impossibly large
  # (possible if cell size is very small, e.g. 1 m,
  # and the glacier outline is very big, e.g. 1000 km2).
  # If it was the SUPPLIED reference grid that was too large,
  # this was already caught much earlier.
  ncell_err <- func_check_ncell(reference_l1)
  if (!is.null(ncell_err)) {
    cat("\n*** ERROR:", ncell_err, "***\n")
    return(ncell_err)
  }
  
  
  # Is the DEM to be reprojected? Do it, otherwise, just resample it.
  # First crop it to the projected extent of the output grid on the current
  # grid (with a little buffer), such that the full grid is not reprojected but only the needed region.
  if (reproj_dem) {
    ref_ext_proj <- terra::project(ext(reference_l1) + xres(reference_l1),
                                   terra::crs(reference_l1, proj = TRUE),
                                   terra::crs(dem_l1, proj = TRUE))
    crop_result <- tryCatch({
      dem_l2 <- crop(dem_l1, ref_ext_proj, snap = "out")
      NULL
    }, error = function(e) {
      return(paste0("Error cropping the DEM file(s): ", conditionMessage(e), ". Please check the locations of the input data."))
    })
    if (!is.null(crop_result)) {
      cat("\n*** ERROR:", crop_result, "***\n")
      return(crop_result)
    }
    
    dem_l2 <- terra::project(dem_l2, reference_l1, method = "bilinear")
    dhm_out <- dem_l2
  }
  
  
  # If we have not reprojected the DEM (i.e., it already had
  # a good CRS), we may still have to resample it so that it
  # matches the desired output grid (called reference_l1, be
  # it user-supplied or computed from cell size and buffer).
  # It is done here.
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
  
  # Any NAs at the end? That would be a problem.
  na_cells_n <- length(which(values(is.na(dhm_out))[,1]))
  if (na_cells_n > 0) {
    err_msg <- "There are NA values in the DEM file. Please check that the input covers the full area of interest, and fill any gaps."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  
  
  
  # Produce output grids --------------------------------------------------------------------------
  # If we just give the buffer size, we just extract the DHM region.
  # Instead, if we give a reference DEM we may have to resample (bilinear filter) ours, because
  # resolution/origin/extent could be different (even after adjusting projection, which we have done above).
  cat("\nPreparing output...\n")
  dem_out      <- terra::mask(dhm_out, outline_l2)
  surftype_out <- 4*is.na(dem_out) # This is the base rock/ice mask.
  
  # Add firn if we have it.
  if (has_firn) {
    surftype_out <- terra::mask(surftype_out, firn_l3, inverse = TRUE, updatevalue = 1)
  }
  
  # Add debris if we have them.
  if (has_debris) {
    surftype_out <- terra::mask(surftype_out, debris_l3, inverse = TRUE, updatevalue = 5)
  }
  
  
  #### Write grids to output ####
  NAflag(dhm_out)      <- -9999
  NAflag(surftype_out) <- -9999
  dir.create(file.path(outpath_base, "dhm"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(outpath_base, "surftype"), showWarnings = FALSE)
  dir.create(file.path(outpath_base, "outline"), showWarnings = FALSE)
  writeRaster(dhm_out, file.path(outpath_base, "dhm", "dhm_glacier.tif"), overwrite = TRUE)
  writeRaster(surftype_out, file.path(outpath_base, "surftype", "surface_type_glacier.tif"), overwrite = TRUE)
  writeVector(outline_l2, file.path(outpath_base, "outline", "outline_glacier.shp"), overwrite = TRUE, insert = FALSE)
  
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
    err_msg <- "There are NA values in the output DEM. Please check that the input DEMs cover the full area of interest. Also check the parameter \"margin size\"."
    cat("\n*** ERROR:", err_msg, "***\n")
    return(err_msg)
  }
  if (any(is.na(values(surftype_out)))) {
    err_msg <- "There are NA values in the output surface type grid. Please check the input shapefiles."
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
                                   tags$li("(OPTIONAL): ", em("the ", strong("cell size of the grids,"), " in meters. It will be rounded to three decimal places. If you don't provide this it is estimated automatically.", strong("This is ignored if you provide the reference grid file."))),
                                   style = "margin-top: 0px; margin-bottom: 5px; text-align: justify;")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("As"), strong(" OUTPUT "), em("the model will create several grids:")),
                                 tags$div(tags$ul(
                                   tags$li(em("a ", strong("DHM"), " (altitude grid, as a full rectangle around the glacier)")),
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
                             max = 100000,
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
  
  volumes <- c(func_getvolumes(), setNames(dirname(getwd()), basename(dirname(getwd()))), setNames(dirname(dirname(getwd())), basename(dirname(dirname(getwd())))))
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
      rename_status <- rep(FALSE, 6)
      rename_status[1] <- file.rename(file.path(getwd(), glaciername(), "dhm", "dhm_glacier.tif"),
                                      file.path(getwd(), glaciername(), "dhm", paste0("dhm_", glaciername(), "_", modelyear(), ".tif")))
      rename_status[2] <- file.rename(file.path(getwd(), glaciername(), "surftype", "surface_type_glacier.tif"),
                                      file.path(getwd(), glaciername(), "surftype", paste0("surface_type_", glaciername(), "_", modelyear(), ".tif")))
      rename_status[3] <- file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.shp"),
                                      file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".shp")))
      rename_status[4] <- file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.shx"),
                                      file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".shx")))
      rename_status[5] <- file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.prj"),
                                      file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".prj")))
      rename_status[6] <- file.rename(file.path(getwd(), glaciername(), "outline", "outline_glacier.dbf"),
                                      file.path(getwd(), glaciername(), "outline", paste0("outline_", glaciername(), "_", modelyear(), ".dbf")))
      
      # Check whether file renaming succeeded - might
      # fail if glacier name was malformed (Cyrillic?).
      if (all(rename_status == TRUE)) {
        
        removeModal()
        showModal(modalDialog(h3("Processing finished -", strong(style="color: #00C000", "SUCCESS!")),
                              h3("Your new files are located here:"),
                              h5(em(normalizePath(file.path(getwd())))),
                              h3("Before you run the mass balance model, move them to the right place (", em("input", .noWS = "before"), "folder)."),
                              h3("Now you can ", strong("close this program"), " or ", strong("run it again"), " to generate another input for the mass balance model."),
                              div(style="margin:auto;margin-top:7%;width:20%;", modalButton(strong("Ok"))),
                              footer = NULL))
      } else {
        processing_output <- "Calculations were successful, but there was a failure while writing the final files. Please check the glacier name and the writing permissions."
      }
      
    } # End if processing_output was "0".
    
    # We get here if the processing failed, or if the
    # processing went well but the final file renaming failed.
    if (processing_output != "0") {
      unlink(file.path(getwd(), glaciername()), recursive = TRUE)
      showModal(modalDialog(h3("Processing ", strong(style="color: #FF0000", "FAILED!")),
                            h3("Information about the error:"),
                            h4(processing_output),
                            h3("Please CORRECT THE ERROR and run the program again."),
                            div(style="margin:auto;margin-top:7%;width:20%;", modalButton(strong("Ok, I try again"))),
                            footer=NULL))
    }
    if (debug_verbose == TRUE) {
      sink()
    }
  })
  
}
# Run the app.
shinyApp(ui, server)
