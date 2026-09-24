###################################################################################################
# This file contains function definitions used by make_input_batch.                               #
# The functions are the same as the self-contained make_input file.                               #
# Author: Enrico Mattea (University of Fribourg)                                                  #
###################################################################################################



# Function to validate a vector outline file ------------------------------------------------------
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


# Function to grep UTM code by regular expression -------------------------------------------------
func_utm_grep <- function(input_line,
                          regexp_utm) {
  
  utm_matches <- regmatches(input_line, regexec(regexp_utm, input_line, ignore.case = TRUE))
  if (length(utm_matches[[1]]) == 6) {
    utm_match <- utm_matches[[1]][2:length(utm_matches[[1]])]
    utm_zone  <- as.integer(utm_match[4])
    utm_ns    <- utm_match[5]
    utm_code  <- 32600 + utm_zone + c(0,100)[2 - (toupper(utm_ns) == "N")] # toupper because we could have "n" here.
    return(utm_code)
    
  } else {
    return(NA)
  }
  
}


# Function to recover a malformed UTM CRS ---------------------------------------------------------
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


# Function to repair CRS of a vector --------------------------------------------------------------
# If the CRS is not recognized, it calls a UTM repair function to try to deduce the CRS.
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



# Function to compute gridded PISR for one specific day -------------------------------------------
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
      # shaded_logi   <- doshade(dem_mat, sun_vec, dem_res)                   # This line would use the old version (original Corripio code)
      shaded_logi   <- doshade2(dem_mat, sun_vec, dem_res, max_threads_n = 0) # This line uses the new, fully correct version, with full multi-threading 
      
      sun_zenith <- degrees(acos(sun_vec[,3]))
      # Compute direct radiation modified by terrain + diffuse irradiation (sky view factor is ignored for diffuse irradiation)
      Idirdif = insolation(sun_zenith, jd_cur, ele_ref, visibility, rh, tempK, O3, alphag)
      Iglobal = Iglobal + (Idirdif[,1] * hillshade_cur * shaded_logi + Idirdif[,2] ) * delta_t / 24 # Values in W m^-2
      
    } # End loop on the timesteps
    
  } # End if day length is > 0
  
  return(Iglobal)
}


# Function to compute 365 grids of daily radiation ------------------------------------------------
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
  }
  
  cat("\n")
}
