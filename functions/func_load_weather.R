###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to load the weather series.                     #
################################################################################################### 

func_load_weather <- function(run_params) {
  
  cat("  Loading daily weather...\n")
  
  filepath_weather <- file.path(normalizePath(run_params$dir_data_weather),
                                run_params$filename_weather)
  
  if (!file.exists(filepath_weather)) {
    func_customlog("File does not exist: ", filepath_weather, level = 2)
    func_stop()
  }
  
  
  tryCatch({data_raw <- read.table(filepath_weather,
                                   header = FALSE,
                                   skip = run_params$file_weather_nskip,
                                   stringsAsFactors = FALSE)},
           error = function(err) {
             func_customlog("Error reading file with daily weather: ", filepath_weather, level = 2)
             func_stop()
           })
  
  
  weather_cols <- c("year", "doy", "hour", "t2m_mean", "precip")
  if (ncol(data_raw) != 5) {
    func_customlog("File with daily weather does not have five columns. Please fix it: ", filepath_weather, level = 2)
    func_customlog("Expected columns (no titles): ", paste0(weather_cols, collapse = " | "), level = 0)
    func_stop()
  }
  names(data_raw) <- weather_cols
  
  
  t2m_bad_ids <- which(is.na(as.numeric(data_raw$t2m_mean)))
  if (length(t2m_bad_ids) > 0) {
    id_wrong_first <- t2m_bad_ids[1]
    func_customlog("there is a problem with the meteo data. ", length(t2m_bad_ids), " temperature value(s) are wrong. Please fix them and run the model again.\n The first bad value is:", level = 2)
    func_customlog(paste(data_raw[id_wrong_first,], collapse = " "))
    func_stop()
  }
  precip_bad_ids <- which(is.na(as.numeric(data_raw$precip)))
  if (length(precip_bad_ids) > 0) {
    id_wrong_first <- precip_bad_ids[1]
    func_customlog("there is a problem with the meteo data. ", length(precip_bad_ids), " precipitation values are wrong. Please fix them and run the model again.\n The first bad value is:", level = 2)
    func_customlog(paste(data_raw[id_wrong_first,], collapse = " "))
    func_stop()
  }
  
  # Sometimes we may have negative precipitation artifacts, remove them.
  data_raw$precip[which(data_raw$precip < 0.0)] <- 0.0
  data_raw$timestamp <- as.Date(paste(data_raw$year, data_raw$doy), format = "%Y %j", tz = "UTC")
  daydiff            <- as.numeric(diff(data_raw$timestamp))
  daydiff_unique     <- unique(daydiff)
  if ((length(daydiff_unique) != 1) || (daydiff_unique[1] != 1)) {
    offending_id1 <- which((is.na(daydiff)) | (daydiff != 1))[1]
    func_customlog("the meteo data do not follow a daily sequence. Please correct the meteo file! The first offending date is:\n", format(data_raw$timestamp[offending_id1], "year = %Y, day of year = %j"), level = 2)
    func_stop()
  }
  
  data_raw$month <- as.integer(format(data_raw$timestamp, "%m"))
  # Hydrological year always starts 92 days before calendar year.
  data_raw$year_hydro <- as.integer(format(data_raw$timestamp + 92, "%Y"))
  
  data_weather <- data_raw[, c(6, 1, 8, 7, 2, 4, 5)]
  
  return(data_weather)
  
}
