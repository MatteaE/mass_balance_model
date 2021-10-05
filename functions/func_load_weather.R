###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to load the weather series.                     #
################################################################################################### 

func_load_weather <- function(run_params) {
  
  cat("  Loading weather...\n")
  
  filepath_weather <- file.path(run_params$dir_data_weather, run_params$filename_weather)
  
  data_raw <- read.table(filepath_weather, header = FALSE, skip = run_params$file_weather_nskip, stringsAsFactors = FALSE)
  names(data_raw) <- c("year", "doy", "hour", "t2m_mean", "precip")
  
  if (typeof(data_raw$t2m_mean) == "character") {
    t2m_mean_numeric <- as.numeric(data_raw$t2m_mean)
    id_wrong_first <- which(is.na(t2m_mean_numeric))[1]
    cat("* WARNING: there is a problem in the meteo data. One or more temperature values are wrong. Please fix them and run the model again.\n The first bad value is:\n")
    cat(paste(data_raw[id_wrong_first,], collapse = " "))
    stop()
    # data_raw$t2m_mean <- as.numeric(interpNA(timeSeries(t2m_mean_numeric), method = "linear"))
  }
  if (typeof(data_raw$precip) == "character") {
    precip_mean_numeric <- as.numeric(data_raw$precip)
    id_wrong_first <- which(is.na(precip_mean_numeric))[1]
    cat("* WARNING: there is a problem in the meteo data. One or more precipitation values are wrong. Please fix them and run the model again.\n The first bad value is:\n")
    cat(paste(data_raw[id_wrong_first,], collapse = " "))
    stop()
    # data_raw$t2m_mean <- as.numeric(interpNA(timeSeries(t2m_mean_numeric), method = "linear"))
  }
  
  # Sometimes we may have negative precipitation artifacts, remove them.
  data_raw$precip[which(data_raw$precip < 0.0)] <- 0.0
  
  data_raw$timestamp <- as.Date(paste(data_raw$year, data_raw$doy), format = "%Y %j", tz = "UTC")
  data_raw$month <- as.integer(format(data_raw$timestamp, "%m"))
  # Hydrological year always starts 92 days before calendar year.
  data_raw$year_hydro <- as.integer(format(data_raw$timestamp + 92, "%Y"))
  
  data_weather <- data_raw[, c(6, 1, 8, 7, 2, 4, 5)]
  
  return(data_weather)
  
}
