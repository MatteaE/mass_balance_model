###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to load the weather series.                     #
################################################################################################### 

func_load_weather <- function(run_params) {
  
  filepath_weather <- file.path(run_params$dir_data_weather, run_params$filename_weather)
  
  data_raw <- read.table(filepath_weather, header = FALSE, skip = run_params$file_weather_nskip)
  names(data_raw) <- c("year", "doy", "hour", "t2m_mean", "precip")
  
  
  data_raw$timestamp <- as.Date(paste(data_raw$year, data_raw$doy), format = "%Y %j", tz = "UTC")
  data_raw$month <- as.integer(format(data_raw$timestamp, "%m"))
  # Hydrological year always starts 92 days before calendar year.
  data_raw$year_hydro <- as.integer(format(data_raw$timestamp + 92, "%Y"))
  
  data_weather <- data_raw[, c(6, 1, 8, 7, 2, 4, 5)]
  
  return(data_weather)
  
}
