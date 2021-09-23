###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute sensible values for some              #
#                 altitude parameters (weather_max_precip_ele, elevation_effect_threshold and     #
#                 initial_snowline_elevation) in case the user leaves them to NA.                 #
###################################################################################################  


# We set the three parameters to respectively the 80th,
# 95th and 70th percentile of the first DEM values.
# We set them only if they are unset (i.e. NA).
func_compute_altitude_params <- function(run_params,
                                         data_dems) {
  
  ele_vals <- getValues(data_dems$elevation[[1]])
  ele_quant <- as.numeric(quantile(ele_vals, c(0.8, 0.95, 0.7), na.rm = T))
  
  if (is.na(run_params$weather_max_precip_ele)) {
    run_params$weather_max_precip_ele <- ele_quant[1]
  }
  
  if (is.na(run_params$elevation_effect_threshold)) {
    run_params$elevation_effect_threshold <- ele_quant[2]
  }
  
  if (is.na(run_params$initial_snowline_elevation)) {
    run_params$initial_snowline_elevation <- ele_quant[3]
  }
  
  return(run_params)
  
}