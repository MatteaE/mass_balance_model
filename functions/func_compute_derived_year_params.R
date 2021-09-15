###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to compute the derived year parameters:             #
#                 the ratio of snow/ice radiation factors, and the year modeling periods.         #
#                 This code is called at the end of the year parameters loading.                  #
###################################################################################################

func_compute_derived_year_params <- function(year_data, year_cur_params) {
  
  # Compute ratio of snow to ice radiation factors.
  # We will keep this ratio constant as we optimize
  # the radiation factors.
  year_cur_params$rad_fact_ratio_snow_ice <- year_cur_params$rad_fact_snow / year_cur_params$rad_fact_ice
  
  
  # Compute start and end of current hydrological year.
  # The hydrological year starts on 1/10/<Y-1> at 00:00 and ends on 1/10/<Y> at 00:00.
  # Since we use Date objects which don't include the time of day, we can set the
  # hydro end to October 1 (else we would miss the mass balance
  # between YYYY/9/30 00:00 and YYYY/10/1 00:00).
  year_cur_params$hydro_start <- as.Date(paste(year_data$year_cur-1, 10, 1), format="%Y %m %d")
  year_cur_params$hydro_end   <- as.Date(paste(year_data$year_cur, 10, 1), format = "%Y %m %d")
  
  year_cur_params$fixed_annual_start <- as.Date(paste(year_data$year_cur-1, run_params$massbal_fixed_annual_start), format = "%Y %m/%d")
  year_cur_params$fixed_annual_end   <- as.Date(paste(year_data$year_cur, run_params$massbal_fixed_annual_end), format = "%Y %m/%d")
  
  year_cur_params$fixed_winter_start <- as.Date(paste(year_data$year_cur-1, run_params$massbal_fixed_winter_start), format = "%Y %m/%d")
  year_cur_params$fixed_winter_end   <- as.Date(paste(year_data$year_cur, run_params$massbal_fixed_winter_end), format = "%Y %m/%d")
  
  return(year_cur_params)
  
} 
