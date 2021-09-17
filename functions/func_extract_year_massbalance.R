###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to extract mass balance maps after modeling a year. #
###################################################################################################


func_extract_year_massbalance <- function(year_data,
                                          run_params,
                                          year_cur_params,
                                          data_dhms,
                                          data_dems) {
  
  # We extract three maps of cumulative annual mass balances:
  # (1) "hydro":       hydrological year (1 October <Year-1> - 30 September <Year>)
  # (2) "meas_period": measurement period, defined as (earliest annual stake start - latest annual stake end)
  # (3) "fixed":       user-defined fixed period.
  massbal_annual_maps_data <- func_extract_massbal_maps_annual(year_data,
                                                               run_params,
                                                               year_cur_params,
                                                               data_dhms,
                                                               data_dems)
  
  year_data$massbal_annual_maps            <- massbal_annual_maps_data$massbal_maps
  year_data$massbal_annual_meas_period     <- massbal_annual_maps_data$meas_period
  year_data$massbal_annual_meas_period_ids <- massbal_annual_maps_data$meas_period_ids
  
  # We also extract two winter mass balances:
  # (1) "fixed":       user-defined fixed period.
  # (2) "meas_period": measurement period, defined as (earliest winter stake start - latest winter stake end).
  # If process_winter is FALSE, the list contains only (1).
  massbal_winter_maps_data <- func_extract_massbal_maps_winter(year_data,
                                                               run_params,
                                                               year_cur_params,
                                                               data_dhms,
                                                               data_dems)
  
  year_data$massbal_winter_maps <- massbal_winter_maps_data$massbal_maps
  if (year_data$process_winter) {
    year_data$massbal_winter_meas_period     <- massbal_winter_maps_data$meas_period
    year_data$massbal_winter_meas_period_ids <- massbal_winter_maps_data$meas_period_ids
  } else {
    year_data$massbal_winter_meas_period     <- c(NA, NA)
    year_data$massbal_winter_meas_period_ids <- c(NA, NA)
  }
  
  return(year_data)
  
}
