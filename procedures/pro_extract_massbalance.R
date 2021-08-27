###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to extract mass balance maps after modeling a year. #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

# We extract three maps of cumulative annual mass balances:
# (1) "hydro":       hydrological year (1 October <Year-1> - 30 September <Year>)
# (2) "meas_period": measurement period, defined as (earliest annnual stake start - latest annual stake end)
# (3) "fixed":       user-defined fixed period.
massbal_annual_maps_data <- func_extract_massbal_maps_annual(run_params,
                                                             year_cur_params,
                                                             weather_series_annual_cur,
                                                             mod_output_annual_cur,
                                                             data_dhms,
                                                             dhm_grid_id,
                                                             dem_grid_id)
massbal_annual_maps <- massbal_annual_maps_data$massbal_maps
massbal_annual_meas_period <- massbal_annual_maps_data$meas_period

# We also extract two winter mass balances:
# (1) "fixed":       user-defined fixed period.
# (2) "meas_period": measurement period, defined as (earliest winter stake start - latest winter stake end).
# If process_winter is FALSE, the list contains only (1).
massbal_winter_maps_data <- func_extract_massbal_maps_winter(run_params,
                                                             year_cur_params,
                                                             weather_series_annual_cur,
                                                             mod_output_annual_cur,
                                                             process_winter,
                                                             mod_output_winter_cur,
                                                             data_dhms,
                                                             dhm_grid_id,
                                                             dem_grid_id)
massbal_winter_maps <- massbal_winter_maps_data$massbal_maps
if (process_winter) {
  massbal_winter_meas_period <- massbal_winter_maps_data$meas_period
} else {
  massbal_winter_meas_period <- NA
}

