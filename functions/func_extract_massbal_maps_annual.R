###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to extract the maps of cumulative mass balance   #
#                 at various dates, for the annual period. We also determine and return the       #
#                 "measurement period".                                                           #                                        #
###################################################################################################

func_extract_massbal_maps_annual <- function(year_data,
                                             run_params,
                                             year_cur_params,
                                             data_dhms,
                                             data_dems) {
  
  # Indices: in the weather series index 1 refers to the whole first day,
  # in the mass balance series index 1 refers to the instant mass balance at the *beginning* of that same first day,
  # index 2 refers to the instant mass balance at the *end* of that same first day.
  # Remember that mass balance vectors have one more element compared to the weather series.
  
  # id of the cumulative mass balance value at the end
  # of the hydrological year (in the mass balance vectors,
  # which include the initial conditions as first element).
  # The "-1)) + 1" is there because the weather series ends
  # on Sep 30 (whose weather values are valid for the whole day),
  # but the hydrological year ends on Oct 1 at 00:00 (the which() would
  # not find anything without the -1).
  id_hydro_start <- which(year_data$weather_series_annual_cur$timestamp == year_cur_params$hydro_start)
  id_hydro_end   <- which(year_data$weather_series_annual_cur$timestamp == (year_cur_params$hydro_end - 1)) + 1
  massbal_hydro_start_values <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_hydro_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_hydro_end_values   <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_hydro_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_hydro_map <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], massbal_hydro_end_values - massbal_hydro_start_values)
  massbal_hydro_map_masked <- mask(massbal_hydro_map, data_dems$elevation[[year_data$dem_grid_id]])
  
  
  # measperiod refers to the period
  # between the earliest annual stake
  # start and the latest annual stake end.
  id_measperiod_start <- min(year_data$mod_output_annual_cur$stakes_start_ids_corr)
  id_measperiod_end   <- max(year_data$mod_output_annual_cur$stakes_end_ids)
  massbal_measperiod_start_values <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_measperiod_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_measperiod_end_values   <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_measperiod_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_measperiod_map <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], massbal_measperiod_end_values - massbal_measperiod_start_values)
  massbal_measperiod_map_masked <- mask(massbal_measperiod_map, data_dems$elevation[[year_data$dem_grid_id]])
  
  id_fixed_start <- which(year_data$weather_series_annual_cur$timestamp == year_cur_params$fixed_annual_start)
  id_fixed_end <- which(year_data$weather_series_annual_cur$timestamp == year_cur_params$fixed_annual_end)
  massbal_fixed_start_values <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_fixed_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_fixed_end_values   <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_fixed_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_fixed_map <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], massbal_fixed_end_values - massbal_fixed_start_values)
  massbal_fixed_map_masked <- mask(massbal_fixed_map, data_dems$elevation[[year_data$dem_grid_id]])
  
  massbal_maps <- list(hydro       = massbal_hydro_map_masked,
                       meas_period = massbal_measperiod_map_masked,
                       fixed       = massbal_fixed_map_masked)
  
  massbal_maps_out <- list(massbal_maps    = massbal_maps,
                           meas_period     = year_data$weather_series_annual_cur$timestamp[c(id_measperiod_start, id_measperiod_end)],
                           meas_period_ids = c(id_measperiod_start, id_measperiod_end))
  
  return(massbal_maps_out)
  
}
