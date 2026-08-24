###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to extract the maps of cumulative mass balance   #
#                 and SWE at various dates, for the annual period.                                #
#                 We also determine and return the "measurement period".                          #
###################################################################################################

func_extract_model_maps_annual <- function(year_data,
                                           run_params,
                                           year_cur_params,
                                           data_dhms,
                                           data_dems) {
  

  # Cumulative mass balance values at the start of the first day of the current hydrological year.
  massbal_hydro_start_values <- year_data$mod_output_annual_cur$vec_massbal_cumul[(year_data$id_hydro_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  
  # Cumulative mass balance values at the end of the last day of the current hydrological year (technically, at the start of the next day)
  massbal_hydro_end_values   <- year_data$mod_output_annual_cur$vec_massbal_cumul[(year_data$id_hydro_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  
  massbal_hydro_map          <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], massbal_hydro_end_values - massbal_hydro_start_values)
  massbal_hydro_map_masked   <- mask(massbal_hydro_map, data_dems$elevation[[year_data$dem_grid_id]])
  
  # SWE values at the start of the first day of the current hydrological year.
  swe_hydro_start_values <- year_data$mod_output_annual_cur$vec_swe_all[(year_data$id_hydro_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  
  # SWE values at the end of the last day of the current hydrological year (technically, at the start of the next day)
  swe_hydro_end_values   <- year_data$mod_output_annual_cur$vec_swe_all[(year_data$id_hydro_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  swe_hydro_start_map    <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], swe_hydro_start_values)
  swe_hydro_end_map      <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], swe_hydro_end_values)
  
  
  
  # measperiod refers to the period
  # between the earliest annual stake
  # start and the latest annual stake end.
  if (year_data$nstakes_annual > 0) {
    id_measperiod_start <- min(year_data$mod_output_annual_cur$stakes_start_ids_corr)
    id_measperiod_end   <- max(year_data$mod_output_annual_cur$stakes_end_ids)
    
    massbal_measperiod_start_values <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_measperiod_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    massbal_measperiod_end_values   <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_measperiod_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    massbal_measperiod_map          <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], massbal_measperiod_end_values - massbal_measperiod_start_values)
    massbal_measperiod_map_masked   <- mask(massbal_measperiod_map, data_dems$elevation[[year_data$dem_grid_id]])
    
    swe_measperiod_start_values <- year_data$mod_output_annual_cur$vec_swe_all[(id_measperiod_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    swe_measperiod_end_values   <- year_data$mod_output_annual_cur$vec_swe_all[(id_measperiod_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    swe_measperiod_start_map    <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], swe_measperiod_start_values)
    swe_measperiod_end_map      <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], swe_measperiod_end_values)
  }
  
  # Add maps relative to measurement period if available.
  # We can't use ifelse() with rasters!
  massbal_maps <- list(hydro       = massbal_hydro_map_masked)
  swe_maps     <- list(hydro_start = swe_hydro_start_map,
                       hydro_end   = swe_hydro_end_map)
  if (year_data$nstakes_annual > 0) {
    massbal_maps$meas_period   <- massbal_measperiod_map_masked
    swe_maps$meas_period_start <- swe_measperiod_start_map
    swe_maps$meas_period_end   <- swe_measperiod_end_map
  }
  
  
  # Combine output maps to return.
  # If there is a measurement period, also return its bounds.
  model_maps_out <- list(massbal_maps    = massbal_maps,
                         swe_maps        = swe_maps)
  if (year_data$nstakes_annual > 0) {
    model_maps_out$meas_period     <- year_data$weather_series_annual_cur$timestamp[c(id_measperiod_start, id_measperiod_end)]
    model_maps_out$meas_period_ids <- c(id_measperiod_start, id_measperiod_end)
  } else {
    model_maps_out$meas_period     <- c(NA, NA)
    model_maps_out$meas_period_ids <- c(NA, NA)
  }
  
  return(model_maps_out)
  
}
