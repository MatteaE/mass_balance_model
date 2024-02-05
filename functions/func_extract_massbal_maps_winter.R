###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to extract the maps of cumulative mass balance   #
#                 at various dates, for the winter period (still extracted from the annual        #
#                 (i.e. fully optimized) simulation). We also determine and return the            #
#                 "measurement period".                                                           #
################################################################################################### 

func_extract_massbal_maps_winter <- function(year_data,
                                             run_params,
                                             year_cur_params,
                                             data_dhms,
                                             data_dems) {
  
  # Indices: in the weather series index 1 refers to the whole first day,
  # in the mass balance series index 1 refers to the instant mass balance at the *beginning* of that same first day,
  # index 2 refers to the instant mass balance at the *end* of that same first day.
  # Remember that mass balance vectors have one more element compared to the weather series.
  
  
  if (year_data$process_winter) {
    # measperiod refers to the period
    # between the earliest winter stake
    # start and the latest winter stake end.
    # If there are no winter stakes, it is
    # just NULL.
    
    # NOTE: here we extract the winter mass balance (over the winter measurement period) from
    # the fully optimized model run (i.e., we take a winter subset of the annual result).
    # Thus, we have to match the indices of the start/end of the winter measurement period
    # between the winter and annual simulations, in order to extract mass balance from
    # mod_output_annual_cur.
    id_winter_measperiod_start_wrt_winter  <- min(year_data$mod_output_winter_cur$stakes_start_ids_corr)
    id_winter_measperiod_end_wrt_winter    <- max(year_data$mod_output_winter_cur$stakes_end_ids)
    
    winter_meas_dates <- year_data$weather_series_winter_cur$timestamp[c(id_winter_measperiod_start_wrt_winter,
                                                                         id_winter_measperiod_end_wrt_winter)]
    id_winter_measperiod_start_wrt_annual <- match(winter_meas_dates[1], year_data$weather_series_annual_cur$timestamp)
    id_winter_measperiod_end_wrt_annual   <- match(winter_meas_dates[2], year_data$weather_series_annual_cur$timestamp)
    
    
    massbal_measperiod_start_values <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_winter_measperiod_start_wrt_annual - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    massbal_measperiod_end_values   <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_winter_measperiod_end_wrt_annual - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    massbal_measperiod_map          <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], massbal_measperiod_end_values - massbal_measperiod_start_values)
    massbal_measperiod_map_masked   <- mask(massbal_measperiod_map, data_dems$elevation[[year_data$dem_grid_id]])
  }
  
  # Extract fixed winter period from the
  # modeled series of annual mass balance.
  # This is available even if we have no
  # winter mass balance measurements
  # (and so year_data$process_winter is FALSE).
  id_fixed_start             <- which(year_data$weather_series_annual_cur$timestamp == year_cur_params$fixed_winter_start)
  id_fixed_end               <- which(year_data$weather_series_annual_cur$timestamp == year_cur_params$fixed_winter_end)
  massbal_fixed_start_values <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_fixed_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_fixed_end_values   <- year_data$mod_output_annual_cur$vec_massbal_cumul[(id_fixed_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_fixed_map          <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], massbal_fixed_end_values - massbal_fixed_start_values)
  massbal_fixed_map_masked   <- mask(massbal_fixed_map, data_dems$elevation[[year_data$dem_grid_id]])
  
  if (year_data$process_winter) {
    massbal_maps     <- list(fixed       = massbal_fixed_map_masked,
                             meas_period = massbal_measperiod_map_masked)
    
    # NOTE: meas_period_ids has the indices of the winter
    # measurement period w.r.t. the annual simulation.
    massbal_maps_out <- list(massbal_maps    = massbal_maps,
                             meas_period     = year_data$weather_series_winter_cur$timestamp[c(id_winter_measperiod_start_wrt_winter, id_winter_measperiod_end_wrt_winter)],
                             meas_period_ids = c(id_winter_measperiod_start_wrt_annual, id_winter_measperiod_end_wrt_annual))
  } else {
    massbal_maps     <- list(fixed        = massbal_fixed_map_masked)
    massbal_maps_out <- list(massbal_maps = massbal_maps)
  }
  
  return(massbal_maps_out)
  
}
