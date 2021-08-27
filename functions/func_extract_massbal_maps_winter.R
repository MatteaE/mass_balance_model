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

func_extract_massbal_maps_winter <- function(run_params,
                                             year_cur_params,
                                             weather_series_cur,
                                             mod_output_annual_cur,
                                             process_winter,
                                             mod_output_winter_cur,
                                             data_dhms,
                                             dhm_grid_id,
                                             dem_grid_id) {
  
  # Indices: in the weather series index 1 refers to the whole first day,
  # in the mass balance series index 1 refers to the instant mass balance at the *beginning* of that same first day,
  # index 2 refers to the instant mass balance at the *end* of that same first day.
  # Remember that mass balance vectors have one more element compared to the weather series.
  
  
  if (process_winter) {
    # measperiod refers to the period
    # between the earliest winter stake
    # start and the latest winter stake end.
    # If there are no winter stakes, this is
    # just NULL.
    id_measperiod_start <- min(mod_output_winter_cur$stakes_start_ids_corr)
    id_measperiod_end   <- max(mod_output_winter_cur$stakes_end_ids) + 1 # For the "+ 1": see comment above, about indices. We want the mass balance at the *end* of the last day.
    massbal_measperiod_start_values <- mod_output_annual_cur$vec_massbal_cumul[(id_measperiod_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    massbal_measperiod_end_values   <- mod_output_annual_cur$vec_massbal_cumul[(id_measperiod_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
    massbal_measperiod_map <- setValues(data_dhms$elevation[[dhm_grid_id]], massbal_measperiod_end_values - massbal_measperiod_start_values)
    massbal_measperiod_map_masked <- mask(massbal_measperiod_map, data_dems$elevation[[dem_grid_id]])
  }
  
  
  id_fixed_start <- which(weather_series_cur$timestamp == year_cur_params$fixed_winter_start)
  id_fixed_end <- which(weather_series_cur$timestamp == year_cur_params$fixed_winter_end)
  massbal_fixed_start_values <- mod_output_annual_cur$vec_massbal_cumul[(id_fixed_start - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_fixed_end_values   <- mod_output_annual_cur$vec_massbal_cumul[(id_fixed_end - 1) * run_params$grid_ncells + 1:run_params$grid_ncells]
  massbal_fixed_map <- setValues(data_dhms$elevation[[dhm_grid_id]], massbal_fixed_end_values - massbal_fixed_start_values)
  massbal_fixed_map_masked <- mask(massbal_fixed_map, data_dems$elevation[[dem_grid_id]])
  
  if (process_winter) {
    massbal_maps <- list(fixed = massbal_fixed_map_masked,
                         meas_period = massbal_measperiod_map_masked)
    
    massbal_maps_out <- list(massbal_maps = massbal_maps,
                             meas_period  = weather_series_cur$timestamp[c(id_measperiod_start, id_measperiod_end)])
  
  } else {
    massbal_maps <- list(fixed = massbal_fixed_map_masked)
    massbal_maps_out <- list(massbal_maps = massbal_maps)
  }
  
  return(massbal_maps_out)
  
}
