###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to process the annual optimization.                 #
###################################################################################################

func_process_annual <- function(year_data,
                                run_params,
                                year_cur_params,
                                data_dhms,
                                data_dems,
                                data_surftype,
                                data_radiation,
                                data_weather) {
  
  # Here do the annual processing.
  # Find grid cells corresponding to the annual stakes.
  # We sort them to enable vectorized bilinear filtering.
  # if (year_data$nstakes_annual > 0) {
    # year_data$annual_stakes_cells <- rowSort(fourCellsFromXY(data_dhms$elevation[[year_data$dhm_grid_id]], as.matrix(year_data$massbal_annual_meas_cur[,4:5]), duplicates = FALSE))
  # }
  
  # Select weather series period.
  # model_time_bounds[1] is the start of the annual run, 
  # model_time_bounds[2] is the end.
  year_data$weather_series_annual_cur <- data_weather[which(data_weather$timestamp == year_data$model_time_bounds[1]):(which(data_weather$timestamp == year_data$model_time_bounds[2])),]
  year_data$model_annual_days_n       <- nrow(year_data$weather_series_annual_cur)
  
  
  # Different processing in case we have or not annual mass balance measurements.
  if (year_data$nstakes_annual > 0) {
    # This is a list with both the best model output
    # and the corresponding best corrections.
    optim_res_annual <- func_optimize_mb("annual", year_data$corr_fact_winter,
                                         run_params, year_cur_params,
                                         year_data,
                                         data_dhms, data_dems, data_surftype, data_radiation)
    year_data$mod_output_annual_cur <- optim_res_annual$mod_output_cur
    
    # Save best correction parameters (additive!).
    year_data$optim_corr_annual <- optim_res_annual$corrections_best
    
  # If we don't have mass balance data:
  } else {
    
    # Simulate year with a single model run,
    # using unmodified year_cur_params.
    year_data$mod_output_annual_cur <- func_simulate_mb_without_data(run_params, year_cur_params, year_data,
                                                                     data_dhms, data_dems, data_surftype, data_radiation)
    # No corrections are computed.
    year_data$optim_corr_annual <- list(melt_factor  = 0,
                                        rad_fact_ice = 0,
                                        prec_corr    = 0)
  }
  
  # Free some memory after processing? Probably useless here.
  # invisible(gc())
  
  return(year_data)
  
}
