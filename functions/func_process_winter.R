###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to process the winter optimization.                 #
###################################################################################################


func_process_winter <- function(year_data,
                                run_params,
                                year_cur_params,
                                data_dhms,
                                data_dems,
                                data_surftype,
                                data_radiation,
                                data_weather) {
  
  # We set this here so that there is no correction
  # if we don't do the winter optimization.
  year_data$corr_fact_winter      <- 0
  
  # We set this to NULL to have it defined (for the
  # extraction functions) in case we don't do winter processing.
  year_data$mod_output_winter_cur <- NULL
  
  if (year_data$process_winter)  {
    
    # We ask duplicates = FALSE, else the bilinear filtering in func_extract_modeled_stakes()
    # can fail when a stake is exactly at the same (X and/or Y) coordinate as a cell center.
    # duplicates = FALSE returns four different cells. In case we have a stake exactly
    # aligned with a cell center, unless we are at the lower raster border (which we should
    # always avoid!) the additional cells returned with duplicates = FALSE (cells which would
    # not be part of the actual adjacent cells) have higher index than the "true" adjacent cells.
    year_data$winter_stakes_cells <- rowSort(fourCellsFromXY(data_dhms$elevation[[year_data$dhm_grid_id]], as.matrix(year_data$massbal_winter_meas_cur[,4:5]), duplicates = FALSE))
    
    # Select weather series period.
    year_data$weather_series_winter_cur <- data_weather[which(data_weather$timestamp == year_data$model_time_bounds[3]):(which(data_weather$timestamp == year_data$model_time_bounds[4])),]
    year_data$model_winter_days_n <- nrow(weather_series_winter_cur)
    
    # The NA is for the optimized corr_fact_winter (which we are
    # determining here, so we don't use a previous value: it is ignored).
    optim_res_winter <- func_optimize_mb("winter", NA,
                                         run_params,
                                         year_cur_params,
                                         year_data,
                                         data_dhms, data_dems, data_surftype, data_radiation)
    year_data$mod_output_winter_cur <- optim_res_winter$mod_output_cur
    
    # Save the correction factor, to re-use it during the annual optimization.
    # We divide by the original prec_corr since the corr_fact is relative
    # (it gets multiplied again during optimization, inside func_optim_worker()).
    optim_corr_winter <- optim_res_winter$corrections_best
    year_data$corr_fact_winter <- optim_corr_winter$prec_corr / year_cur_params$prec_corr
    
    # Free some memory after processing.
    invisible(gc())
  }
  
  return(year_data)
  
}
