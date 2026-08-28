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
                                data_outlines,
                                data_weather) {
  
  # We set this here so that there is no correction
  # if we don't do the winter optimization.
  year_data$corr_fact_winter      <- 0
  
  # We set this to NULL to have it defined (for the
  # extraction functions) in case we don't do winter processing.
  year_data$mod_output_winter_cur <- NULL
  
  if (year_data$process_winter)  {
    
    # No LOO validation of winter mass balance.
    year_data$run_loo_logi <- FALSE
    
    # Select weather series period.
    year_data$weather_series_winter_cur <- data_weather[which(data_weather$timestamp == year_data$model_time_bounds[3]):(which(data_weather$timestamp == year_data$model_time_bounds[4])),]
    year_data$model_winter_days_n       <- nrow(year_data$weather_series_winter_cur)
    
    
    # Compute weights for the point biases. The function also handles the case where there is a single measurement.
    year_data <- func_compute_massbal_weights(run_params,
                                              "winter",
                                              year_data,
                                              data_dhms,
                                              data_outlines,
                                              compute_loo = FALSE)
    
    
    # In the end, is there any weight which is not 1.0 in the main annual run?
    # If yes, store a logical value which will affect plot appearance (how the bias is written).
    year_data$winter_bias_weighted_logi <- FALSE
    if (any(year_data$massbal_winter_meas_cur$area_weight != 1.0)) {
      year_data$winter_bias_weighted_logi <- TRUE
    }
    
    
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
    
    
    
    # Check if any winter stakes were affected by avalanches --------------------------------------
    # If yes, emit a warning
    if (all(!is.na(run_params$model_avalanche_dates))) {
      
      avalanche_r <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]],
                               year_data$mod_output_winter_cur$avalanche_net)
      avalanche_stakes_net <- terra::extract(avalanche_r,
                                             year_data$massbal_winter_meas_cur[,c("x", "y")],
                                             method = "bilinear",
                                             ID = FALSE,
                                             raw = TRUE)[,1]
      
      year_data$mod_output_winter_cur$avalanche_stakes_net <- avalanche_stakes_net
      
      ids_aval <- which(abs(avalanche_stakes_net) > 0)
      if (length(ids_aval) > 0) {
        
        winter_stakes_avalanche_df <- year_data$massbal_winter_meas_cur[,c("id", "start_date", "end_date", "x", "y", "z_dem", "massbal")]
        winter_stakes_avalanche_df$avalanche_net <- year_data$mod_output_winter_cur$avalanche_stakes_net
        winter_stakes_avalanche_df <- winter_stakes_avalanche_df[ids_aval,]
        
        cat("\n")
        func_customlog("Year ", year_data$year_cur, ": there are ", length(ids_aval), " winter measurement points which are affected by modeled avalanches.", level = 1)
        func_customlog("          Please check carefully their modeled mass balance, it could have unexpected values.", level = 0)
        func_customlog("          Full information:\n", level = 0)
        
        func_print_mb_points_df(winter_stakes_avalanche_df,
                                run_params)
        
      } # End if there are any winter measurements with avalanche effect
      
    } # End if there are any avalanches
    
    # Free some memory after processing.
    invisible(gc())
    
  } # End if process_winter
  
  return(year_data)
  
}
