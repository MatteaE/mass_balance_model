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
    
    
    
    # Check if any annual stakes were affected by avalanches --------------------------------------
    # If yes, emit a warning
    if (all(!is.na(run_params$model_avalanche_dates))) {
      
      avalanche_r <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]],
                               year_data$mod_output_annual_cur$avalanche_net)
      avalanche_stakes_net <- terra::extract(avalanche_r,
                                             year_data$massbal_annual_meas_cur[,c("x", "y")],
                                             method = "bilinear",
                                             ID = FALSE,
                                             raw = TRUE)[,1]
      
      year_data$massbal_annual_meas_cur$avalanche_net <- avalanche_stakes_net
      
      ids_aval <- which(abs(avalanche_stakes_net) > 0)
      if (length(ids_aval) > 0) {
        
        annual_stakes_avalanche_df <- year_data$massbal_annual_meas_cur[,c("id", "start_date", "end_date", "x", "y", "z_dem", "massbal", "avalanche_net")]
        annual_stakes_avalanche_df <- annual_stakes_avalanche_df[ids_aval,]
        
        cat("\n")
        func_customlog("Year ", year_data$year_cur, ": there are ", length(ids_aval), " annual measurement points which are affected by modeled avalanches.", level = 1)
        func_customlog("          Please check carefully their modeled mass balance, it could have unexpected values.", level = 0)
        func_customlog("          Full information:\n", level = 0)
        
        func_print_mb_points_df(annual_stakes_avalanche_df,
                                run_params)
        
      } # End if there are any annual measurements with avalanche effect
      
    } # End if there are any avalanches
    
    
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
  
  
  return(year_data)
  
}
