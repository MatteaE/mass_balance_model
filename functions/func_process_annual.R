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
  
  
  # Select weather series period ------------------------------------------------------------------
  # model_time_bounds[1] is the start of the annual run, 
  # model_time_bounds[2] is the end.
  year_data$weather_series_annual_cur <- data_weather[which(data_weather$timestamp == year_data$model_time_bounds[1]):(which(data_weather$timestamp == year_data$model_time_bounds[2])),]
  year_data$model_annual_days_n       <- nrow(year_data$weather_series_annual_cur)
  
  # Set ids of hydrological year bounds within the simulation, used in multiple locations.
  
  # Indices: in the weather series index 1 refers to the whole first day,
  # in the mass balance series index 1 refers to the instant mass balance at the *beginning* of that same first day,
  # index 2 refers to the instant mass balance at the *end* of that same first day.
  # Mass balance vectors have one more element compared to the weather series.
  
  # The "-1)) + 1" is there because the weather series ends
  # on Sep 30 (whose weather values are valid for the whole day),
  # but the hydrological year ends on Oct 1 (as Date object) even
  # though it is at 00:00 - then, the which() would
  # not find anything without the -1).
  year_data$id_hydro_start <- which(year_data$weather_series_annual_cur$timestamp == year_cur_params$hydro_start)
  year_data$id_hydro_end   <- which(year_data$weather_series_annual_cur$timestamp == (year_cur_params$hydro_end - 1)) + 1
  
  
  # Different processing in case we have or not annual mass balance measurements.
  # If we have mass balance data ------------------------------------------------------------------
  year_data$run_loo_logi <- FALSE
  if (year_data$nstakes_annual > 0) {
    
    year_data$run_loo_logi <- run_params$run_loo_validation
    if ((year_data$run_loo_logi == TRUE) && (year_data$nstakes_annual < 2)) {
      func_customlog("Year ", year_data$year_cur, ": there are not enough points to run LOO validation. It will be skipped.", level = 1)
      year_data$run_loo_logi <- FALSE
    }
    
    
    # . Run optimization --------------------------------------------------------------------------    
    # The return value is a list with everything we need.
    optim_res_annual <- func_optimize_mb("annual", year_data$corr_fact_winter,
                                         run_params, year_cur_params,
                                         year_data,
                                         data_dhms, data_dems, data_surftype, data_radiation)
    
    # Store full model output
    year_data$mod_output_annual_cur <- optim_res_annual$mod_output_cur
    
    
    # Store best correction parameters (additive!)
    year_data$optim_corr_annual     <- optim_res_annual$corrections_best
    
    # Store data frames with point smb and biases for all model runs
    # (useful for LOO validation/sensitivity plots).
    # If there is no LOO validation (year_data$run_loo_logi is FALSE)
    # then these are just NULL, i.e. not set in year_data.
    year_data$df_runs_smb           <- optim_res_annual$df_runs_smb
    year_data$df_runs_biases        <- optim_res_annual$df_runs_biases
    
    
    # . Check if any annual stakes were affected by avalanches ------------------------------------
    # If yes, emit a warning
    if (all(!is.na(run_params$model_avalanche_dates))) {
      
      avalanche_r <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]],
                               year_data$mod_output_annual_cur$avalanche_net)
      avalanche_stakes_net <- terra::extract(avalanche_r,
                                             year_data$massbal_annual_meas_cur[,c("x", "y")],
                                             method = "bilinear",
                                             ID = FALSE,
                                             raw = TRUE)[,1]
      
      year_data$mod_output_annual_cur$avalanche_stakes_net <- avalanche_stakes_net
      
      ids_aval <- which(abs(avalanche_stakes_net) > 0)
      if (length(ids_aval) > 0) {
        
        annual_stakes_avalanche_df <- year_data$massbal_annual_meas_cur[,c("id", "start_date", "end_date", "x", "y", "z_dem", "massbal")]
        annual_stakes_avalanche_df$avalanche_net <- year_data$mod_output_annual_cur$avalanche_stakes_net
        annual_stakes_avalanche_df <- annual_stakes_avalanche_df[ids_aval,]
        
        cat("\n")
        func_customlog("Year ", year_data$year_cur, ": there are ", length(ids_aval), " annual measurement points which are affected by modeled avalanches.", level = 1)
        func_customlog("          Please check carefully their modeled mass balance, it could have unexpected values.", level = 0)
        func_customlog("          Full information:\n", level = 0)
        
        func_print_mb_points_df(annual_stakes_avalanche_df,
                                run_params)
        
      } # End if there are any annual measurements with avalanche effect
      
    } # End if there are any avalanches
    
    
    
    # . Run LOO validation ------------------------------------------------------------------------
    if (year_data$run_loo_logi) {
      year_data <- func_loo_validation(run_params, year_cur_params, year_data,
                                       data_dhms, data_dems, data_surftype, data_radiation,
                                       verbose_logi = FALSE)
    }
    
    
    
    # If we don't have mass balance data ----------------------------------------------------------
  } else {
    
    # . Simulate year with a single model run -----------------------------------------------------
    # The run uses unmodified year_cur_params.
    year_data$mod_output_annual_cur <- func_simulate_mb_without_data(run_params, year_cur_params, year_data,
                                                                     data_dhms, data_dems, data_surftype, data_radiation)
    # No corrections are computed.
    year_data$optim_corr_annual <- list(melt_factor  = 0,
                                        rad_fact_ice = 0,
                                        prec_corr    = 0)
  }
  
  
  return(year_data)
  
}
