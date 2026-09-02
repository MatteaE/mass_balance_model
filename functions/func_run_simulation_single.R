###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to call the mass balance model and compare       #
#                 its output to the measured stakes, computing BIAS w.r.t. each stake.            #
#                 The first argument is a list of named multipliers which is passed to this       #
#                 by the optimization routine, to find best parameters to fit the data.           #
###################################################################################################

func_run_simulation_single <- function(year_param_corrections,
                                       run_params, year_cur_params,
                                       dhm_grid_id, dem_grid_id, surftype_grid_id,
                                       data_dhms, data_dems, data_surftype,
                                       snowdist_init, data_radiation, weather_series_cur, dist_topographic_values_red,
                                       dist_probes_norm_values_red, grids_avalanche_cur,
                                       grid_ice_albedo_fact_cur_values, dx1, dx2, dy1, dy2,
                                       nstakes, model_days_n, massbal_meas_cur, stakes_cells,
                                       verbose_logi) {
  
  # verbose_logi can be used to mute the output (used for LOO validation)
  
  
  #### . .  APPLY ADDITIVE CORRECTIONS FOR OPTIMIZATION ####
  # We apply all the available corrections
  # It is up to anyone using this function to
  # pass only the proper corrections when simulating
  # winter / year / year without previous winter optimization.
  corr_available <- names(year_param_corrections)
  year_cur_params_corr <- year_cur_params
  for (corr_cur in corr_available) {
    year_cur_params_corr[[corr_cur]] <- year_cur_params_corr[[corr_cur]] + year_param_corrections[[corr_cur]]
  }
  
  # Compute radiation factor for snow, using the
  # fixed (initial) ratio of the radiation factors.
  year_cur_params_corr$rad_fact_snow <- year_cur_params_corr$rad_fact_ice * year_cur_params_corr$rad_fact_ratio_snow_ice
  
  
  
  if (verbose_logi) {
    cat("melt_factor =",  round(year_cur_params_corr$melt_factor, 3),  "\n")
    cat("rad_fact_ice =", round(year_cur_params_corr$rad_fact_ice, 3), "\n")
    cat("prec_corr =",    round(year_cur_params_corr$prec_corr, 3),    "\n")
  }
  
  #### . .  RUN MASS BALANCE MODEL ####
  mb_model_output <- func_massbal_model(run_params,
                                        year_cur_params_corr,
                                        values(data_dhms$elevation[[dhm_grid_id]])[,1],
                                        data_dems$glacier_cell_ids[[dem_grid_id]],
                                        as.numeric(values(data_surftype$grids[[surftype_grid_id]])),
                                        as.numeric(values(snowdist_init)),
                                        data_radiation,
                                        weather_series_cur,
                                        dist_topographic_values_red,
                                        dist_probes_norm_values_red,
                                        grids_avalanche_cur,
                                        grid_ice_albedo_fact_cur_values,
                                        verbose_level = min(verbose_logi, 1)) # Keep verbosity from received parameter
  
  
  #### . .  COMPARE TO STAKE MEASUREMENTS ####
  # Extract the whole modeled series for all stakes.
  stakes_series_mod_all <- func_extract_modeled_points(run_params,
                                                       dx1, dx2, dy1, dy2,
                                                       mb_model_output$vec_massbal_cumul,
                                                       nstakes,
                                                       model_days_n,
                                                       stakes_cells)
  
  
  # Find indices of the days corresponding to the stake measurements.
  # We match w.r.t. weather_series_cur whose index is off by ~0.5 with the
  # mass balance (mb_model_out$gl_massbal_cumul[1] is the initial condition
  # (i.e. value 0.0) at 00:00 of the first day, then the index of the weather series
  # corresponds to the full following 24 hours, then gl_massbal_cumul[2] is the
  # cumulative mass balance by the end of that same day.
  # So it would be equally correct to also shift all the day indices by one (little to no change).
  stakes_start_ids      <- pmatch(massbal_meas_cur$start_date,
                                  weather_series_cur$timestamp,
                                  duplicates.ok = TRUE)
  stakes_end_ids        <- pmatch(massbal_meas_cur$end_date,
                                  weather_series_cur$timestamp,
                                  duplicates.ok = TRUE)
  
  
  # We also find the start date for stakes set to NA (i.e. start date = date of mass balance minimum).
  stakes_start_ids_corr <- func_compute_unknown_stakes_start_ids(run_params,
                                                                 stakes_start_ids,
                                                                 weather_series_cur,
                                                                 stakes_series_mod_all)
  
  
  # Check duration of stakes observation period now that we have computed it also for NA stakes.
  # Only NA-starting stakes will be seen here, since the ones with a provided start date were
  # already validated within the func_load_massbalance_measurements().
  # It should be greater than the given run_params$stake_duration_min_n (otherwise
  # there could be an error in the end_date).
  ids_duration_bad <- which((stakes_end_ids - stakes_start_ids_corr) < run_params$stake_duration_min_n)
  if (length(ids_duration_bad) > 0) {
    func_customlog("Too short observation period detected for ", length(ids_duration_bad), " of the mass balance measurements with NA starting date.", level = 2)
    func_customlog("        The lower duration limit is set to: stake_duration_min_n = ", run_params$stake_duration_min_n, " days.", level = 0)
    func_customlog("        Please adjust it in set_params or fix the stake end dates in the respective mass balance input file.", level = 0)
    func_customlog("        The first wrong value is: computed start date ", format(weather_series_cur$timestamp[stakes_start_ids_corr[ids_duration_bad[1]]], "%d.%m.%Y"),
                   ", provided end date ", format(massbal_meas_cur$end_date[ids_duration_bad[1]], "%d.%m.%Y"),
                   " (interval of ", stakes_end_ids[ids_duration_bad[1]] - stakes_start_ids_corr[ids_duration_bad[1]],
                   " days, point id ", massbal_meas_cur$id[ids_duration_bad[1]], ")", level = 0)
    func_stop()
  }
  
  
  # Cumulative mass balance of each stake
  # over its individual measurement period (numeric vector).
  stakes_mb_mod  <- as.numeric(stakes_series_mod_all)[((1:nstakes)-1)*(model_days_n+1) + stakes_end_ids] -
    as.numeric(stakes_series_mod_all)[((1:nstakes)-1)*(model_days_n+1) + stakes_start_ids_corr]
  
  # Corresponding measurement.
  stakes_mb_meas <- massbal_meas_cur$massbal
  
  # Bias of each stake (numeric vector, one element per stake).
  stakes_bias <- stakes_mb_mod - stakes_mb_meas
  
  # Global arithmetic mean bias.
  global_bias   <- mean(stakes_bias)
  global_rms    <- sqrt(mean(stakes_bias^2))
  
  # Global area-weighted bias.
  # This is used as target (< 1 mm) for the optimization.
  # If run_params$optim_winter_areaweight_fact is 0.0, then for the winter
  # runs this is the same as the global_bias.
  # If run_params$optim_annual_areaweight_fact is 0.0, then for the annual
  # runs this is the same as the global bias.
  weighted_bias <- mean(stakes_bias * massbal_meas_cur$area_weight)
  # Global area-weighted RMS. Same comments as above for the case of uniform weights.
  weighted_rms <- sqrt(mean(massbal_meas_cur$area_weight * (stakes_bias^2)))
  
  # Print weighted BIAS, BIAS and RMS, with aligned digits ----------------------------------------
  if (verbose_logi) {
    
    print_wstats_logi <- FALSE
    wbias_val_txt     <- ""
    wrms_val_txt      <- ""
    if (any(massbal_meas_cur$area_weight != 1.0)) {
      print_wstats_logi <- TRUE
      wbias_val_txt     <- sprintf("%+.2f", weighted_bias)
      wrms_val_txt      <- sprintf("%.2f", weighted_rms)
    }
    
    bias_val_txt  <- sprintf("%+.2f", global_bias)
    rms_val_txt   <- sprintf("%.2f", global_rms)
    
    nchar_max <- max(nchar(wbias_val_txt), nchar(bias_val_txt), nchar(wrms_val_txt), nchar(rms_val_txt))
    
    wbias_val_txt <- str_pad(wbias_val_txt, width = nchar_max, side = "left")
    bias_val_txt  <- str_pad(bias_val_txt, width = nchar_max, side = "left")
    wrms_val_txt  <- str_pad(wrms_val_txt, width = nchar_max, side = "left")
    rms_val_txt   <- str_pad(rms_val_txt, width = nchar_max, side = "left")
    
    cat(paste0("Global BIAS:        ", bias_val_txt, " mm w.e.\n"))
    cat(paste0("Global RMS:         ", rms_val_txt,  " mm w.e.\n"))
    if (print_wstats_logi) {
      cat(paste0("Area-weighted BIAS: ", wbias_val_txt, " mm w.e.\n"))
      cat(paste0("Area-weighted RMS:  ", wrms_val_txt, " mm w.e.\n"))
    }
  }
  
  # Compile output with everything we may need
  # for either plots or optimization.
  run_output <- list(vec_swe_all           = mb_model_output$vec_swe_all,
                     vec_surftype_all      = mb_model_output$vec_surftype_all,
                     vec_massbal_cumul     = mb_model_output$vec_massbal_cumul,
                     avalanche_net         = mb_model_output$avalanche_net,
                     gl_massbal_cumul      = mb_model_output$gl_massbal_cumul,
                     gl_melt_daily         = mb_model_output$gl_melt_daily,
                     gl_melt_cumul         = mb_model_output$gl_melt_cumul,
                     gl_accum_daily        = mb_model_output$gl_accum_daily,
                     gl_accum_cumul        = mb_model_output$gl_accum_cumul,
                     gl_rainfall_daily     = mb_model_output$gl_rainfall_daily,
                     weather_series        = mb_model_output$weather_series,
                     stakes_start_ids_corr = stakes_start_ids_corr,
                     stakes_end_ids        = stakes_end_ids,
                     stakes_series_mod_all = stakes_series_mod_all,
                     stakes_mb_mod         = stakes_mb_mod,
                     stakes_mb_meas        = stakes_mb_meas,
                     stakes_bias           = stakes_bias,
                     global_bias           = global_bias,
                     global_rms            = global_rms,
                     weighted_bias         = weighted_bias,
                     weighted_rms          = weighted_rms)
  
  return(run_output)
  
}
