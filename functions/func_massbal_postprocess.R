###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to post-process the simulated mass balance.         #
#                 Specifically, here are performed:                                               #
#                 (1) mass balance correction in elevation bands                                  #
#                 (2) computation of (band-corrected) mass balance over the hydrological year     #
#                 (3) computation of ELA and AAR                                                  #
#                 (4) computation of BIAS and RMS of winter stakes in the annual simulation       #
#                     (if we have them)                                                           #
#                 (5) standardization (to the whole measurement period) of stake measurements     #
#                     (both annual and - if we have them - winter)                                #
#                 (6) extraction of the mass balance and SWE time series at user-defined points   #
#                 (7) calculation of vertical ablation gradients (measured and modeled)           #
###################################################################################################


func_massbal_postprocess <- function(year_data,
                                     run_params,
                                     year_cur_params,
                                     data_dems) {
  
  #### Correct mass balance bias according to user-defined elevation bands ####
  if (year_data$nstakes_annual > 0) {
    year_data$massbal_annual_maps$meas_period_corr <- func_correct_massbal_elebands(year_data,
                                                                                    year_cur_params,
                                                                                    data_dems)
  }
  
  # Compute time series of glacier-wide mean values.
  year_data$massbal_annual_values <- sapply(year_data$massbal_annual_maps, global, fun = "mean", na.rm = TRUE)
  year_data$massbal_winter_values <- sapply(year_data$massbal_winter_maps, global, fun = "mean", na.rm = TRUE)
  
  if (year_data$nstakes_annual > 0) {
    
    # Compute time series of glacier-wide mass balance,
    # including the bias correction in elevation bands.
    # We assign the correction to the melt component,
    # accumulation stays the same.
    mb_band_bias <- year_data$massbal_annual_values[["meas_period.mean"]] - year_data$massbal_annual_values[["meas_period_corr.mean"]]
    mb_band_corr_fact <- (year_data$mod_output_annual_cur$gl_melt_cumul[year_data$massbal_annual_meas_period_ids[2]] - year_data$mod_output_annual_cur$gl_melt_cumul[year_data$massbal_annual_meas_period_ids[1]] + mb_band_bias) / (year_data$mod_output_annual_cur$gl_melt_cumul[year_data$massbal_annual_meas_period_ids[2]] - year_data$mod_output_annual_cur$gl_melt_cumul[year_data$massbal_annual_meas_period_ids[1]])
    year_data$mod_output_annual_cur$gl_melt_cumul_bandcorr <- year_data$mod_output_annual_cur$gl_melt_cumul * mb_band_corr_fact
    year_data$mod_output_annual_cur$gl_massbal_cumul_bandcorr <- year_data$mod_output_annual_cur$gl_accum_cumul - year_data$mod_output_annual_cur$gl_melt_cumul_bandcorr
  }
  
  # COMMENTED OUT: WE NO LONGER CORRECT GLACIER-WIDE MASS BALANCE WITH THE LOCAL BAND CORRECTION
  # AS THIS RE-INTRODUCES SOME GLOBAL BIAS. WE KEEP THE LOCAL BAND CORRECTION ONLY FOR DISTRIBUTED
  # MASS BALANCE MAPS.
  # Apply the correction also to the hydrological year mass balance
  # (selecting the appropriate correction offsets, i.e. at the start
  # and end of the hydrological year: one is summed, the other one
  # is subtracted).
  # id_hydro_start <- which(weather_series_annual_cur$timestamp == year_cur_params$hydro_start)
  # id_hydro_end   <- which(weather_series_annual_cur$timestamp == (year_cur_params$hydro_end - 1)) + 1
  # massbal_corr_series <- mod_output_annual_cur$gl_massbal_cumul_bandcorr - mod_output_annual_cur$gl_massbal_cumul
  # massbal_annual_values[["hydro_corr.mean"]] <- massbal_annual_values[["hydro.mean"]] + massbal_corr_series[id_hydro_end] - massbal_corr_series[id_hydro_start]
  
  
  #### Compute ELA and AAR ####
  year_data$ela_aar <- func_compute_ela_aar(year_data,
                                            run_params,
                                            data_dems)
  
  
  #### Compute daily SCAF in percent ####
  year_data$gl_scaf_daily <- func_compute_scaf(year_data,
                                               run_params,
                                               data_dems)
  
  
  
  #### Compute BIAS and RMS of winter measurements within the annual simulation ####
  # This is used in the vertical scatterplot of winter measurements.
  if (year_data$process_winter > 0) {
    
    # Extract the modeled series of the winter stakes from the annual simulation.
    dxdy_winter       <- year_data$points_dxdy[["winter"]]
    stakes_winter_mod <- func_extract_modeled_points(run_params,
                                                     dxdy_winter[[1]], dxdy_winter[[2]], dxdy_winter[[3]], dxdy_winter[[4]],
                                                     year_data$mod_output_annual_cur$vec_massbal_cumul,
                                                     year_data$nstakes_winter,
                                                     year_data$model_annual_days_n, # Extract whole annual simulation of winter stakes, then we subset.
                                                     year_data$winter_stakes_cells)
    
    
    # Match winter stake dates to annual time series,
    # to extract modeled mass balance at proper timesteps.
    stakes_winter_start_dates          <- year_data$weather_series_winter_cur$timestamp[year_data$mod_output_winter_cur$stakes_start_ids_corr]
    stakes_winter_start_ids_wrt_annual <- match(stakes_winter_start_dates, year_data$weather_series_annual_cur$timestamp)
    stakes_winter_end_dates            <- year_data$weather_series_winter_cur$timestamp[year_data$mod_output_winter_cur$stakes_end_ids]
    stakes_winter_end_ids_wrt_annual   <- match(stakes_winter_end_dates, year_data$weather_series_annual_cur$timestamp)
    
    # For each winter stake, extract:
    # 1) result of annual run over full winter measurement period
    # 2) model bias (over each individual stake period).
    year_data$mod_output_annual_cur$stakes_winter_measperiod_mb <- rep(NA_real_, year_data$nstakes_winter) # Model result for each stake over the full winter measurement period.
    year_data$mod_output_annual_cur$stakes_winter_bias <- rep(NA_real_, year_data$nstakes_winter)
    for(stake_id in 1:year_data$nstakes_winter) {
      
      stake_mod_mb_stake_start      <- stakes_winter_mod[stakes_winter_start_ids_wrt_annual[stake_id], stake_id]
      stake_mod_mb_stake_end        <- stakes_winter_mod[stakes_winter_end_ids_wrt_annual[stake_id], stake_id]
      
      year_data$mod_output_annual_cur$stakes_winter_measperiod_mb[stake_id] <- stakes_winter_mod[year_data$massbal_winter_meas_period_ids[2], stake_id] - stakes_winter_mod[year_data$massbal_winter_meas_period_ids[1], stake_id]
      
      # Compute model bias (over each stake's individual measurement period).
      year_data$mod_output_annual_cur$stakes_winter_bias[stake_id] <- stake_mod_mb_stake_end - stake_mod_mb_stake_start - year_data$massbal_winter_meas_cur$massbal[stake_id]
      
    }
    
    # Now compute global BIAS and RMS of winter stakes.
    year_data$mod_output_annual_cur$global_bias_winter <- mean(year_data$mod_output_annual_cur$stakes_winter_bias)
    year_data$mod_output_annual_cur$global_rms_winter <- sqrt(mean(year_data$mod_output_annual_cur$stakes_winter_bias^2))
    
  }
  
  
  #### Compute standardized stake measurements: ANNUAL ####
  # We correct the mass balance at each stake (using the model) to cover
  # a same standardized period: the longest annual measurement periods
  # (i.e., earliest stake start to latest stake end).
  # If all stakes are measured on the same day, the standardized mass balance
  # is the same as the original one.
  # We also store the model result over the same period, enabling comparison.
  if (year_data$nstakes_annual > 0) {
    year_data$massbal_annual_meas_cur$massbal_meas_standardized <- func_compute_stake_mb_standardized_annual(year_data)
    year_data$massbal_annual_meas_cur$massbal_mod_standardized  <- year_data$mod_output_annual_cur$stakes_series_mod_all[year_data$massbal_annual_meas_period_ids[2],] - year_data$mod_output_annual_cur$stakes_series_mod_all[year_data$massbal_annual_meas_period_ids[1],]
  }
  
  
  #### Compute standardized stake measurements: WINTER ####
  # See comment above for short explanation.
  # NOTE: this call reuses some of the data/ids computed above for
  # the computation of BIAS and RMS of winter measurements.
  if (year_data$process_winter > 0) {
    year_data$massbal_winter_meas_cur$massbal_meas_standardized <- func_compute_stake_mb_standardized_winter(year_data,
                                                                                                             stakes_winter_mod,
                                                                                                             stakes_winter_start_ids_wrt_annual,
                                                                                                             stakes_winter_end_ids_wrt_annual)
  }
  
  
  #### Extract daily data at user-defined points ####
  if (year_data$npoints_daily_out > 0) {
    
    dxdy_daily       <- year_data$points_dxdy[["daily"]]
    year_data$points_daily_massbal_cumul <- func_extract_modeled_points(run_params,
                                                                        dxdy_daily[[1]], dxdy_daily[[2]], dxdy_daily[[3]], dxdy_daily[[4]],
                                                                        year_data$mod_output_annual_cur$vec_massbal_cumul,
                                                                        year_data$npoints_daily_out,
                                                                        year_data$model_annual_days_n,
                                                                        year_data$points_daily_cells)
    
    year_data$points_daily_swe <- func_extract_modeled_points(run_params,
                                                              dxdy_daily[[1]], dxdy_daily[[2]], dxdy_daily[[3]], dxdy_daily[[4]],
                                                              year_data$mod_output_annual_cur$vec_swe_all,
                                                              year_data$npoints_daily_out,
                                                              year_data$model_annual_days_n,
                                                              year_data$points_daily_cells)
    
  }
  
  
  #### Calculate vertical ablation gradients ####
  # Conditions to do this:
  # - there are at least 2 annual stakes which have negative measured-period mass balance in both measurements (standardized) and model
  # - those stakes have at least 50 m vertical elevation span (z_dem)
  # Units are m w.e. / m asl
  year_data$abl_grad_meas <- NA_real_
  year_data$abl_grad_mod  <- NA_real_
  if (year_data$nstakes_annual > 1) {
    
    stakes_cand_ids <- which((year_data$massbal_annual_meas_cur$massbal_meas_standardized <= 0) &
                               year_data$massbal_annual_meas_cur$massbal_mod_standardized <= 0)
    if (length(stakes_cand_ids) > 1) {
      
      if (diff(range(year_data$massbal_annual_meas_cur$z_dem[stakes_cand_ids])) > 50) {
        
        meas_lm <- lm(formula = massbal_meas_standardized~z_dem,
                      data = year_data$massbal_annual_meas_cur)
        mod_lm  <- lm(formula = massbal_mod_standardized~z_dem,
                      data = year_data$massbal_annual_meas_cur)
        
        year_data$abl_grad_meas   <- meas_lm$coefficients["z_dem"]/1e3       # Gradient in m w.e. / m asl
        year_data$abl_grad_meas_i <- meas_lm$coefficients["(Intercept)"]/1e3 # Mass balance at 0 m asl, in m w.e.
        year_data$abl_grad_mod    <- mod_lm$coefficients["z_dem"]/1e3        # Gradient in m w.e. / m asl
        year_data$abl_grad_mod_i  <- mod_lm$coefficients["(Intercept)"]/1e3  # Mass balance at 0 m asl, in m w.e.
        
        year_data$abl_grad_diff_rel <- abs((year_data$abl_grad_mod - year_data$abl_grad_meas) / year_data$abl_grad_meas)
        if (!is.na(year_data$abl_grad_diff_rel) && (year_data$abl_grad_diff_rel > 0.25)) {
          func_customlog("Year ", year_data$year_cur, ": modeled ablation gradient deviates from measured one by more than 25 %. Please check.", level = 1)
          func_customlog("Measured gradient: ", sprintf("%.4f", year_data$abl_grad_meas), " m w.e. / m asl", level = 0)
          func_customlog("Modeled gradient: ", sprintf("%.4f", year_data$abl_grad_mod), " m w.e. / m asl", level = 0)
        }
        
      } else { # End if there is a vertical range of at least 50 m in the selected ablation stakes. Else warning.
        
        func_customlog("Year ", year_data$year_cur, ": point measurements are insufficient to calculate ablation gradients.", level = 1) 
        
      } # End else there is not a 50 m vertical range in the selected ablation stakes
      
    } # End if there are at least 2 stakes with actual ablation (modeled and measured)
  } # End if there are at least 2 annual mass balance values
  
  
  return(year_data)
  
}
