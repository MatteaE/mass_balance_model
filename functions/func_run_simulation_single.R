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
                                       nstakes, model_days_n, massbal_meas_cur, stakes_cells) {
  
  
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
  
  cat("melt_factor =",  round(year_cur_params_corr$melt_factor, 3),  "\n")
  cat("rad_fact_ice =", round(year_cur_params_corr$rad_fact_ice, 3), "\n")
  cat("prec_corr =",    round(year_cur_params_corr$prec_corr, 3),    "\n")
  
  #### . .  RUN MASS BALANCE MODEL ####
  mb_model_output <- func_massbal_model(run_params,
                                        year_cur_params_corr,
                                        getValues(data_dhms$elevation[[dhm_grid_id]]),
                                        data_dems$glacier_cell_ids[[dem_grid_id]],
                                        getValues(data_surftype$grids[[surftype_grid_id]]),
                                        getValues(snowdist_init),
                                        data_radiation,
                                        weather_series_cur,
                                        dist_topographic_values_red,
                                        dist_probes_norm_values_red,
                                        grids_avalanche_cur,
                                        grid_ice_albedo_fact_cur_values)
  
  
  #### . .  COMPARE TO STAKE MEASUREMENTS ####
  # Extract the whole modeled series for all stakes.
  stakes_series_mod_all <- func_extract_modeled_stakes(run_params,
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
  # We also find the start date for stakes set to NA (i.e. start date = date of mass balance minimum).
  stakes_start_ids <- pmatch(massbal_meas_cur$start_date,
                             weather_series_cur$timestamp,
                             duplicates.ok = TRUE)
  stakes_end_ids   <- pmatch(massbal_meas_cur$end_date,
                             weather_series_cur$timestamp,
                             duplicates.ok = TRUE)
  stakes_start_ids_corr <- func_compute_unknown_stakes_start_ids(run_params, stakes_start_ids, weather_series_cur, stakes_series_mod_all)
  
  
  # Cumulative mass balance of each stake
  # over its individual measurement period (numeric vector).
  stakes_mb_mod  <- as.numeric(stakes_series_mod_all)[((1:nstakes)-1)*(model_days_n+1) + stakes_end_ids] -
                    as.numeric(stakes_series_mod_all)[((1:nstakes)-1)*(model_days_n+1) + stakes_start_ids_corr]
  
  # Corresponding measurement.
  stakes_mb_meas <- massbal_meas_cur$massbal
  
  # Bias of each stake (numeric vector, one element per stake).
  stakes_bias <- stakes_mb_mod - stakes_mb_meas
  
  global_bias <- mean(stakes_bias)
  global_rms  <- sqrt(mean(stakes_bias^2))
  
  cat("BIAS:", round(global_bias, 2), "mm w.e.\n")
  cat("RMS:",  round(global_rms, 2),  "mm w.e.\n")
  
  # Compile output with everything we may need
  # for either plots or optimization.
  run_output <- list(vec_swe_all           = mb_model_output$vec_swe_all,
                     vec_surftype_all      = mb_model_output$vec_surftype_all,
                     vec_massbal_cumul     = mb_model_output$vec_massbal_cumul,
                     gl_massbal_cumul      = mb_model_output$gl_massbal_cumul,
                     gl_melt_cumul         = mb_model_output$gl_melt_cumul,
                     gl_accum_cumul        = mb_model_output$gl_accum_cumul,
                     stakes_start_ids_corr = stakes_start_ids_corr,
                     stakes_end_ids        = stakes_end_ids,
                     stakes_series_mod_all = stakes_series_mod_all,
                     stakes_mb_mod         = stakes_mb_mod,
                     stakes_mb_meas        = stakes_mb_meas,
                     stakes_bias           = stakes_bias,
                     global_bias           = global_bias,
                     global_rms            = global_rms)
  
  return(run_output)
  
}
