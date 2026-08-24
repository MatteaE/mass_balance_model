###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to optimize the mass balance (either winter      #
#                 or annual, with a switch: code is almost the same.                              #
#                 Optimization is performed by computing the bias derivative w.r.t. the           #
#                 correction factor, since the bias is actually quasi-linear (weak albedo         #
#                 feedback). Then we can converge quickly to zero bias.                           #
################################################################################################### 

# Winter optimization:
# cancel the mean mass balance bias at the winter stakes by altering the
# precipitation correction.

# Annual optimization:
# cancel the mean mass balance bias at the annual stakes by altering the melt factor
# and radiation factor together, by the same amount. Also set the precipitation
# correction which we got from the winter optimization (set to no correction if we did no
# winter optimization).

# We have a list of additive corrections (see func_run_simulation_single)
# which are applied to the year_cur_params before the
# model run, so that we can tune parameters for the optimization.
# The variable specifying the amount of correction is
# corr_fact_cur, which multiplies the original parameter value to
# get the additive correction. So a corr_fact_cur of -1 means
# that the melt factor and radiation factors become 0,
# a corr_fact_cur of +1 means that they get doubled.
# This approach works seamlessly for the optimization
# of winter and annual mass balance, and allows easy
# optimization of other parameters.
# For the moment we just do the same optimization
# as the original IDL implementation, on Barkrak glacier
# there is almost no RMS gain (< 2 mm w.e. out of 500)
# to optimize independently the radiation factors.


#### ACTUAL OPTIMIZATION FUNCTION ####
# corr_fact_winter is considered only during the annual optimization,
# to use the correction previously determined for winter precipitation.
func_optimize_mb <- function(optimization_period, corr_fact_winter,
                             run_params, year_cur_params, year_data,
                             data_dhms, data_dems, data_surftype, data_radiation) {
  
  func_customlog("\n** ", year_data$year_cur, " ", optimization_period, " mass balance optimization **\n", level = 0)
  
  #### Select winter or annual data from year_data. ####
  if (optimization_period == "winter") {
    snowdist_init      <- year_data$snowdist_init_winter
    weather_series_cur <- year_data$weather_series_winter_cur
    dxdy               <- year_data$points_dxdy[["winter"]]
    nstakes            <- year_data$nstakes_winter
    model_days_n       <- year_data$model_winter_days_n
    massbal_meas_cur   <- year_data$massbal_winter_meas_cur
    stakes_cells       <- year_data$winter_stakes_cells
    store_melt_logi    <- FALSE
  } else {
    snowdist_init      <- year_data$snowdist_init_annual
    weather_series_cur <- year_data$weather_series_annual_cur
    dxdy               <- year_data$points_dxdy[["annual"]]
    nstakes            <- year_data$nstakes_annual
    model_days_n       <- year_data$model_annual_days_n
    massbal_meas_cur   <- year_data$massbal_annual_meas_cur
    stakes_cells       <- year_data$annual_stakes_cells
    store_melt_logi    <- TRUE
  }
  
  cat("Simulation runs", nrow(weather_series_cur), "days, from the start of", format(weather_series_cur$timestamp[1], "%F"), "to the end of", format(weather_series_cur$timestamp[nrow(weather_series_cur)], "%F"), "included\n")
  
  cat("\n* Optimization run # 1\n")
  corr_fact_prev <- 0
  
  mod_output_cur <- func_optim_worker(optimization_period, corr_fact_prev, corr_fact_winter,
                                      run_params, year_cur_params,
                                      year_data$dhm_grid_id, year_data$dem_grid_id, year_data$surftype_grid_id,
                                      data_dhms, data_dems, data_surftype,
                                      snowdist_init, data_radiation, weather_series_cur, year_data$dist_topographic_values_red,
                                      year_data$dist_probes_norm_values_red, year_data$grids_avalanche_cur,
                                      year_data$grid_ice_albedo_fact_cur_values,
                                      dxdy[[1]], dxdy[[2]], dxdy[[3]], dxdy[[4]],
                                      nstakes, model_days_n, massbal_meas_cur, stakes_cells,
                                      store_melt_logi = FALSE, verbose_logi = TRUE) # First iteration is never the final one so no need to store the melt.
  bias_prev <- mod_output_cur$global_bias
  
  # Create output data frames.
  # Note: we also extract and store the glacier-wide hydrological and measurement period mass balances.
  # These are extracted again later (only for the final calibrated run) to write the output and plots,
  # with a different logic (going through mass balance maps).
  # But if we do LOO validation/sensitivity, we need them earlier,
  # to store the result of each run (including uncalibrated and LOO ones),
  # so we extract them here for each run, from the gl_massbal_cumul vector.
  if (year_data$run_loo_logi) {
    df_runs_smb    <- func_compile_df_runs_smb(year_cur_params, year_data, mod_output_cur, 1, corr_fact_prev, "main_optim_dummy")
    df_runs_biases <- func_compile_df_runs_biases(year_data, mod_output_cur, 1, corr_fact_prev, "main_optim_dummy")
  }
  
  
  cat("\n* Optimization run # 2\n")
  # This 0.01 increment is arbitrary, we just need
  # a small interval to approximate the bias
  # derivative with a finite difference.
  # A very small value is safer in case the starting
  # value of the factors was very low
  # (we don't want to go to the negatives!).
  corr_fact_cur <- 0.01
  mod_output_cur <- func_optim_worker(optimization_period, corr_fact_cur, corr_fact_winter,
                                      run_params, year_cur_params,
                                      year_data$dhm_grid_id, year_data$dem_grid_id, year_data$surftype_grid_id,
                                      data_dhms, data_dems, data_surftype,
                                      snowdist_init, data_radiation, weather_series_cur, year_data$dist_topographic_values_red,
                                      year_data$dist_probes_norm_values_red, year_data$grids_avalanche_cur,
                                      year_data$grid_ice_albedo_fact_cur_values,
                                      dxdy[[1]], dxdy[[2]], dxdy[[3]], dxdy[[4]],
                                      nstakes, model_days_n, massbal_meas_cur, stakes_cells,
                                      store_melt_logi, verbose_logi = TRUE)
  bias_cur <- mod_output_cur$global_bias
  if (year_data$run_loo_logi) {
    df_runs_smb    <- rbind(df_runs_smb,
                            func_compile_df_runs_smb(year_cur_params, year_data, mod_output_cur, 2, corr_fact_cur, "main_optim_dummy"))
    df_runs_biases <- rbind(df_runs_biases,
                            func_compile_df_runs_biases(year_data, mod_output_cur, 2, corr_fact_cur, "main_optim_dummy"))
  }
  
  
  niter <- 2
  while ((abs(bias_cur) > run_params$optim_bias_threshold) && (niter < run_params$optim_max_iter)) {
    bias_slope <- (bias_cur - bias_prev) / (corr_fact_cur - corr_fact_prev)
    bias_prev <- bias_cur
    corr_fact_prev <- corr_fact_cur
    corr_fact_cur <- corr_fact_cur - (bias_cur / bias_slope) # Apply linear correction with the computed derivative.
    niter <- niter + 1
    cat("\n* Optimization run #", niter, "\n")
    mod_output_cur <- func_optim_worker(optimization_period, corr_fact_cur, corr_fact_winter,
                                        run_params, year_cur_params,
                                        year_data$dhm_grid_id, year_data$dem_grid_id, year_data$surftype_grid_id,
                                        data_dhms, data_dems, data_surftype,
                                        snowdist_init, data_radiation, weather_series_cur, year_data$dist_topographic_values_red,
                                        year_data$dist_probes_norm_values_red, year_data$grids_avalanche_cur,
                                        year_data$grid_ice_albedo_fact_cur_values,
                                        dxdy[[1]], dxdy[[2]], dxdy[[3]], dxdy[[4]],
                                        nstakes, model_days_n, massbal_meas_cur, stakes_cells,
                                        store_melt_logi, verbose_logi = TRUE)
    bias_cur <- mod_output_cur$global_bias
    if (year_data$run_loo_logi) {
      df_runs_smb    <- rbind(df_runs_smb,
                              func_compile_df_runs_smb(year_cur_params, year_data, mod_output_cur, niter, corr_fact_cur, "main_optim"))
      df_runs_biases <- rbind(df_runs_biases,
                              func_compile_df_runs_biases(year_data, mod_output_cur, niter, corr_fact_cur, "main_optim"))
    }
  }
  # The last (highest-id) iteration is the one which converged to zero global bias.
  # We mark it as such.
  if (year_data$run_loo_logi) {
    df_runs_smb$run_type[nrow(df_runs_smb)]       <- "main_optim_final"
    df_runs_biases$run_type[nrow(df_runs_biases)] <- "main_optim_final"
  }
  
  
  
  # These are the absolute additive corrections.
  # The final value of the parameter is given by
  # year_cur_params$<param_name> + corrections_best$<param_name>.
  if (optimization_period == "annual") {
    corrections_best <- list(melt_factor  = corr_fact_cur    * year_cur_params$melt_factor,
                             rad_fact_ice = corr_fact_cur    * year_cur_params$rad_fact_ice,
                             prec_corr    = corr_fact_winter * year_cur_params$prec_corr)
  } else if (optimization_period == "winter") {
    corrections_best <- list(prec_corr    = corr_fact_cur    * year_cur_params$prec_corr)
  }
  
  
  # Assemble output.
  out_l <- list(mod_output_cur   = mod_output_cur,
                corrections_best = corrections_best)
  if (year_data$run_loo_logi) {
    out_l$df_runs_smb    <- df_runs_smb
    out_l$df_runs_biases <- df_runs_biases
  }
  
  return(out_l)
  
}
