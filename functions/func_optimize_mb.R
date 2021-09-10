###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routines to optimize the mass balance (either winter     #
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


#### CONVENIENCE FUNCTION TO RUN THE OPTIMIZATION ON ####
# corr_fact_cur is the variable which gets optimized.
# corr_fact_winter is only used in the annual optimization,
# to use the previously optimized (constant) winter correction.
func_optim_worker <- function(optimization_period, corr_fact_cur, corr_fact_winter,
                              run_params, year_cur_params, dhm_grid_id, dem_grid_id, surftype_grid_id,
                              data_dhms, data_dems, data_surftype,
                              snowdist_init, data_radiation, weather_series_cur, dist_topographic_values_red,
                              dist_probes_norm_values_red, grids_avalanche_cur,
                              grid_ice_albedo_fact_cur_values,
                              dx1, dx2, dy1, dy2,
                              nstakes, model_days_n, massbal_meas_cur, stakes_cells) {
  
  
  if (optimization_period == "annual") {
    
    corrections_cur <- list(melt_factor  = corr_fact_cur    * year_cur_params$melt_factor,
                            rad_fact_ice = corr_fact_cur    * year_cur_params$rad_fact_ice,
                            prec_corr    = corr_fact_winter * year_cur_params$prec_corr)
    
  } else if (optimization_period == "winter") {
    
    corrections_cur <- list(prec_corr  = corr_fact_cur * year_cur_params$prec_corr)
    
  } else {
    
    stop("Wrong optimization_period!")
    
  }
  
  # We already set this in the global environment
  # so that we don't have to re-run the model with
  # the optimized parameters to get the actual
  # model output.
  mod_output_cur <- func_run_simulation_single(corrections_cur,
                                               run_params, year_cur_params,
                                               dhm_grid_id, dem_grid_id, surftype_grid_id,
                                               data_dhms, data_dems, data_surftype,
                                               snowdist_init, data_radiation, weather_series_cur, dist_topographic_values_red,
                                               dist_probes_norm_values_red, grids_avalanche_cur,
                                               grid_ice_albedo_fact_cur_values,
                                               dx1, dx2, dy1, dy2,
                                               nstakes, model_days_n, massbal_meas_cur, stakes_cells)
  
  return(mod_output_cur)
  
}




#### ACTUAL OPTIMIZATION FUNCTION ####
# corr_fact_winter is considered only during the annual optimization,
# to use the correction previously determined for winter precipitation.
func_optimize_mb <- function(optimization_period, corr_fact_winter,
                             run_params, year_cur_params,
                             year_data,
                             data_dhms, data_dems, data_surftype, data_radiation,
                             nstakes, model_days_n, massbal_meas_cur, stakes_cells) {
  
  cat("\n**", year_cur, optimization_period, "mass balance optimization **\n")
  cat("\n* Optimization run # 1\n")
  corr_fact_prev <- 0
  
  #### Select winter or annual data from year_data. ####
  if (optimization_period == "winter") {
    snowdist_init      <- year_data$snowdist_init_winter
    weather_series_cur <- year_data$weather_series_winter_cur
    dxdy               <- year_data$stake_dxdy[["winter"]]
    nstakes            <- year_data$nstakes_winter
    model_days_n       <- year_data$model_winter_days_n
    massbal_meas_cur   <- year_data$massbal_winter_meas_cur
    stakes_cells       <- year_data$winter_stakes_cells
  } else {
    snowdist_init      <- year_data$snowdist_init_annual
    weather_series_cur <- year_data$weather_series_annual_cur
    dxdy               <- year_data$stake_dxdy[["annual"]]
    nstakes            <- year_data$nstakes_annual
    model_days_n       <- year_data$model_annual_days_n
    massbal_meas_cur   <- year_data$massbal_annual_meas_cur
    stakes_cells       <- year_data$annual_stakes_cells
  }
  
  
  mod_output_cur <- func_optim_worker(optimization_period, corr_fact_prev, corr_fact_winter,
                                      run_params, year_cur_params,
                                      year_data$dhm_grid_id, year_data$dem_grid_id, year_data$surftype_grid_id,
                                      data_dhms, data_dems, data_surftype,
                                      snowdist_init, data_radiation, weather_series_cur, year_data$dist_topographic_values_red,
                                      year_data$dist_probes_norm_values_red, year_data$grids_avalanche_cur,
                                      year_data$grid_ice_albedo_fact_cur_values,
                                      dxdy[[1]], dxdy[[2]], dxdy[[3]], dxdy[[4]],
                                      nstakes, model_days_n, massbal_meas_cur, stakes_cells)
  bias_prev <- mod_output_cur$global_bias
  
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
                                      nstakes, model_days_n, massbal_meas_cur, stakes_cells)
  bias_cur <- mod_output_cur$global_bias
    
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
                                        nstakes, model_days_n, massbal_meas_cur, stakes_cells)
    bias_cur <- mod_output_cur$global_bias
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
  
  return(list(mod_output_cur   = mod_output_cur,
              corrections_best = corrections_best))
  
}
