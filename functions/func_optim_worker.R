###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the convenience routine to be optimized as we optimize       #
#                 the mass balance (either winter or annual).                                     #
################################################################################################### 


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
    
    stop("FATAL: wrong optimization_period!")
    
  }
  
  # The model is run here.
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
