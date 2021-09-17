###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to simulate the annual mass balance of a year       #
#                 without mass balance measurements. The mass balance model is then run only      #
#                 once, with the given parameters.                                                #
###################################################################################################  

func_simulate_mb_without_data <- function(run_params,
                                          year_cur_params,
                                          year_data,
                                          data_dhms,
                                          data_dems,
                                          data_surftype,
                                          data_radiation) {
  
  cat("\n**", year_data$year_cur, "annual mass balance simulation **\n")
  
  snowdist_init      <- year_data$snowdist_init_annual
  weather_series_cur <- year_data$weather_series_annual_cur
  model_days_n       <- year_data$model_annual_days_n
  
  #### RUN MASS BALANCE MODEL ####
  mb_model_output <- func_massbal_model(run_params,
                                        year_cur_params,
                                        getValues(data_dhms$elevation[[year_data$dhm_grid_id]]),
                                        data_dems$glacier_cell_ids[[year_data$dem_grid_id]],
                                        getValues(data_surftype$grids[[year_data$surftype_grid_id]]),
                                        getValues(snowdist_init),
                                        data_radiation,
                                        weather_series_cur,
                                        year_data$dist_topographic_values_red,
                                        year_data$dist_probes_norm_values_red,
                                        year_data$grids_avalanche_cur,
                                        year_data$grid_ice_albedo_fact_cur_values)
  cat("melt_factor =",  round(year_cur_params$melt_factor, 3),  "\n")
  cat("rad_fact_ice =", round(year_cur_params$rad_fact_ice, 3), "\n")
  cat("prec_corr =",    round(year_cur_params$prec_corr, 3),    "\n")
  
  
  # Compile output with everything we may need
  # for either plots or optimization.
  run_output <- list(vec_swe_all           = mb_model_output$vec_swe_all,
                     vec_surftype_all      = mb_model_output$vec_surftype_all,
                     vec_massbal_cumul     = mb_model_output$vec_massbal_cumul,
                     gl_massbal_cumul      = mb_model_output$gl_massbal_cumul,
                     gl_melt_cumul         = mb_model_output$gl_melt_cumul,
                     gl_accum_cumul        = mb_model_output$gl_accum_cumul)
                     # stakes_start_ids_corr = NA,
                     # stakes_end_ids        = NA,
                     # stakes_series_mod_all = NA,
                     # stakes_mb_mod         = NA,
                     # stakes_mb_meas        = NA,
                     # stakes_bias           = NA,
                     # global_bias           = NA,
                     # global_rms            = NA)

  return(run_output)
  
}