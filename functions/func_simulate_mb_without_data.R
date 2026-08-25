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
  
  func_customlog("\n** ", year_data$year_cur, " annual mass balance simulation **\n", level = 0)
  
  snowdist_init      <- year_data$snowdist_init_annual
  weather_series_cur <- year_data$weather_series_annual_cur
  
  cat("Simulation runs", nrow(weather_series_cur), "days, from the start of", format(weather_series_cur$timestamp[1], "%F"), "to the end of", format(weather_series_cur$timestamp[nrow(weather_series_cur)], "%F"), "included\n")
  
  #### RUN MASS BALANCE MODEL ####
  mod_output_cur <- func_massbal_model(run_params,
                                       year_cur_params,
                                       values(data_dhms$elevation[[year_data$dhm_grid_id]])[,1],
                                       data_dems$glacier_cell_ids[[year_data$dem_grid_id]],
                                       as.numeric(values(data_surftype$grids[[year_data$surftype_grid_id]])),
                                       as.numeric(values(snowdist_init)),
                                       data_radiation,
                                       weather_series_cur,
                                       year_data$dist_topographic_values_red,
                                       year_data$dist_probes_norm_values_red,
                                       year_data$grids_avalanche_cur,
                                       year_data$grid_ice_albedo_fact_cur_values,
                                       verbose_level = 1)
  cat("melt_factor =",  round(year_cur_params$melt_factor, 3),  "\n")
  cat("rad_fact_ice =", round(year_cur_params$rad_fact_ice, 3), "\n")
  cat("prec_corr =",    round(year_cur_params$prec_corr, 3),    "\n")
  
  # Compile the data frame of SMB per model realization with the
  # result from the single realization of the current year (which
  # is not optimized).
  # The definition of "corr_fact" here is somewhat uncertain, since
  # this function can have various sources for the values of the melt parameters:
  # (1) custom values, possibly for some parameters only, if provided within the single-year parameter file
  # (2) the average result of the values from the optimized years, if run_params$nodata_years_automatic is TRUE and there are some parameters not set via single-year parameter file
  # (3) the global defaults, e.g. if run_params$nodata_years_automatic is FALSE.
  # So, corr_fact here gets an NA.
  df_runs_smb    <- func_compile_df_runs_smb(year_cur_params, year_data, mod_output_cur, 1, NA, "main_simul_single")
  
  return(list(mod_output_cur = mod_output_cur,
              df_runs_smb    = df_runs_smb))
  
}
