###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to process the winter optimization.                 #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.


# We set this here so that there is no correction
# if we don't do the winter optimization.
corr_fact_winter      <- 0
# We set this to NULL to have it defined (for the
# extraction functions) in case we don't do winter processing.
mod_output_winter_cur <- NULL
if (process_winter)  {
  
  # We ask duplicates = FALSE, else the bilinear filtering in func_extract_modeled_stakes()
  # can fail when a stake is exactly at the same (X and/or Y) coordinate as a cell center.
  # duplicates = FALSE returns four different cells. In case we have a stake exactly
  # aligned with a cell center, unless we are at the lower raster border (which we should
  # always avoid!) the additional cells returned with duplicates = FALSE (cells which would
  # not be part of the actual adjacent cells) have higher index than the "true" adjacent cells.
  winter_stakes_cells <- rowSort(fourCellsFromXY(data_dhms$elevation[[dhm_grid_id]], as.matrix(massbal_winter_meas_cur[,4:5]), duplicates = FALSE))
  
  model_winter_bounds <- model_time_bounds[3:4]
  
  # Select weather series period.
  weather_series_winter_cur <- data_weather[which(data_weather$timestamp == model_winter_bounds[1]):(which(data_weather$timestamp == model_winter_bounds[2])),]
  model_winter_days_n <- nrow(weather_series_winter_cur)
  
  # This leaves the result of the last optimization
  # iteration in mod_output_annual_cur.
  # The NA is for the optimized corr_fact_winter (which we are
  # determining here, so we don't use a previous value: it is ignored).
  optim_corr_winter <- func_optimize_mb("winter", NA,
                                        run_params, year_cur_params,
                                        dhm_grid_id, dem_grid_id, surftype_grid_id,
                                        data_dhms, data_dems, data_surftype, snowdist_init_winter, data_radiation,
                                        weather_series_winter_cur, dist_topographic_values_red,
                                        dist_probes_norm_values_red, grids_avalanche_cur,
                                        grid_ice_albedo_fact_cur_values,
                                        stake_dxdy[["winter"]],
                                        nstakes_winter, model_winter_days_n, massbal_winter_meas_cur,
                                        winter_stakes_cells)
  
  # Save the correction factor, to re-use it during the annual optimization.
  # We divide by the original prec_corr since the corr_fact is relative
  # (it gets multiplied again during optimization, inside func_optim_worker()).
  corr_fact_winter <- optim_corr_winter$prec_corr / year_cur_params$prec_corr
  
  # Free some memory after processing.
  invisible(gc())
}
