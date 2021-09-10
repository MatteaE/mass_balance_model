###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to process the annual optimization.                 #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.


# Here do the annual processing.
# Find grid cells corresponding to the annual stakes.
# We sort them to enable vectorized bilinear filtering.
annual_stakes_cells <- rowSort(fourCellsFromXY(data_dhms$elevation[[dhm_grid_id]], as.matrix(massbal_annual_meas_cur[,4:5]), duplicates = FALSE))

# Time specification of the annual run.
model_annual_bounds <- model_time_bounds[1:2]

# Select weather series period.
weather_series_annual_cur <- data_weather[which(data_weather$timestamp == model_annual_bounds[1]):(which(data_weather$timestamp == model_annual_bounds[2])),]
model_annual_days_n <- nrow(weather_series_annual_cur)


# This returns the optimized parameters
# and also leaves the result of the
# last optimization iteration in
# a variable called mod_output_annual_cur.
optim_corr_annual <- func_optimize_mb("annual", corr_fact_winter,
                                      run_params, year_cur_params,
                                      dhm_grid_id, dem_grid_id, surftype_grid_id,
                                      data_dhms, data_dems, data_surftype, snowdist_init_annual, data_radiation,
                                      weather_series_annual_cur, dist_topographic_values_red,
                                      dist_probes_norm_values_red, grids_avalanche_cur,
                                      grid_ice_albedo_fact_cur_values,
                                      stake_dxdy[["annual"]],
                                      nstakes_annual, model_annual_days_n, massbal_annual_meas_cur,
                                      annual_stakes_cells)
# Free some memory after processing.
invisible(gc())

# After an annual model run we have SWE information
# suitable for use as starting condition of the next
# year, if the user decides to use it.
swe_prev_available <- TRUE
