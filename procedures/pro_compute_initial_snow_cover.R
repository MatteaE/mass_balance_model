###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to call the computation of the initial snow cover.  #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.


# The initial snow cover can be either (1) a single, constant estimated map
# (from topography, avalanches and user-defined snow line elevation)
# or (2) the result of the previous year of modeling at the starting date
# of the simulation.
# In case (2), we distinguish between initial snow cover for winter
# and for annual modeling, since the two can have different
# dates, depending on the dates of the annual and winter stakes.
# Case (2) is obviously not applicable to the first year of modeling
# (there is no previous result for it).
if (run_params$initial_snow_dist_from_model && swe_prev_available) {
  
  # NOTE: here weather_series_annual_cur and mod_output_annual_cur are
  # still the weather series and modeled series of the PREVIOUS year!
  swe_prev_annual_day_id <- which.min(abs(weather_series_annual_cur$timestamp - model_time_bounds[1]))
  snowdist_init_annual <- setValues(data_dhms$elevation[[dhm_grid_id]], mod_output_annual_cur$vec_swe_all[(swe_prev_annual_day_id - 1) * run_params$grid_ncells + 1:run_params$grid_ncells])
  
  if (process_winter) {
    swe_prev_winter_day_id <- which.min(abs(weather_series_annual_cur$timestamp - model_time_bounds[3]))
    snowdist_init_winter <- setValues(data_dhms$elevation[[dhm_grid_id]], mod_output_annual_cur$vec_swe_all[(swe_prev_winter_day_id - 1) * run_params$grid_ncells + 1:run_params$grid_ncells])
  }
  
  # Here instead estimate the initial snow cover from snow line elevation,
  # topography, avalanches and snow probes if available.
} else {
  snowdist_init_annual <- func_compute_initial_snow_cover(run_params,
                                                          data_dhms,
                                                          data_dems,
                                                          grids_snowdist_topographic,
                                                          grids_avalanche_cur,
                                                          dist_probes_idw_norm,
                                                          dhm_grid_id,
                                                          dem_grid_id,
                                                          massbal_winter_meas_cur)
  snowdist_init_winter <- snowdist_init_annual
} 
