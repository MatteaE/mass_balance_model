###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to call the computation of the initial snow cover.  #
###################################################################################################


# The initial snow cover can be either (1) a single, constant estimated map
# (from topography, avalanches and user-defined snow line elevation)
# or (2) the result of the previous year of modeling at the starting date
# of the simulation.
# In case (2), we distinguish between initial snow cover for winter
# and for annual modeling, since the two can have different
# dates, depending on the dates of the annual and winter stakes.
# Case (2) is obviously not applicable to the first year of modeling
# (there is no previous result for it).
func_setup_initial_snow_cover <- function(year_data,
                                          year_data_prev,
                                          data_dhms,
                                          data_dems,
                                          grids_snowdist_topographic,
                                          swe_prev_available,
                                          run_params) {
  
  cat("Setting up the initial snow cover...\n")
  
  # If told to do so, use the previous year's result as starting SWE map.
  # We can do that only if the previous modeled year is also the previous
  # year (we have to save memory by keeping only the last model output!).
  if ((run_params$initial_snow_dist_from_model) &&
      (swe_prev_available[year_data$year_id])  &&
      (year_data_prev$year_cur == year_data$year_cur - 1)) {
    
    # NOTE: we retrieve the weather_series_annual_cur
    # and mod_output_annual_cur of the PREVIOUS year!
    swe_prev_annual_day_id         <- which.min(abs(year_data_prev$weather_series_annual_cur$timestamp - year_data$model_time_bounds[1]))
    year_data$snowdist_init_annual <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], year_data_prev$mod_output_annual_cur$vec_swe_all[(swe_prev_annual_day_id - 1) * run_params$grid_ncells + 1:run_params$grid_ncells])
    
    if (year_data$process_winter) {
      swe_prev_winter_day_id         <- which.min(abs(year_data_prev$weather_series_annual_cur$timestamp - year_data$model_time_bounds[3]))
      year_data$snowdist_init_winter <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], year_data_prev$mod_output_annual_cur$vec_swe_all[(swe_prev_winter_day_id - 1) * run_params$grid_ncells + 1:run_params$grid_ncells])
    }
    
    # Here instead estimate the initial snow cover from snow line elevation,
    # topography, avalanches and snow probes if available.
  } else {
    year_data$snowdist_init_annual <- func_compute_initial_snow_cover(run_params,
                                                                      data_dhms,
                                                                      data_dems,
                                                                      grids_snowdist_topographic,
                                                                      year_data$grids_avalanche_cur,
                                                                      year_data$dist_probes_idw_norm,
                                                                      year_data$dhm_grid_id,
                                                                      year_data$dem_grid_id,
                                                                      year_data$massbal_winter_meas_cur)
    year_data$snowdist_init_winter <- year_data$snowdist_init_annual
  }
  
  return(year_data)
  
}
