###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to setup the snow distribution grids                #
#                 from winter snow probes, if available.                                          #
###################################################################################################


func_setup_winter_probes_dist <- function(year_data,
                                          data_dhms,
                                          data_dems,
                                          run_params) {
  
  # Should we make a winter run to optimize the precipitation correction?
  # Only if we have some measurements of winter snow cover, else we can't.
  # We also disable this when we don't have annual stakes, we don't support
  # having winter measurements only.
  year_data$process_winter <- (year_data$nstakes_winter > 0) && ((year_data$nstakes_annual > 0))
  
  if (year_data$process_winter) {
    dist_probes_idw                 <- func_snow_probes_idw(run_params, year_data$massbal_winter_meas_cur, data_dhms)$var1.pred
    dist_probes_idw                 <- clamp(dist_probes_idw, lower = 0, upper = Inf, values = TRUE)
    dist_probes_idw_norm            <- dist_probes_idw / mean(dist_probes_idw[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]][,1])
  } else {
    # No winter probes to work with, so uniform distribution for the probes component.
    dist_probes_idw_norm            <- setValues(data_dhms$elevation[[1]], 1.0)
  }
  
  
  # Reduce variability of large-scale variability from winter probes
  # (accum_probes_fact < 1 makes sense if there are probes affected by avalanches).
  dist_probes_norm_mean <- mean(values(dist_probes_idw_norm, mat = F))
  if (is.na(dist_probes_norm_mean)) {
    func_customlog("There are NA values in the interpolated map of snow amounts from winter probes, please investigate!", level = 2)
    func_stop_msg()
  }
  if ((is.na(run_params$accum_probes_fact)) || (run_params$accum_probes_fact < 0)) {
    func_customlog("Parameter accum_probes_fact must be >= 0. Provided value: ", run_params$accum_probes_fact, level = 2)
    func_stop_msg()
  }
  dist_probes_norm_red <- dist_probes_norm_mean + run_params$accum_probes_fact * (dist_probes_idw_norm - dist_probes_norm_mean)
  
  # Normalize to arithmetic average = 1 on glacier.
  dist_probes_norm_red <- dist_probes_norm_red / mean(dist_probes_norm_red[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]][,1])
  
  year_data$dist_probes_norm_values_red <- values(dist_probes_norm_red, mat = FALSE)
  
  return(year_data)
  
}
