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
  year_data$process_winter <- (year_data$nstakes_winter > 0)
  
  if (year_data$process_winter) {
    dist_probes_idw                 <- func_snow_probes_idw(run_params, year_data$massbal_winter_meas_cur, data_dhms)
    dist_probes_idw                 <- clamp(dist_probes_idw, lower = 0, upper = Inf)
    year_data$dist_probes_idw_norm  <- dist_probes_idw / mean(dist_probes_idw[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]])
  } else {
    # No winter probes to work with, so uniform distribution for the probes component.
    year_data$dist_probes_idw_norm  <- setValues(data_dhms$elevation[[1]], 1.0)
  }
  dist_probes_norm_values               <- getValues(year_data$dist_probes_idw_norm) # For the accumulation model.
  dist_probes_norm_mean                 <- mean(dist_probes_norm_values, na.rm = TRUE)
  year_data$dist_probes_norm_values_red <- dist_probes_norm_mean + run_params$accum_probes_red_fac * (dist_probes_norm_values - dist_probes_norm_mean)
  
  return(year_data)
  
}
