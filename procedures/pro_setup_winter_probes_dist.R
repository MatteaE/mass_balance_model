###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to setup the snow distribution grids                #
#                 from winter snow probes, if available.                                          #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.


# Should we make a winter run to optimize the precipitation correction?
# Only if we have some measurements of winter snow cover, else we can't.
process_winter <- (nstakes_winter > 0)

if (process_winter) {
  dist_probes_idw           <- func_snow_probes_idw(run_params, massbal_winter_meas_cur, data_dhms)
  dist_probes_idw           <- clamp(dist_probes_idw, lower = 0, upper = Inf)
  dist_probes_idw_norm      <- dist_probes_idw / mean(dist_probes_idw[data_dems$glacier_cell_ids[[dem_grid_id]]])
} else {
  # No winter probes to work with, so uniform distribution for the probes component.
  dist_probes_idw_norm      <- setValues(data_dhms$elevation[[1]], 1.0)
}
dist_probes_norm_values     <- getValues(dist_probes_idw_norm) # For the accumulation model.
dist_probes_norm_mean       <- mean(dist_probes_norm_values, na.rm = T)
dist_probes_norm_values_red <- dist_probes_norm_mean + run_params$accum_probes_red_fac * (dist_probes_norm_values - dist_probes_norm_mean)

