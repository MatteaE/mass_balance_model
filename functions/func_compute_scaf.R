###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to compute the daily SCAF from the big vector       #
#                 of surface types.                                                               #
###################################################################################################

func_compute_scaf <- function(year_data,
                              run_params,
                              data_dems) {
  
  scaf_daily <- numeric(year_data$model_annual_days_n)
  
  # model_annual_days_n + 1: there are n days modeled, so n+1 scaf values
  # (incl. start of first day and end of last one).
  for (day_id in 1:(year_data$model_annual_days_n+1)) {
    offset_cur <- day_id * run_params$grid_ncells # The _cur start at the end of the first day (i.e. there is one "iteration" before which holds the initial state).
    cells_ids  <- offset_cur - run_params$grid_ncells + 1:run_params$grid_ncells # Indices of all the grid cells with values at the beginning of the current day.
    cells_glaciated_ids <- cells_ids[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]
    cells_snow_n <- length(which(year_data$mod_output_annual_cur$vec_surftype_all[cells_glaciated_ids] == 2))
    scaf_daily[day_id] <- cells_snow_n * run_params$grid_cell_size * run_params$grid_cell_size / year_data$glacier_area
  }
  
  return(scaf_daily * 100) # Percent.

}
