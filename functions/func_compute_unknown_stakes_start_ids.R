###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the start date for the stakes which   #
#                 were marked as "unknown" (NA), whose start is assigned to the date of the       #
#                 modeled mass balance minimum.                                                   #
###################################################################################################


func_compute_unknown_stakes_start_ids <- function(run_params, annual_stakes_start_ids, weather_series_cur, stakes_series_mod_all) {
  
  # Find start date for stakes with NA (i.e. mass balance minimum of previous year):
  annual_stakes_start_ids_corr <- annual_stakes_start_ids  # We leave the original set unaltered, it will serve during optimization.
  stakes_start_unknown_ids <- which(is.na(annual_stakes_start_ids))
  
  # User-defined latest possible day for the search of
  # the stake start, i.e. for the mass balance minimum.
  stakes_start_latest_id <- which(format(weather_series_cur$timestamp, "%m/%d") == run_params$stakes_unknown_latest_start)
  
  for (stake_cur_id in stakes_start_unknown_ids) {
    # cat("Finding start date for stake", stake_cur_id, "...\n")
    annual_stakes_start_ids_corr[stake_cur_id] <- which.min(stakes_series_mod_all[1:stakes_start_latest_id, stake_cur_id])
  }
  
  return(annual_stakes_start_ids_corr)
  
}
