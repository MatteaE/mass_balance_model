###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the start date for the stakes which   #
#                 were marked as "unknown" (NA), whose start is assigned to the date of the       #
#                 modeled mass balance minimum.                                                   #
###################################################################################################


func_compute_unknown_stakes_start_ids <- function(run_params,
                                                  annual_stakes_start_ids,
                                                  weather_series_cur,
                                                  stakes_series_mod_all) {
  
  # Find start date for stakes with NA (i.e. mass balance minimum of previous year)
  # This search only spans the previous year (i.e., a stake with NA will never be
  # found to be multi-annual within this function).
  
  annual_stakes_start_ids_corr <- annual_stakes_start_ids  # We leave the original set unaltered, it will serve during optimization.
  stakes_start_unknown_ids     <- which(is.na(annual_stakes_start_ids))
  
  # Only do this search if there are any such stakes.
  if (length(stakes_start_unknown_ids) > 0) {
    
    # Earliest possible day for the start of the observation period
    # (i.e. for the mass balance minimum) at a point with unknown start (NA).
    # We set it to 90 days before the start of the current hydrological year,
    # this should be safely in the middle of the ablation season for all points.
    # The hydrological year always starts during calendar year YYYY-1 where YYYY
    # is the current target year.
    # In case there are multi-annual stakes, this restricts the search to the latest year.
    stakes_start_earliest_id <- max(1, which(format(weather_series_cur$timestamp, "%Y/%m/%d") == paste0(max(weather_series_cur$year)-1, "/", run_params$hydro_start_mmdd))-90)
    
    # User-defined latest possible day for the search of
    # the start of the stake observation period, i.e. for the mass balance minimum,
    # at a point with unknown start (NA).
    # The max() ensures that the most recent minimum is selected
    # in case there are multi-annual stakes (so that the measurement
    # period spans two years or more).
    stakes_start_latest_id <- max(which(format(weather_series_cur$timestamp, "%m/%d") == run_params$stakes_unknown_latest_start))
    
    
    if (!is.finite(stakes_start_earliest_id) || !is.finite(stakes_start_latest_id) || (stakes_start_latest_id < stakes_start_earliest_id)) {
      func_customlog("There was a problem setting the search range for the date of stakes with unknown start.", level = 2)
      func_customlog("Please check the time bounds of the simulation and the value of run_params$stakes_unknown_latest_start", level = 0)
      func_stop()
    }
    
    for (stake_cur_id in stakes_start_unknown_ids) {
      # cat("Finding start date for stake", stake_cur_id, "...\n")
      # stakes_start_earliest_id - 1 because we do which.min() on a subset starting on stakes_start_earliest_id
      annual_stakes_start_ids_corr[stake_cur_id] <- stakes_start_earliest_id - 1 + which.min(stakes_series_mod_all[stakes_start_earliest_id:stakes_start_latest_id, stake_cur_id])
    }
    
  }
  
  return(annual_stakes_start_ids_corr)
  
}
