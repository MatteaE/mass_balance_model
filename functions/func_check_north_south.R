###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which determines whether the simulation is       #
#                 taking place in the Northern or Southern Hemisphere.                            #
###################################################################################################

# The function also sets dates which are specific to the Northern or Southern Hemispheres.

func_check_north_south <- function(raster_blueprint,
                                   run_params) {
  
  ext_cur     <- ext(raster_blueprint)[1:4]
  crds_center <- cbind(mean(ext_cur[1:2]), mean(ext_cur[3:4]))
  lat_center  <- terra::project(crds_center, run_params$grids_crs_epsg, "EPSG:4326")[,2]
  
  
  if (lat_center >= 0) {
    
    cat("Setting up date parameters for the Northern Hemisphere.\n")
    run_params$north_south        <- "North"
    run_params$firnification_date <- "03/01"
    run_params$hydro_start_mmdd   <- "10/01"
    run_params$hydro_end_mmdd     <- "09/30"
    if (is.na(run_params$massbal_fixed_winter_start))  run_params$massbal_fixed_winter_start   <- "10/01"
    if (is.na(run_params$massbal_fixed_winter_end))    run_params$massbal_fixed_winter_end     <- "04/30"
    
    # This is the latest possible date for the search of the start date of accumulation points marked with NA.
    # In case of multi-annual observation periods, it refers to the most recent instance of this date within the simulation.
    if (is.na(run_params$stakes_unknown_latest_start)) run_params$stakes_unknown_latest_start  <- "02/28"

    # See func_process_run_params and func_select_year_mb_measurements for an explanation of these two.
    if (is.na(run_params$stake_end_earliest))          run_params$stake_end_earliest           <- "12/01"
    if (is.na(run_params$stake_end_latest))            run_params$stake_end_latest             <- "11/30"
    
  } else {
    
    cat("Setting up date parameters for the Southern Hemisphere.\n")
    run_params$north_south        <- "South"
    run_params$firnification_date <- "09/01"
    run_params$hydro_start_mmdd   <- "04/01"
    run_params$hydro_end_mmdd     <- "03/31"
    if (is.na(run_params$massbal_fixed_winter_start))  run_params$massbal_fixed_winter_start   <- "04/01"
    if (is.na(run_params$massbal_fixed_winter_end))    run_params$massbal_fixed_winter_end     <- "10/31"
    if (is.na(run_params$stakes_unknown_latest_start)) run_params$stakes_unknown_latest_start  <- "08/31"
    if (is.na(run_params$stake_end_earliest))          run_params$stake_end_earliest           <- "06/01"
    if (is.na(run_params$stake_end_latest))            run_params$stake_end_latest             <- "05/31"
  }
  
  return(run_params)
  
}
