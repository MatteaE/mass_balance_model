###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which calls all the data loading sub-routines.   #
#                 The DEM (= elevation model cropped to the glacier) is also computed here, from  #
#                 DHM (= full elevation model with no gaps) and outline.                          #
###################################################################################################  


#### Load input data ####
func_load_data_all <- function(run_params) {
  
  message("Loading all input data...")
  
  data_all <- list()
  
  data_all$data_weather               <-   func_load_weather(run_params)
  data_all$data_surftype              <-   func_load_surftype_grids(run_params)
  data_all$data_outlines              <-   func_load_outlines(run_params)
  data_all$data_dhms                  <-   func_load_elevation_grids(run_params)
  
  # Check and if needed resample the elevation / surface type grids, for alignment.
  data_all$raster_blueprint           <-   func_compute_blueprint_grid(data_all$data_surftype, data_all$data_dhms)
  data_all                            <-   func_check_resample_grids(run_params, data_all)
  
  # Compute DEMs from DHMs and outlines.
  data_all$data_dems                  <-   func_dhm_to_dem(run_params, data_all$data_dhms, data_all$data_outlines)
  
  # Fix surface type in case it is inconsistent with the DEM.
  data_all$data_surftype              <-   func_repair_surface_type(run_params, data_all$data_dems, data_all$data_surftype)
  
  data_all$data_radiation             <-   func_load_radiation_grids(run_params, data_all$raster_blueprint)
  data_all$data_massbalance_annual    <-   func_load_massbalance_measurements(run_params, "annual", data_all$data_dhms)
  data_all$data_massbalance_winter    <-   func_load_massbalance_measurements(run_params, "winter", data_all$data_dhms)
  
  # Memory cleanup.
  invisible(gc())
  
  cat("  Finished loading all input data.\n\n")
  
  return(data_all)
}
