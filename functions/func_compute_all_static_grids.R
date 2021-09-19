###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to call the computation of static grids for         #
#                 avalanches, topographic snow distribution and variable ice albedo.              #
###################################################################################################

func_compute_all_static_grids <- function(run_params,
                                         data_dhms,
                                         data_dems) {

  message("Computing static grids...")
  grids_static_list <- list()
  
  grids_static_list$grids_avalanche            <-   func_compute_avalanche_static_grids(run_params, data_dhms)
  grids_static_list$grids_snowdist_topographic <-   func_compute_snowdist_topographic(run_params, data_dhms, data_dems)
  grids_static_list$grids_ice_albedo_fact      <-   func_compute_variable_ice_albedo(run_params, data_dhms)

return(grids_static_list)

}
