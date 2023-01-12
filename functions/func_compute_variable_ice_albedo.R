###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the variable ice albedo.              #
###################################################################################################

func_compute_variable_ice_albedo <- function(run_params,
                                             data_dhms) {
  
  cat("Computing variable ice albedo...\n")
  
  # List with 1 to 1 correspondence with the DHMs.
  ice_albedo_fact <- list()
  
  for (grid_id in 1:data_dhms$n_grids) {
    
    ice_albedo_fact[[grid_id]] <- clamp((run_params$albedo_ice_decrease_elev - data_dhms$elevation[[grid_id]]) * run_params$albedo_ice_decrease_fact,
                                        lower = 0,
                                        upper = Inf,
                                        values = TRUE) + 1
    
  }

  cat("  Finished computation of variable ice albedo.\n")
  
  return(ice_albedo_fact)

}
