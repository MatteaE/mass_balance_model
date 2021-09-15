###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to compute the main grid parameters.                #
###################################################################################################

# Assign global grid parameters to run_params.
# We do it explicitly here since we need to have
# loaded the elevation grids to get their parameters.
# These overwrite the loaded run parameters, to avoid
# the possibility of dangerous inconsistencies with the loaded data.
func_compute_grid_parameters <- function(run_params, data_dhms) {
  
  run_params$grid_nrow       <- nrow(data_dhms$elevation[[1]])
  run_params$grid_ncol       <- ncol(data_dhms$elevation[[1]])
  run_params$grid_cell_size  <- xres(data_dhms$elevation[[1]])
  run_params$grid_ncells     <- run_params$grid_nrow * run_params$grid_ncol 
  
  
  return(run_params)
  
}
