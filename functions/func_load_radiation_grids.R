###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for the grid(s) of daily potential       #
#                 radiation sum.                                                                  #
#                 As output we get a list with one numeric vector per day of year (366 vectors,   #
#                 last two equal). We don't use rasters in order to enable Rcpp to use the        #
#                 radiation values.                                                               #
###################################################################################################


func_load_radiation_grids <- function(run_params) {
  
  # Here we will put the output.
  grids_out <- list()
  
  grid_paths <- file.path(run_params$dir_data_radiation,
                          paste0(run_params$filename_radiation_prefix,
                                 sprintf("%03d", 1:365),
                                 run_params$filename_radiation_suffix))

  
  # Actual loading happens here.
  for (doy in 1:365) {
    
    cat("\rLoading radiation files...", doy, "/", 365)
    grids_out[[doy]] <- getValues(raster(grid_paths[doy]))
    
  }
  cat("\n")
  
  grids_out[[366]] <- grids_out[[365]]
  
  return(grids_out)
  
}
