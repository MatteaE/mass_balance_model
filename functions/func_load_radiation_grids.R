###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for the grid(s) of daily potential       #
#                 radiation sum.                                                                  #
#                 If needed, grids are resampled to match the extent and resolution of the        #
#                 elevation grids.                                                                #
#                 As output we get a list with one numeric vector per day of year (366 vectors,   #
#                 last two equal). We don't use rasters in order to enable Rcpp to use the        #
#                 radiation values.                                                               #
###################################################################################################


func_load_radiation_grids <- function(run_params, raster_blueprint) {
  
  # Here we will put the output.
  grids_out <- list()
  
  grid_paths <- file.path(run_params$dir_data_radiation,
                          paste0(run_params$filename_radiation_prefix,
                                 sprintf("%03d", 1:365),
                                 run_params$filename_radiation_suffix))

  
  # Actual loading happens here.
  # We resample grids on the fly if needed.
  for (doy in 1:365) {
    
    cat("\rLoading radiation files...", doy, "/", 365)
    ras_cur <- raster(grid_paths[doy])
    if ((extent(ras_cur) != extent(raster_blueprint)) || (xres(ras_cur) != xres(raster_blueprint))) {
      ras_cur <- resample(ras_cur, raster_blueprint, method = "bilinear")
    }
    grids_out[[doy]] <- getValues(ras_cur)
    
    # We don't want any NAs in the radiation (they can arise with resampling to larger extent).
    grids_out[[doy]][is.na(grids_out[[doy]])] <- 0
    
  }
  cat("\n")
  
  grids_out[[366]] <- grids_out[[365]]
  
  return(grids_out)
  
}
