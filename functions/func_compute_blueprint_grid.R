###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute a common blueprint grid               #
#                 to which the other grids will be resampled if needed, to match alignment.       #
###################################################################################################   

func_compute_blueprint_grid <- function(data_surftype, data_dhms) {
  
  # Find largest extent and most common resolution.
  xmin_all   <- min(c(sapply(data_surftype$grids, "xmin"), sapply(data_dhms$elevation, "xmin")))
  xmax_all   <- max(c(sapply(data_surftype$grids, "xmax"), sapply(data_dhms$elevation, "xmax")))
  ymin_all   <- min(c(sapply(data_surftype$grids, "ymin"), sapply(data_dhms$elevation, "ymin")))
  ymax_all   <- max(c(sapply(data_surftype$grids, "ymax"), sapply(data_dhms$elevation, "ymax")))
  extent_all <- extent(xmin_all, xmax_all, ymin_all, ymax_all)
  res_all    <- func_get_mode(c(sapply(data_surftype$grids, "xres"), sapply(data_dhms$elevation, "xres")))
  
  # This grid will be used as reference for extent and resolution.
  raster_blueprint <- raster(ext = extent_all, resolution = res_all)
  
  return(raster_blueprint)
} 
