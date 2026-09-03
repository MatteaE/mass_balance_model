###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute a common blueprint grid               #
#                 to which the other grids will be resampled if needed, to match alignment.       #
###################################################################################################   

func_compute_blueprint_grid <- function(run_params,
                                        data_surftype,
                                        data_dhms) {
  
  # Find largest extent and most common resolution.
  res_all    <- func_get_mode(c(sapply(data_surftype$grids, "xres"), sapply(data_dhms$elevation, "xres")))
  res_ndec   <- func_count_decimals(res_all)
  
  if (res_ndec > 3) {
    func_customlog("The computed grid resolution is ", sprintf(paste0("%.", res_ndec, "f"), res_all), " which has more than three decimal places.", level = 1)
    func_customlog("This can produce errors in the grid calculations. Resolution will be rounded to three decimal places.", level = 0)
    res_all <- round(res_all, 3)
  }
  
  crs_all    <- run_params$grids_crs_epsg
  
  # Compute intersection of all grids - like this, all resampled grids will have data.
  xmin_all   <- max(c(sapply(data_surftype$grids, "xmin"), sapply(data_dhms$elevation, "xmin")))
  xmax_all   <- min(c(sapply(data_surftype$grids, "xmax"), sapply(data_dhms$elevation, "xmax")))
  ymin_all   <- max(c(sapply(data_surftype$grids, "ymin"), sapply(data_dhms$elevation, "ymin")))
  ymax_all   <- min(c(sapply(data_surftype$grids, "ymax"), sapply(data_dhms$elevation, "ymax")))
  
  # In case of inconsistent extent vs resolution (e.g. extent from 0 to 10
  # and resolution of 3), rast() will give priority to the resolution, and compute
  # the closest matching extent - thus, the output extent could sometimes be unexpected
  # (e.g., with extent (0,11) and resolution 3, the output extent will be (0,12)).
  # So, we compute a proper extent here, not exceeding the computed bounds.
  xmax_all   <- xmin_all + floor((xmax_all-xmin_all)/res_all)*res_all
  ymax_all   <- ymin_all + floor((ymax_all-ymin_all)/res_all)*res_all
  extent_all <- ext(xmin_all, xmax_all, ymin_all, ymax_all)
  
  # This blueprint grid will be used as reference for extent and resolution.
  raster_blueprint <- rast(ext = extent_all, resolution = res_all, crs = crs_all)
  
  # Compute final grid parameters for the current model run.
  run_params$grid_nrow       <- nrow(raster_blueprint)
  run_params$grid_ncol       <- ncol(raster_blueprint)
  run_params$grid_cell_size  <- xres(raster_blueprint)
  run_params$grid_ncells     <- run_params$grid_nrow * run_params$grid_ncol 
  
  return(list(raster_blueprint = raster_blueprint,
              run_params       = run_params))
}
