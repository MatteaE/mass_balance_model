###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to resample grids to a common blueprint grid     #
#                 in case the extent/origin/resolution do not match.                              #
###################################################################################################   

# We want grids that are all aligned, because later we will work
# with their cell values only.
# We align everything to the largest possible extent
# (union of the grids), and to the most common resolution
# found in the input grids.
func_check_resample_grids <- function(run_params,
                                      data_all) {
  
  # Resample grids if needed, remove NAs.
  for (grid_id in 1:length(data_all$data_surftype$grids)) {
    if (!compareGeom(data_all$data_surftype$grids[[grid_id]], data_all$raster_blueprint, stopOnError = FALSE)) {
      
      cat("* WARNING: func_check_resample_grids: I am resampling surface type grid ", grid_id, "!\n")
      data_all$data_surftype$grids[[grid_id]]      <- resample(data_all$data_surftype$grids[[grid_id]], data_all$raster_blueprint, method = "near")
      crs(data_all$data_surftype$grids[[grid_id]]) <- run_params$grids_crs_epsg
      
      # Any NA in surface type becomes rock.
      data_all$data_surftype$grids[[grid_id]] <- subst(data_all$data_surftype$grids[[grid_id]], NA, 4)
    }
  }
  for (grid_id in 1:length(data_all$data_dhms$elevation)) {
    if (!compareGeom(data_all$data_dhms$elevation[[grid_id]], data_all$raster_blueprint, stopOnError = FALSE)) {
      
      cat("* WARNING: func_check_resample_grids: I am resampling DHM grid ", grid_id, "!\n")
      data_all$data_dhms$elevation[[grid_id]]      <- resample(data_all$data_dhms$elevation[[grid_id]], data_all$raster_blueprint, method = "bilinear")
      crs(data_all$data_dhms$elevation[[grid_id]]) <- run_params$grids_crs_epsg
      
      # Any NA in elevation (potentially present e.g. on the borders
      # after resampling) is set to the mean value of the grid.
      data_all$data_dhms$elevation[[grid_id]] <- subst(data_all$data_dhms$elevation[[grid_id]], NA, global(data_all$data_dhms$elevation[[grid_id]], fun = "mean", na.rm = TRUE))
    }
  }
  
  return(data_all)
}
