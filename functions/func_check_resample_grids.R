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
  
  cat("  Checking common extent of all grids...\n")
  
  # Resample grids if needed, remove NAs.
  for (grid_id in 1:length(data_all$data_surftype$grids)) {
    if (!compareGeom(data_all$data_surftype$grids[[grid_id]],
                     data_all$raster_blueprint,
                     res = TRUE,
                     stopOnError = FALSE)) {
      
      
      ext1 <- ext(data_all$data_surftype$grids[[grid_id]])
      ext2 <- ext(data_all$raster_blueprint)
      func_customlog("Resampling surface type grid ", grid_id, " to match the common extent.", level = 1)
      func_customlog("Left        ", sprintf("%11.3f", ext1[1]), " --> ", sprintf("%11.3f", ext2[1]), " (", sprintf("%+.3f", ext2[1] - ext1[1]), ")", level = 0)
      func_customlog("Right       ", sprintf("%11.3f", ext1[2]), " --> ", sprintf("%11.3f", ext2[2]), " (", sprintf("%+.3f", ext2[2] - ext1[2]), ")", level = 0)
      func_customlog("Bottom      ", sprintf("%11.3f", ext1[3]), " --> ", sprintf("%11.3f", ext2[3]), " (", sprintf("%+.3f", ext2[3] - ext1[3]), ")", level = 0)
      func_customlog("Top         ", sprintf("%11.3f", ext1[4]), " --> ", sprintf("%11.3f", ext2[4]), " (", sprintf("%+.3f", ext2[4] - ext1[4]), ")", level = 0)
      func_customlog("Resolution  ", sprintf("%11.3f", xres(data_all$data_surftype$grids[[grid_id]])), " --> ", sprintf("%11.3f", xres(data_all$raster_blueprint)), level = 0)
      
      data_all$data_surftype$grids[[grid_id]]      <- resample(data_all$data_surftype$grids[[grid_id]], data_all$raster_blueprint, method = "near")
      crs(data_all$data_surftype$grids[[grid_id]]) <- run_params$grids_crs_epsg
      
      # Any NA in surface type becomes rock.
      data_all$data_surftype$grids[[grid_id]] <- subst(data_all$data_surftype$grids[[grid_id]], NA, 4)
    }
  }
  for (grid_id in 1:length(data_all$data_dhms$elevation)) {
    if (!compareGeom(data_all$data_dhms$elevation[[grid_id]],
                     data_all$raster_blueprint,
                     res = TRUE,
                     stopOnError = FALSE)) {
      
      
      ext1 <- ext(data_all$data_dhms$elevation[[grid_id]])
      ext2 <- ext(data_all$raster_blueprint)
      func_customlog("Resampling DHM grid ", grid_id, " to match the common extent.", level = 1)
      func_customlog("Left        ", sprintf("%11.3f", ext1[1]), " --> ", sprintf("%11.3f", ext2[1]), " (", sprintf("%+.3f", ext2[1] - ext1[1]), ")", level = 0)
      func_customlog("Right       ", sprintf("%11.3f", ext1[2]), " --> ", sprintf("%11.3f", ext2[2]), " (", sprintf("%+.3f", ext2[2] - ext1[2]), ")", level = 0)
      func_customlog("Bottom      ", sprintf("%11.3f", ext1[3]), " --> ", sprintf("%11.3f", ext2[3]), " (", sprintf("%+.3f", ext2[3] - ext1[3]), ")", level = 0)
      func_customlog("Top         ", sprintf("%11.3f", ext1[4]), " --> ", sprintf("%11.3f", ext2[4]), " (", sprintf("%+.3f", ext2[4] - ext1[4]), ")", level = 0)
      func_customlog("Resolution  ", sprintf("%11.3f", xres(data_all$data_dhms$elevation[[grid_id]])), " --> ", sprintf("%11.3f", xres(data_all$raster_blueprint)), level = 0)
      
      data_all$data_dhms$elevation[[grid_id]]      <- resample(data_all$data_dhms$elevation[[grid_id]], data_all$raster_blueprint, method = "bilinear")
      crs(data_all$data_dhms$elevation[[grid_id]]) <- run_params$grids_crs_epsg
      
      # Any NA in elevation (potentially present e.g. on the borders
      # after resampling) is set to the mean value of the grid.
      data_all$data_dhms$elevation[[grid_id]] <- subst(data_all$data_dhms$elevation[[grid_id]], NA, global(data_all$data_dhms$elevation[[grid_id]], fun = "mean", na.rm = TRUE))
    }
  }
  
  return(data_all)
}
