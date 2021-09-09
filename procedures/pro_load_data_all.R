###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which calls all the data loading sub-routines.   #
#                 The DEM (= elevation model cropped to the glacier) is computed here, from       #
#                 DHM (= full elevation model with no gaps) and outline.                          #
###################################################################################################  

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

#### Load input data from sources or reboot file ####
if (boot_file_read) {
  
  load(boot_file_name)
  
} else {
  
  data_weather               <-   func_load_weather(run_params)
  data_surftype              <-   func_load_surftype_grids(run_params)
  data_outlines              <-   func_load_outlines(run_params)
  data_dhms                  <-   func_load_elevation_grids(run_params)
  
  # Check and if needed resample the elevation / surface type grids:
  # we want grids that are all aligned, because later we will work
  # with their values only.
  # We align everything to the largest possible extent
  # (union of the grids), and to the most common resolution
  # found in the input grids.
  # Find largest extent and most common resolution.
  xmin_all   <- min(c(sapply(data_surftype$grids, "xmin"), sapply(data_dhms$elevation, "xmin")))
  xmax_all   <- max(c(sapply(data_surftype$grids, "xmax"), sapply(data_dhms$elevation, "xmax")))
  ymin_all   <- min(c(sapply(data_surftype$grids, "ymin"), sapply(data_dhms$elevation, "ymin")))
  ymax_all   <- max(c(sapply(data_surftype$grids, "ymax"), sapply(data_dhms$elevation, "ymax")))
  extent_all <- extent(xmin_all, xmax_all, ymin_all, ymax_all)
  res_all    <- func_get_mode(c(sapply(data_surftype$grids, "xres"), sapply(data_dhms$elevation, "xres")))
  raster_blueprint <- raster(ext = extent_all, resolution = res_all) # Used as reference for extent and resolution.
  
  # Resample grids if needed, remove NAs.
  for (grid_id in 1:length(data_surftype$grids)) {
    if (!compareRaster(data_surftype$grids[[grid_id]], raster_blueprint, stopiffalse = FALSE)) {
      
      message("WARNING: pro_load_data_all.R: I am resampling surface type grid ", grid_id, "!")
      data_surftype$grids[[grid_id]] <- resample(data_surftype$grids[[grid_id]], raster_blueprint, method = "ngb")
      crs(data_surftype$grids[[grid_id]]) <- run_params$grids_crs
      
      # Any NA in surface type becomes rock.
      data_surftype$grids[[grid_id]][is.na(data_surftype$grids[[grid_id]][])] <- 4
    }
  }
  for (grid_id in 1:length(data_dhms$elevation)) {
    if (!compareRaster(data_dhms$elevation[[grid_id]], raster_blueprint, stopiffalse = FALSE)) {
      
      message("WARNING: pro_load_data_all.R: I am resampling DHM grid ", grid_id, "!")
      data_dhms$elevation[[grid_id]] <- resample(data_dhms$elevation[[grid_id]], raster_blueprint, method = "bilinear")
      crs(data_dhms$elevation[[grid_id]]) <- run_params$grids_crs
      
      # Any NA in elevation is set to the mean value of the grid.
      data_dhms$elevation[[grid_id]][is.na(data_dhms$elevation[[grid_id]][])] <- cellStats(data_dhms$elevation[[grid_id]], stat = "mean", na.rm = TRUE)
    }
  }
  
  data_dems                  <-   func_dhm_to_dem(run_params, data_dhms, data_outlines)
  data_radiation             <-   func_load_radiation_grids(run_params, raster_blueprint)
  data_massbalance_annual    <-   func_load_massbalance_measurements(run_params, "annual")
  data_massbalance_winter    <-   func_load_massbalance_measurements(run_params, "winter")
}
