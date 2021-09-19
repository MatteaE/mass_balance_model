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

# Algorithm:
# look for available radiation boot file,
# if it's there then load it,
# then load the first radiation grid anyway
# and compare it to the corresponding grid
# in the boot file; if they match then keep
# the grids loaded from the booth file and
# skip loading all others, if they don't match
# then reload all radiation grids from individual files
# and save boot file for next time.
func_load_radiation_grids <- function(run_params, raster_blueprint) {
  
  cat("  Loading radiation grids...\n")
  
  # This will be TRUE only if the boot file is available
  # AND the first grid from the boot file matches (within 1e-5)
  # the first grid from the individual grid files.
  skip_loading_logi <- FALSE
  
  # Path to potentially available boot file
  # (much quicker to load).
  radiation_boot_file_path <- file.path(run_params$dir_data_radiation, "radiation_grids.RData")
  
  # 365 paths to radiation grid files.
  grid_paths <- file.path(run_params$dir_data_radiation,
                          paste0(run_params$filename_radiation_prefix,
                                 sprintf("%03d", 1:365),
                                 run_params$filename_radiation_suffix))
  
  # Do we have an RData file to speed up loading of radiation grids?
  # If so, use it!
  if (file.exists(radiation_boot_file_path)) {
    
    cat("    Radiation boot file found! Checking first grid...\n")
    
    load(radiation_boot_file_path)
    grid_day1 <- raster(grid_paths[1])
    if ((extent(grid_day1) != extent(raster_blueprint)) || (xres(grid_day1) != xres(raster_blueprint))) {
      grid_day1 <- resample(grid_day1, raster_blueprint, method = "bilinear")
    }
    grid_day1_val <- getValues(grid_day1)
    grid_day1_val[is.na(grid_day1_val)] <- 0
    
    # If first grid from boot file and from grid files
    # is the same, then skip loading the other grids
    # and keep the ones we have from the boot file.
    if (all(abs(grid_day1_val - grids_out[[1]]) < 1e-5)) {
      cat("    First grid matches! We can use the boot file.\n")
      skip_loading_logi <- TRUE
    } else {
      cat("    First grid does NOT match. I am reloading the individual files.\n")
    }
  }
  
  if (!skip_loading_logi) {
    
    # Here we will put the output.
    grids_out <- list()
    
    # Actual loading happens here.
    # We resample grids on the fly if needed.
    for (doy in 1:365) {
      
      cat("\r    Loading daily radiation files...", doy, "/", 365)
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
    
    cat("    Saving radiation boot file for faster loading next time...\n")
    
    # Save radiation boot file to speed up next model run.
    save(grids_out, file = radiation_boot_file_path)
  }
  
  cat("    Radiation grids are ready.\n")
  return(grids_out)
  
}
