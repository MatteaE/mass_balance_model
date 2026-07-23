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
# the grids loaded from the boot file and
# skip loading all others, if they don't match
# then reload all radiation grids from individual files
# and save boot file for next time.
func_load_radiation_grids <- function(run_params,
                                      raster_blueprint) {
  
  cat("  Loading radiation grids...\n")
  
  if (!dir.exists(run_params$dir_data_radiation)) {
    func_customlog("Radiation folder does not exist: ", run_params$dir_data_radiation, level = 2)
    func_stop()
  }
  
  rad_abspath <- normalizePath(run_params$dir_data_radiation)
  
  # This will be TRUE only if the boot file is available
  # AND the first grid from the boot file matches (within 1e-5)
  # the first grid from the individual grid files.
  skip_loading_logi <- FALSE
  
  # Path to potentially available boot file
  # (much quicker to load).
  radiation_boot_file_path <- file.path(rad_abspath, "radiation_grids.RData")
  
  # 365 paths to radiation grid files.
  # We allow different extensions.
  grid_exts             <- c(".tif", ".grid", ".asc")
  grid_files            <- list.files(rad_abspath,
                                      pattern = paste0("^",
                                                       run_params$filename_radiation_prefix,
                                                       "[0-9]{3}",
                                                       run_params$filename_radiation_suffix,
                                                       "(\\", paste0(grid_exts, collapse = "$)|(\\"), "$)"))
  
  grid_exts_found <- unique(sapply(strsplit(grid_files, ".", fixed = T), `[[`, 2))
  if (length(grid_exts_found) > 1) {
    func_customlog("Inconsistent file types found in the radiation folder: ", rad_abspath, ". Please check it.\n", level = 2)
    func_stop()
  }
  if (length(grid_files) != 365) {
    if (length(grid_files) == 0) {
      func_customlog("No radiation grids found in the radiation folder: ", rad_abspath, ". Please check parameters dir_data_radiation, filename_radiation_prefix and filename_radiation_suffix.\n", level = 2)
      func_stop()
    } else {
      func_customlog("Expected 365 radiation files in the radiation folder: ", rad_abspath, ". Found ", length(grid_files), " instead. This is unexpected, please check.\n", level = 2)
      func_stop()
    }
  }
  
  
  
  grid_paths <- file.path(rad_abspath,
                          grid_files)
  
  
  # Do we have an RData file to speed up loading of radiation grids?
  # If so, use it if possible!
  if (file.exists(radiation_boot_file_path)) {
    
    cat("    Radiation boot file found! Checking first grid...\n")
    load(radiation_boot_file_path)
    
    tryCatch({grid_day1 <- rast(grid_paths[1])},
             error = function(err) {
               func_customlog("Error reading radiation grid: ", grid_paths[1], level = 2)
               func_stop()
             })
    
    if (!compareGeom(grid_day1,
                     raster_blueprint,
                     res = TRUE,
                     stopOnError = FALSE)) {
      
      ext1 <- ext(grid_day1)
      ext2 <- ext(raster_blueprint)
      func_customlog("Resampling the first radiation grid to common extent, to check the boot file.", level = 1)
      func_customlog("Left        ", sprintf("%11.3f", ext1[1]), " --> ", sprintf("%11.3f", ext2[1]), " (", sprintf("%+.3f", ext2[1] - ext1[1]), ")", level = 0)
      func_customlog("Right       ", sprintf("%11.3f", ext1[2]), " --> ", sprintf("%11.3f", ext2[2]), " (", sprintf("%+.3f", ext2[2] - ext1[2]), ")", level = 0)
      func_customlog("Bottom      ", sprintf("%11.3f", ext1[3]), " --> ", sprintf("%11.3f", ext2[3]), " (", sprintf("%+.3f", ext2[3] - ext1[3]), ")", level = 0)
      func_customlog("Top         ", sprintf("%11.3f", ext1[4]), " --> ", sprintf("%11.3f", ext2[4]), " (", sprintf("%+.3f", ext2[4] - ext1[4]), ")", level = 0)
      func_customlog("Resolution  ", sprintf("%11.3f", xres(grid_day1)), " --> ", sprintf("%11.3f", xres(raster_blueprint)), level = 0)
      
      grid_day1 <- resample(grid_day1, raster_blueprint, method = "bilinear")
    }
    grid_day1_val <- values(grid_day1)
    grid_day1_val[is.na(grid_day1_val)] <- 0
    
    # If first grid from boot file and from grid files
    # is the same, then skip loading the other grids
    # and keep the ones we have from the boot file.
    if (length(grid_day1_val) == length(grids_out[[1]])) {
      if (all(abs(grid_day1_val - grids_out[[1]]) < 1e-3)) {
        cat("    The first grid matches! We can use the boot file.\n")
        skip_loading_logi <- TRUE
      } else {
        func_customlog("    The first grid has the same number of cells but the values do NOT match. Reloading the individual files.", level = 1)
      }
    } else {
      func_customlog("    The first grid does not match, it even has a different number of cells. Reloading the individual files.", level = 1)
    }
  }
  
  if (!skip_loading_logi) {
    
    # Here we will put the output.
    grids_out <- list()
    
    # Actual loading happens here.
    # We resample grids on the fly if needed.
    for (doy in 1:365) {
      
      cat("\r    Loading daily radiation files...", doy, "/", 365)
      tryCatch({ras_cur <- rast(grid_paths[doy])},
               error = function(err) {
                 func_customlog("Error reading radiation grid: ", grid_paths[doy], level = 2)
                 func_stop()
               })
      
      if ((ext(ras_cur) != ext(raster_blueprint)) || (xres(ras_cur) != xres(raster_blueprint))) {
        # cat("\nResampling radiation grid!")
        ras_cur <- resample(ras_cur, raster_blueprint, method = "bilinear")
      }
      grids_out[[doy]] <- values(ras_cur)
      
      # We don't want any NAs in the radiation (they can arise when resampling to larger extent).
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
