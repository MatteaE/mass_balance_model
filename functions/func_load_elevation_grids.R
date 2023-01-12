###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for the DHM(s). As output                #
#                 we get a list of lists, with the loaded rasters, (if required) the              #
#                 interpolated rasters (within the same list, appended after the base ones), and  #
#                 an integer vector of indices showing which raster index should be used for each #
#                 year.                                                                           #
#                 NOTE: we should never modify the list elements, rather work on copies.          #
###################################################################################################


# ALGORITHM:
# If we have only one grid just load it and use it everywhere,
# else,
#   if grid_interpolate is false, for each modeled year find the closest grid year and use its grid.
#   If grid_interpolate is true, for each modeled year look for the year's grid, if you have it use it,
#     if not look for the two closest enclosing years,
#       if both found do linear interpolation,
#       if only one found (i.e. modeling a year before the earliest grid or after the last one) just use it.

func_load_elevation_grids <- function(run_params) {
  
  cat("  Loading DHMs...\n")
  
  # Here we will put the output.
  grids_out <- list(elevation = list(),
                    grid_year_id = rep(NA, run_params$n_years))
  
  cat("    Looking for DHM files...\n")
  
  run_params  <- func_find_input_files_single(run_params, "dhm")
  grid_paths  <- run_params$dhm_paths
  dhm_n       <- length(grid_paths)
  
  if (dhm_n == 0) {
    cat("** FATAL: no DHM files found. Please check parameters dir_data_dhm, filename_dhm_prefix and filename_dhm_suffix.\n")
    stop()
  } else {
    cat("    Found", dhm_n, "DHM file(s). Available year(s):", run_params$dhm_years, "\n")
  }
  
  grid_interpolate  <- run_params$dhm_interpolate
  grid_years        <- run_params$dhm_years
  
  
  # Do we have a single DHM? If so just use it every year.
  if (length(grid_years) == 1) {
    
    grids_out$elevation[[1]]      <- rast(grid_paths[1])
    crs(grids_out$elevation[[1]]) <- run_params$grids_crs_epsg
    
    for (year_cur_id in 1:run_params$n_years) {
      grids_out$grid_year_id[year_cur_id] <- 1
    }
    
    # We have more than a single DHM!
  } else {
    
    # Load base grids (their indices correspond to the grid_years vector).
    for (grid_id in 1:length(grid_paths)) {
      grids_out$elevation[[grid_id]]      <- rast(grid_paths[grid_id])
      crs(grids_out$elevation[[grid_id]]) <- run_params$grids_crs_epsg
    }
    
    
    if (grid_interpolate == FALSE) {
      
      # For each modeled year find the closest grid year and use its grid.
      # In this case, grids_out$grid_year_id is sorted (if also run_params$dhm_years was sorted.
      for (year_cur_id in 1:run_params$n_years) {
        year_cur <- run_params$years[year_cur_id]
        grid_year_closest_id <- which.min(abs(grid_years - year_cur))
        grids_out$grid_year_id[year_cur_id] <- grid_year_closest_id
      }
      
      # Here the case grid_interpolate == TRUE.
      # In this case, grids_out$grid_year_id is in general not sorted
      # (the interpolation generates new grids which are appended to
      # the list *after* the original grids).
      # Also, to interpolate we need aligned DHMs.
      # So we resample them to be aligned to the largest
      # extent and most common resolution.
    } else {
      
      # Find largest extent and most common resolution.
      xmin_all   <- min(sapply(grids_out$elevation, "xmin"))
      xmax_all   <- max(sapply(grids_out$elevation, "xmax"))
      ymin_all   <- min(sapply(grids_out$elevation, "ymin"))
      ymax_all   <- max(sapply(grids_out$elevation, "ymax"))
      extent_all <- ext(xmin_all, xmax_all, ymin_all, ymax_all)
      res_all    <- func_get_mode(sapply(grids_out$elevation, "xres"))
      crs_all    <- run_params$grids_crs_epsg
      raster_blueprint <- rast(ext = extent_all, resolution = res_all, crs = crs_all) # Used as reference for extent and resolution.
      # Resample if needed, removing NAs.
      for (grid_id in 1:length(grids_out$elevation)) {
        if (!compareGeom(grids_out$elevation[[grid_id]], raster_blueprint, stopOnError = FALSE)) {
          
          cat("* WARNING: func_load_elevation_grids.R: I am resampling DHM grid ", grid_id, " to enable DHM interpolation!\n")
          grids_out$elevation[[grid_id]] <- resample(grids_out$elevation[[grid_id]], raster_blueprint, method = "bilinear")
          crs(grids_out$elevation[[grid_id]]) <- run_params$grids_crs_epsg
          
          # Any NA in elevation is set to the mean value of the grid.
          grids_out$elevation[[grid_id]] <- subst(grids_out$elevation[[grid_id]], NA, global(grids_out$elevation[[grid_id]], fun = "mean", na.rm = TRUE))
        }
      }
      
      # For each modeled year look for a grid exactly from that year,
      # if found use it,
      # if not look for the two closest enclosing years,
      #     if both found do linear interpolation and append resulting grid to grids_out$elevation,
      #     if only one found (i.e. modeling a year before the earliest DHM or after the last one) just use it.
      for (year_cur_id in 1:run_params$n_years) {
        
        year_cur <- run_params$years[year_cur_id]
        grid_id_cur <- which(grid_years == year_cur) # Has a value only if we find a grid exactly from the current year.
        
        # Found DHM for the current year!
        # So for this year we don't need to interpolate.
        if (length(grid_id_cur) != 0) { # This should be either 0 or 1: two DHMs for a single year are not allowed.
          
          grids_out$grid_year_id[year_cur_id] <- grid_id_cur
          
          # No DHM for the current year, we have to interpolate if we can.  
        } else {
          
          year_dist <- grid_years - year_cur # Integer vector with distance in years from the year of each base input grid to the current year.
          
          # Check if we have two enclosing years, or if instead we are outside the range of the DHM years.
          if (max(year_dist) * min(year_dist) < 0) {
            # We can interpolate! Find the enclosing DHMs.
            grid_year_earlier_id <- which.max(year_dist[which(year_dist < 0)]) # year_dist is a sorted vector, so this indexing should work.
            grid_year_later_id <- grid_year_earlier_id + 1
            grid_year_earlier <- grid_years[grid_year_earlier_id]
            grid_year_later <- grid_years[grid_year_later_id]
            
            # Here interpolate between the two grids of grid_year_earlier_id and grid_year_later_id.
            # This generates a new grid, which we put at the end of the grids_out$elevation list.
            grid_earlier <- grids_out$elevation[[grid_year_earlier_id]]
            grid_later   <- grids_out$elevation[[grid_year_later_id]]
            
            grid_interpolated <- grid_earlier + (grid_later - grid_earlier) * (year_cur - grid_year_earlier) / (grid_year_later - grid_year_earlier)
            crs(grid_interpolated) <- run_params$grids_crs_epsg
            grids_out$elevation[[length(grids_out$elevation) + 1]] <- grid_interpolated
            grids_out$grid_year_id[year_cur_id] <- length(grids_out$elevation)
            
            # Else: we are outside the range of the DHM years. Just take the
            # closest grid (which will be either the earliest or the most recent).
          } else {
            grid_year_closest_id <- which.min(abs(year_dist))
            grids_out$grid_year_id[year_cur_id] <- grid_year_closest_id
          }
        }
      }
    }
  }
  
  grids_out$n_grids <- length(grids_out$elevation)
  
  return(grids_out)
  
}
