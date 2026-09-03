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
    func_customlog("No DHM files found. Please check parameters dir_data_dhm, filename_dhm_prefix and filename_dhm_suffix.", level = 2)
    func_stop()
  } else {
    cat("    Found", dhm_n, "DHM file(s). Available year(s):", run_params$dhm_years, "\n")
  }
  
  grid_interpolate  <- run_params$dhm_interpolate
  grid_years        <- run_params$dhm_years
  
  
  # Do we have a single DHM? If so just use it every year.
  if (length(grid_years) == 1) {
    
    tryCatch({grids_out$elevation[[1]] <- rast(grid_paths[1])},
             error = function(err) {
               func_customlog("Error reading elevation grid: ", grid_paths[1], level = 2)
               func_stop()
             })
    if (nchar(crs(grids_out$elevation[[1]])) == 0) {
      crs(grids_out$elevation[[1]]) <- run_params$grids_crs_epsg
    } else {
      if (!same.crs(crs(grids_out$elevation[[1]]),
                    run_params$grids_crs_epsg)) {
        func_customlog("The CRS of elevation grid ", grid_paths[1], " does not match the provided global CRS parameter. Please check.", level = 2)
        func_stop()
      }
    } # End else: the grid already had a CRS.
    
    
    
    for (year_cur_id in 1:run_params$n_years) {
      grids_out$grid_year_id[year_cur_id] <- 1
    }
    
    # We have more than a single DHM!
  } else {
    
    # Load base grids (their indices correspond to the grid_years vector).
    for (grid_id in 1:length(grid_paths)) {
      
      tryCatch({grids_out$elevation[[grid_id]] <- rast(grid_paths[grid_id])},
               error = function(err) {
                 func_customlog("Error reading elevation grid: ", grid_paths[grid_id], level = 2)
                 func_stop()
               })
      if (nchar(crs(grids_out$elevation[[grid_id]])) == 0) {
        crs(grids_out$elevation[[grid_id]]) <- run_params$grids_crs_epsg
      } else {
        if (!same.crs(crs(grids_out$elevation[[grid_id]]),
                      run_params$grids_crs_epsg)) {
          func_customlog("The CRS of elevation grid ", grid_paths[grid_id], " does not match the provided global CRS parameter. Please check.", level = 2)
          func_stop()
        }
      } # End else: the grid already had a CRS.
      
    } # End loop loading of DHM grids from files.
    
    
    if (grid_interpolate == FALSE) {
      
      cat("    Selecting closest DHM grid for each year...\n")
      
      # For each modeled year find the closest grid year and use its grid.
      # In this case, grids_out$grid_year_id is sorted (if also run_params$dhm_years was sorted).
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
      
      cat("    Interpolating DHM grids to all years...\n")
      
      # Find largest extent and most common resolution.
      xmin_all   <- min(sapply(grids_out$elevation, "xmin"))
      xmax_all   <- max(sapply(grids_out$elevation, "xmax"))
      ymin_all   <- min(sapply(grids_out$elevation, "ymin"))
      ymax_all   <- max(sapply(grids_out$elevation, "ymax"))
      extent_all <- ext(xmin_all, xmax_all, ymin_all, ymax_all)
      res_all    <- func_get_mode(sapply(grids_out$elevation, "xres"))
      crs_all    <- run_params$grids_crs_epsg
      raster_blueprint <- rast(ext = extent_all, resolution = res_all, crs = crs_all) # Used as reference for extent and resolution.
      # Resample if needed, removing NAs (which may appear at
      # borders from resampling grids with different extents).
      for (grid_id in 1:length(grids_out$elevation)) {
        if (!compareGeom(grids_out$elevation[[grid_id]],
                         raster_blueprint,
                         res = TRUE,
                         stopOnError = FALSE,
                         tolerance = 1e-6)) {
          
          ext1 <- ext(grids_out$elevation[[grid_id]])
          ext2 <- ext(raster_blueprint)
          func_customlog("Resampling DHM grid ", grid_id, " to enable DHM interpolation.", level = 1)
          func_customlog("Left        ", sprintf("%11.3f", ext1[1]), " --> ", sprintf("%11.3f", ext2[1]), " (", sprintf("%+.3f", ext2[1] - ext1[1]), ")", level = 0)
          func_customlog("Right       ", sprintf("%11.3f", ext1[2]), " --> ", sprintf("%11.3f", ext2[2]), " (", sprintf("%+.3f", ext2[2] - ext1[2]), ")", level = 0)
          func_customlog("Bottom      ", sprintf("%11.3f", ext1[3]), " --> ", sprintf("%11.3f", ext2[3]), " (", sprintf("%+.3f", ext2[3] - ext1[3]), ")", level = 0)
          func_customlog("Top         ", sprintf("%11.3f", ext1[4]), " --> ", sprintf("%11.3f", ext2[4]), " (", sprintf("%+.3f", ext2[4] - ext1[4]), ")", level = 0)
          func_customlog("Resolution  ", sprintf("%11.3f", xres(grids_out$elevation[[grid_id]])), " --> ", sprintf("%11.3f", xres(raster_blueprint)), level = 0)
          
          
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
            
            grid_interpolated      <- grid_earlier + (grid_later - grid_earlier) * (year_cur - grid_year_earlier) / (grid_year_later - grid_year_earlier)
            crs(grid_interpolated) <- run_params$grids_crs_epsg
            grids_out$elevation[[length(grids_out$elevation) + 1]] <- grid_interpolated
            grids_out$grid_year_id[year_cur_id] <- length(grids_out$elevation)
            
            # Else: we are outside the range of the DHM years. Just take the
            # closest grid (which will be either the earliest or the most recent).
          } else {
            grid_year_closest_id <- which.min(abs(year_dist))
            grids_out$grid_year_id[year_cur_id] <- grid_year_closest_id
          }
        } # End else: we had to interpolate for the current year
      } # End loop on modeled years
    } # End else: grid_interpolate was TRUE
  } # End else: we had more than one DHm grid file
  
  grids_out$n_grids <- length(grids_out$elevation)
  
  return(grids_out)
  
}
