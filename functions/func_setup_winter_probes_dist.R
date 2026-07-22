###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to setup the grid of snow distribution for the      #
#                 current year.                                                                   #
#                 This grid is loaded from file if instructed to do so, otherwise calculated      #
#                 from winter measurements, or set to a constant grid if those do not exist.      #
###################################################################################################


func_setup_winter_probes_dist <- function(year_data,
                                          data_dhms,
                                          data_dems,
                                          data_outlines,
                                          run_params,
                                          year_cur_params) {
  
  
  # Should we make a winter run to optimize the precipitation correction?
  # Only if we have some measurements of winter snow cover, else we can't.
  # NOTE: the winter period will be run (optimizing precipitation correction)
  #       even if there are no corresponding annual measurements. This is ok
  #       but a bit strange (melt model will be uncalibrated except if there are
  #       other years with data and nodata_years_automatic is set to TRUE).
  year_data$process_winter <- (year_data$nstakes_winter > 0)
  
  
  
  # Did the user instruct to use an external map of snow distribution?
  # If yes, try to load it and project it to the current DHM grid.
  if (nchar(year_cur_params$probes_snowdist_filename) > 0) {
    
    cat("Attempting to load user-defined map of snow distribution for the current year...\n")
    
    # Check if file exists and can be opened, fail gracefully if not.
    probes_fp <- file.path(normalizePath(dirname(run_params$dir_data_snowdist)), basename(run_params$dir_data_snowdist), year_cur_params$probes_snowdist_filename)
    if (!file.exists(probes_fp)) {
      func_customlog("User-defined map of snow distribution does not exist: ", probes_fp, level = 2)
      func_stop()
    }
    tryCatch({
      dist_probes_raw_r <- rast(probes_fp)
    },
    error = function(err) {
      func_customlog("Error reading user-defined map of snow distribution: ", probes_fp, level = 2)
      func_stop()
    })
    
    dist_probes_r <- project(dist_probes_raw_r,
                             data_dhms$elevation[[year_data$dhm_grid_id]],
                             method = "bilinear")
    
    if (any(is.na(values(dist_probes_r, mat = F)))) {
      func_customlog("Resampled map of snow distribution has NA values. They will be replaced with 1, but check carefully the input maps.", level = 1)
      dist_probes_r <- subst(dist_probes_r, NA, 1.0)
    }
    
    
    
    # Else: was not provided an external map of snow distribution.
  } else {
    
    # Do we have winter measurements?
    # If yes, make a map of snow distribution out of them.
    if (year_data$nstakes_winter > 0) {
      
      cat("Calculating map of snow distribution from winter data...\n")
      dist_probes_raw_r <- func_snowdist_from_probes(year_data, run_params, data_dhms)
      
      # Normalize to arithmetic average = 1 on glacier.
      dist_probes_norm_r <- dist_probes_raw_r / mean(dist_probes_raw_r[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]][,1])
      
      # Apply a smooth mask to transition to uniform (1.0) distribution away from glacier
      # (the snow probes do not give information on what is happening away from the glacier;
      # this only has a second-order effect via the avalanches).
      # We leave the probes distribution untouched within 100 m of the glacier outline.
      # We then arrive at uniform distribution 500 m away from the glacier outline.
      # "od" = "offglacier_distance"
      od_th1 <- 100
      od_th2 <- 500
      offglacier_dist <- distance(dist_probes_norm_r, vect(data_outlines$outlines[[year_data$outline_id]]))
      od1 <- classify(offglacier_dist, rbind(c(-Inf, od_th1, od_th1),
                                             c(od_th2, Inf, od_th2)))
      
      # This is the weight of the constant value we use (1.0):
      # 1.0 farther than od_th2, 0.0 (no weight) within od_th1,
      # sinusoidally interpolated in between.
      od2 <- sin((od1 - od_th1) * (pi/2) / (od_th2 - od_th1))
      dist_probes_r <- 1.0 * od2 + dist_probes_norm_r * (1-od2)
      
      # If not, apply uniform snow distribution (for large-scale
      # variability only - then there is still the topographic distribution!)    
    } else {
      
      cat("No winter data to calculate map of large-scale snow variability - using uniform distribution.\n")
      dist_probes_r <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]], 1.0)
    }
    
  } # End else was not provided an external map of snow distribution.
  
  # Here we do have a map of snow distribution in dist_probes_r.
  
  
  # Here we possibly reduce variability of large-scale variability from winter probes
  # (probes_snowdist_fact < 1; it makes sense if there are probes affected by avalanches).
  dist_probes_mean <- mean(values(dist_probes_r, mat = F))
  if (is.na(dist_probes_mean)) {
    func_customlog("There are still NA values in the map of snow distribution, please investigate!", level = 2)
    func_stop()
  }
  if ((is.na(run_params$probes_snowdist_fact)) || (run_params$probes_snowdist_fact < 0)) {
    func_customlog("Parameter probes_snowdist_fact must be >= 0. Provided value: ", run_params$probes_snowdist_fact, level = 2)
    func_stop()
  }
  dist_probes_red_r <- dist_probes_mean + run_params$probes_snowdist_fact * (dist_probes_r - dist_probes_mean)
  
  # Normalize again to arithmetic average = 1 on glacier.
  dist_probes_norm_red_r <- dist_probes_red_r / mean(dist_probes_red_r[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]][,1])
  
  year_data$dist_probes_norm_values_red <- values(dist_probes_norm_red_r, mat = FALSE)
  
  cat("Snow distribution map is ready.\n")
  
  return(year_data)
  
}
