###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the adaptive IDW interpolation of snow probing data,         #
#                 which imitates the original IDL implementation (lower clamping, progressively   #
#                 grown search radius towards having a minimum number of points.                  #
###################################################################################################


func_snow_probes_idw_adaptive <- function(snow_probes_df,
                                          ref_grid,
                                          run_params) {
  
  crds_m <- crds(ref_grid, na.rm = FALSE)
  x_grid <- setValues(ref_grid, crds_m[,1])
  y_grid <- setValues(ref_grid, crds_m[,2])
  
  # Multi-band raster, one band per cell
  r1 <- sqrt((x_grid - snow_probes_df$x)^2 + (y_grid - snow_probes_df$y)^2)
  
  # We progressively enlarge the search radius
  # until the grid can be fully computed,
  # then we merge the grids by prioritizing
  # values computed with smaller search radii.
  # This is a fast, vectorized version of the
  # original IDL algorithm.
  idw_result_cur    <- setValues(ref_grid,
                                 NA_real_)
  
  idw_results_all   <- list(idw_result_cur)
  iter_max_n <- 100
  iter_id <- 1
  cat("  Starting adaptive IDW interpolation...\n")
  while ((anyNA(values(idw_results_all[[iter_id]], mat = F))) && (iter_id < iter_max_n)) {
    
    iter_id <- iter_id + 1
    
    search_radius_cur <- run_params$probes_snowdist_search_radius_init * (iter_id - 1)
    
    # Distance raster (one band per snow point),
    # clamped to minimum distance (constant value below that)
    # and current search radius (NA above that).
    r2 <- clamp(clamp(r1, lower = run_params$probes_snowdist_dist_min, upper = Inf, values = TRUE),
                lower = -Inf, upper = search_radius_cur, values = FALSE)
    
    # Count of points available for each cell (single band).
    r3 <- sum(r2/r2, na.rm = TRUE)
    
    # Same as previous but after lower thresholding (single band)
    r4 <- clamp(r3, lower = run_params$probes_snowdist_search_npoints_min, upper = Inf, values = FALSE)
    
    # IDW weight of each point (one band per snow point).
    r5 <- 1/(r2^run_params$probes_snowdist_idw_exp)
    
    # Sum of weights, for normalization.
    r6 <- sum(r5, na.rm = TRUE)
    
    # Weighted average, not yet masked.
    r7 <- sum(snow_probes_df$swe * r5, na.rm = T) / r6
    
    # Mask to enforce minimnum number of points
    idw_results_all[[iter_id]] <- mask(r7, r4)
    
  }
  
  if ((iter_id == iter_max_n) && (anyNA(values(idw_results_all[[iter_id]], mat = F)))) {
    func_customlog("Adaptive IDW interpolation of winter snow probes hit the maximum iteration count. Please check manually.", level = 2)
    func_stop()
  }
  
  snowdist_idw <- cover(rast(idw_results_all))
  
  if (anyNA(values(snowdist_idw, mat = F))) {
    func_customlog("Adaptive IDW interpolation of winter snow probes failed. Please check manually.", level = 2)
    func_stop()
  }
  
  cat("  Adaptive IDW interpolation finished after", iter_id, "iterations.\n")
  
  return(snowdist_idw)
}
