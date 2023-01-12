###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the computation of a grid of relative (normalized)           #
#                 snow distribution from elevation, and curvature, used to distribute             #
#                 the snow cover at the beginning of the modelling and after snowfall.            #
#                 The effect of avalanches is NOT included here since it is not constant          #
#                 but depends on the snow amounts.                                                #
###################################################################################################


func_compute_snowdist_topographic <- function(run_params, data_dhms, data_dems) {
  
  cat("Computing topographic snow distribution...\n")
  
  # This list has the same indices as the data_dems.
  snowdist_topographic <- list()
  
  for (dem_grid_id in 1:data_dems$n_grids) {
    
    cat("  Grid number", paste0(dem_grid_id, "...\n"))
    

    #### Curvature factor (lower accumulation on convex surfaces, higher on concave) ####
    # In the original IDL implementation, the curvature multiplication factor
    # is (1 - x), with x linearly dependent on the terrain curvature up to a cutoff:
    # curvature values at or exceeding ±threshold produce an x = ±0.5.
    # threshold is currently the smaller of the two curvature extremes (abs(max) and abs(min))
    # reduced by a tunable factor (default 1.2), so that both 1.5 and 0.5 are reached (even with some margin!)
    # in the curvature multiplication factor (arbitrary!).
    # IMPORTANT: terrain curvature in IDL is computed manually by taking cells at distance 3, 5 and 6
    # from the focal cell. This is a bit arbitrary (taken from some old ArcGIS documentation)
    # and produces a smoothed-out curvature.
    
    # We use a smoothed DHM to compute curvature because it is very sensitive to DHM noise.
    # The window size used for the smoothing is automatically computed from the smoothing amount.
    # We have to use data_dems$dhm_id[dem_grid_id] to select the DHM (see func_dhm_to_dem.R).
    dhm_smooth <- raster.gaussian.smooth(data_dhms$elevation[[data_dems$dhm_id[dem_grid_id]]],
                                         run_params$curvature_dhm_smooth,
                                         run_params$dhm_smooth_windowsize,
                                         type = mean)
    dhm_na_border <- which(is.na(values(dhm_smooth)))
    dhm_valid     <- setdiff(1:run_params$grid_ncells, dhm_na_border)
    dhm_smooth <- cover(dhm_smooth, data_dhms$elevation[[data_dems$dhm_id[dem_grid_id]]], values = NA) # Fill NA edges of smoothed raster with original values.
    dhm_curvature <- curvature(dhm_smooth, type = "total")
    dhm_curvature <- subst(dhm_curvature, NA, 0.0) # NAs can appear in curvature over flat regions.
    # Rescale curvature along the raster edges,
    # where we've just had to use the unsmoothed raster:
    # we don't want to have extreme curvature values here
    # since it is outside our region of interest.
    dhm_curvature[dhm_na_border] <- (dhm_curvature[dhm_na_border] / max(dhm_curvature[dhm_na_border]))[,1] * min(abs(range(dhm_curvature[dhm_valid])))
    
    # Compute curvature cutoff.
    dhm_curvature_bound <- min(abs(max(values(dhm_curvature), na.rm=T)), abs(min(values(dhm_curvature), na.rm=T))) / run_params$curvature_cutoff_fact
    
    # Apply corrected cutoff to curvature.
    dhm_curvature_cut <- setValues(dhm_curvature,
                                   pmin(dhm_curvature_bound, pmax(-dhm_curvature_bound, values(dhm_curvature))))
    
    # Compute final curvature factor for snow distribution.
    snowdist_curv_mult <- 1 - (dhm_curvature_cut * run_params$curvature_effect_limit / dhm_curvature_bound)
    
    
    #### Elevation factor (lower accumulation at very high elevations) ####
    snowdist_ele_mult <- setValues(data_dhms$elevation[[data_dems$dhm_id[dem_grid_id]]], 1)
    ele_scaling_fac <- (max(values(data_dhms$elevation[[data_dems$dhm_id[dem_grid_id]]]), na.rm=T) - run_params$elevation_effect_threshold) / run_params$elevation_effect_fact
    dhm_ids_high <- which(values(data_dhms$elevation[[data_dems$dhm_id[dem_grid_id]]]) > run_params$elevation_effect_threshold)
    
    snowdist_ele_mult[dhm_ids_high] <- 2 - 2^( (data_dhms$elevation[[data_dems$dhm_id[dem_grid_id]]][dhm_ids_high][,1] - run_params$elevation_effect_threshold) / ele_scaling_fac )
    
    
    #### Final distribution ####
    snowdist_topographic_cur_raw <- snowdist_curv_mult * snowdist_ele_mult
    
    # The four corners get weird values because
    # slope is not well defined there: just take
    # the next value along the diagonals (towards the center).
    snowdist_topographic_cur_raw[1] <- snowdist_topographic_cur_raw[run_params$grid_ncol + 2][,1]
    snowdist_topographic_cur_raw[run_params$grid_ncol] <- snowdist_topographic_cur_raw[2 * run_params$grid_ncol - 1][,1]
    snowdist_topographic_cur_raw[run_params$grid_ncells - run_params$grid_ncol + 1] <- snowdist_topographic_cur_raw[run_params$grid_ncells - 2 * run_params$grid_ncol + 2][,1]
    snowdist_topographic_cur_raw[run_params$grid_ncells] <- snowdist_topographic_cur_raw[run_params$grid_ncells - run_params$grid_ncol - 1][,1]
    
    # Normalize over the glacier: we assume
    # that curvature and elevation redistribution
    # have no net effect on the total snow amount
    # over the glacier.
    snowdist_topographic[[dem_grid_id]] <- snowdist_topographic_cur_raw / mean(snowdist_topographic_cur_raw[data_dems$glacier_cell_ids[[dem_grid_id]]][,1])
  
  }
  
  cat("  Finished computation of topographic snow distribution.\n")
  
  return(snowdist_topographic)
  
}
