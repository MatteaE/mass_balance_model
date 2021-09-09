###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the preprocessing of the elevation model to make it          #
#                 suitable for the avalanche redistribution model.                                #
#                 Specifically, there should not be any sinks when considering connectivity       #
#                 with the 4 closest neighbors (rook's case), and there should be no adjacent     #
#                 cells with the same elevation.                                                  #
###################################################################################################

func_elevation_preprocess <- function(run_params, elevation) {
  
  #### REMOVE FLAT PATCHES ####
  # Find flat patches and replace them with smoothed DEM.
  # We iterate while we enlargen the smoothing window and amount,
  # so that even large flat patches (lakes!) will eventually disappear.
  elevation_unpatched <- elevation # elevation_unpatched will be the output.
  ids_patch_flat <- func_find_flat_patches(elevation, run_params)
  elevation_mean <- mean(getValues(elevation_unpatched), na.rm = T) # To add padding at the DEM borders with a value not too far from the DEM itself.
  n_flat_iter <- 1
  
  while (length(ids_patch_flat) > 0) {
    
    smoothing_mat <- gaussian.kernel(sigma = n_flat_iter, n = max(5, 2 * n_flat_iter + 1))
    elevation_smoothed <- focal(elevation_unpatched, w = smoothing_mat, fun = sum, na.rm = TRUE, pad = TRUE, padValue = elevation_mean)
    elevation_unpatched[ids_patch_flat] <- elevation_smoothed[ids_patch_flat]
    n_flat_iter <- n_flat_iter + 1
    ids_patch_flat <- func_find_flat_patches(elevation_unpatched, run_params) # Check again for any flat patches left.
    
  }
  
  cat("    All flat patches gone after", n_flat_iter-1, "iteration(s).\n")
  
  # writeRaster(elevation_unpatched, "1-elevation-unpatched.tif", overwrite = T)
  
  
  #### FILL SINKS ####
  # First we fill the cells which are sinks on a 8-connectivity grid, with topmodel::sinkfill().
  # Unfortunately that algorithm ignores the 4-connectivity sinks (cells which drain diagonally
  # but not on the 4 closest neighbors).
  # So we find those 4-sinks with focal(), by checking who is lowest in the 4-neighborhood,
  # and we remove those sinks by raising them to the mean of the 4-neighbors.
  # This might stop drainage of some other cell, so we repeat topmodel::sinkfill(),
  # and we iterate until there are no sinks of any kind left.
  invisible(capture.output(elevation_filled <- setValues(elevation_unpatched,    # elevation_filled will be the output.
                                                         topmodel::sinkfill(as.matrix(elevation_unpatched),
                                                                            res = xres(elevation_unpatched),
                                                                            degree = 0.5))))
  
  elevation_filled_focal_min <- focal(elevation_filled, w = rbind(c(Inf,1,Inf),c(1,1,1),c(Inf,1,Inf)), fun = min)
  ids_sink_4neighbors <- which((elevation_filled < elevation_filled_focal_min + 0.01)[])
  sinkfill_iter_count <- 1
  
  while (length(ids_sink_4neighbors) > 0) {
    
    # cat("    Iteration", sinkfill_iter_count, "to fill all sinks...\n")
    
    # Raise isolated 4-connectivity sinks to the mean of the 4-neighbors.
    elevation_filled_mean_nofocal <- focal(elevation_filled, w = rbind(c(0,1/4,0),c(1/4,0,1/4),c(0,1/4,0)))
    elevation_filled[ids_sink_4neighbors] <- elevation_filled_mean_nofocal[ids_sink_4neighbors]
    # Fill again in case we have created new sinks.
    invisible(capture.output(elevation_filled <- setValues(elevation_filled, topmodel::sinkfill(as.matrix(elevation_filled), res = xres(elevation_filled), degree = 0.5))))
    # Look again for 4-connectivity sinks.
    elevation_filled_focal_min <- focal(elevation_filled, w = rbind(c(Inf,1,Inf),c(1,1,1),c(Inf,1,Inf)), fun = min)
    ids_sink_4neighbors <- which((elevation_filled < elevation_filled_focal_min + 0.01)[])
    sinkfill_iter_count <- sinkfill_iter_count + 1
    
  }
  
  cat("    All sinks gone after", sinkfill_iter_count-1, "iteration(s).\n")
  
  dem_diff <- getValues(elevation_filled - elevation)
  
  cat("    Altered cells:", length(which(abs(dem_diff) > 1e-9)), "\n")
  cat("    New DEM bias compared to the original: within [", as.numeric(min(dem_diff)), ",", as.numeric(max(dem_diff)), "] m\n")

  # [LEGACY] (Optionally) add tiny jitter (noise) to the DEM to avoid problematic
  # cases where cell patches have a constant (flat) value, which disturbes drainage.
  # jitter_amount <- 1e-4
  # set.seed(1)
  # elevation_jitter <- setValues(elevation_cur, elevation_cur[] + rnorm(grid_ncol*grid_nrow, mean = 0, sd = jitter_amount))
  # elevation_cur <- elevation_jitter [/LEGACY]

  return(elevation_filled)
  
}
