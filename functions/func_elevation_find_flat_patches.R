###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a function to find flat patches in an elevation grid.        #
#                 These are defined as contiguous pixels with exactly the same elevation.         #
###################################################################################################

func_find_flat_patches <- function(elevation, run_params) {
  
  grid_ncol <- ncol(elevation)
  grid_nrow <- nrow(elevation)
  
  # Look for flat patches (contiguous cells with exact same elevation).
  # We do this with focal() and a 3x3 matrix getting the proper neighbor.
  dz1 <- elevation - focal(elevation, w = rbind(c(0,1,0),
                                                c(0,0,0),
                                                c(0,0,0)), expand = FALSE, fillvalue = NA_real_)
  dz2 <- elevation - focal(elevation, w = rbind(c(0,0,0),
                                                c(0,0,1),
                                                c(0,0,0)), expand = FALSE, fillvalue = NA_real_)
  dz3 <- elevation - focal(elevation, w = rbind(c(0,0,0),
                                                c(1,0,0),
                                                c(0,0,0)), expand = FALSE, fillvalue = NA_real_)
  dz4 <- elevation - focal(elevation, w = rbind(c(0,0,0),
                                                c(0,0,0),
                                                c(0,1,0)), expand = FALSE, fillvalue = NA_real_)
  
  ids_patch_flat <- which((abs(values(dz1)) < run_params$elevation_equal_threshold) |
                          (abs(values(dz2)) < run_params$elevation_equal_threshold) |
                          (abs(values(dz3)) < run_params$elevation_equal_threshold) |
                          (abs(values(dz4)) < run_params$elevation_equal_threshold))
  
  return(ids_patch_flat)
  
}
