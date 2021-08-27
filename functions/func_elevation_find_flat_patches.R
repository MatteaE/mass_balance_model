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
  dz1 <- setValues(elevation, c(rep(NA, grid_ncol), elevation[2:grid_nrow,] - elevation[1:(grid_nrow - 1),]))
  dz2 <- dz1
  dz2[,1:grid_ncol] <- c(rep(NA, grid_nrow), elevation[,2:grid_ncol] - elevation[,1:(grid_ncol - 1)]) # We cannot use setValues() here because we compute by column and setValues sets by row.
  dz3 <- dz1
  dz3[,1:grid_ncol] <- c(elevation[,1:(grid_ncol - 1)] - elevation[,2:grid_ncol], rep(NA, grid_nrow)) # We cannot use setValues() here because we compute by column and setValues sets by row.
  dz4 <- setValues(elevation, c(elevation[1:(grid_nrow - 1),] - elevation[2:grid_nrow,], rep(NA, grid_ncol)))
  
  ids_patch_flat <- which((abs(getValues(dz1)) < run_params$elevation_equal_threshold) |
                          (abs(getValues(dz2)) < run_params$elevation_equal_threshold) |
                          (abs(getValues(dz3)) < run_params$elevation_equal_threshold) |
                          (abs(getValues(dz4)) < run_params$elevation_equal_threshold))
  
  return(ids_patch_flat)
  
}
