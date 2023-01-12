###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to automatically compute the elevation bands     #
#                 for the contour-line correction, starting from the annual stakes.               #
###################################################################################################


# Algorithm:
# If there are fewer than 4 unique stake elevations, no band is returned (NA).
# Else, if stake elevations are h1, h2, ..., hN:
# first create bands [h1-50m, (h1+h2)/2]; [(h1+h2)/2, (h2+h3)/2]; ...; [(hN-1+hN)/2, hN+50m].
# Then compute bands vertical extent.
# While there are bands whose vertical extent is smaller than 50 m, merge them
# with the smaller of the two neighbors.
# While there are bands whose vertical extent is smaller than q25-1.5*IQR
# (recompute at each iteration), merge them (same logic as above).
# At the end of this, if there are fewer than 3 bands give up,
# if there are more than 10 bands merge them with the usual logic,
# starting from the smallest one (recompute at each iteration).
func_compute_ele_bands_from_stakes <- function(stakes_ele,
                                               run_params) {
  
  stakes_ele_unique <- unique(stakes_ele)
  stakes_ele_n      <- length(stakes_ele_unique)
  
  if (stakes_ele_n < 4) {
    return(NA)
  }
  
  stakes_ele_sorted <- sort(stakes_ele_unique)
  
  # End bands 50 m below/above extreme stakes.
  bands_ele_min <- stakes_ele_sorted[1] - 50
  bands_ele_max <- stakes_ele_sorted[stakes_ele_n] + 50
  
  ele_bands             <- data.frame(bound_lower = c(bands_ele_min, (stakes_ele_sorted[1:(stakes_ele_n-1)] + stakes_ele_sorted[2:stakes_ele_n]) / 2))
  ele_bands$bound_upper <- c(ele_bands$bound_lower[2:stakes_ele_n], bands_ele_max)
  ele_bands$extent      <- ele_bands$bound_upper - ele_bands$bound_lower
  
  # Merge bands below fixed extent threshold.
  ele_bands_smallest_id <-  which.min(ele_bands$extent)
  while (ele_bands$extent[ele_bands_smallest_id] < run_params$ele_bands_auto_min_extent) {
    # cat("Merging bands with extent below fixed threshold...\n")
    ele_bands <- func_merge_elevation_band(ele_bands, ele_bands_smallest_id)
    ele_bands_smallest_id <-  which.min(ele_bands$extent)
  }
  
  # Merge bands below q25 - 1.5 * IQR (i.e. outliers).
  ele_bands_extent_q25 <- as.numeric(quantile(ele_bands$extent, 0.25))
  ele_bands_extent_iqr <- IQR(ele_bands$extent)
  ele_bands_extent_lower_bound <- ele_bands_extent_q25 - 1.5 * ele_bands_extent_iqr
  ele_bands_smallest_id <-  which.min(ele_bands$extent)
  while (ele_bands$extent[ele_bands_smallest_id] < ele_bands_extent_lower_bound) {
    # cat("Merging bands with unusually small extent...\n")
    ele_bands <- func_merge_elevation_band(ele_bands, ele_bands_smallest_id)
    ele_bands_extent_q25 <- as.numeric(quantile(ele_bands$extent, 0.25))
    ele_bands_extent_iqr <- IQR(ele_bands$extent)
    ele_bands_extent_lower_bound <- ele_bands_extent_q25 - 1.5 * ele_bands_extent_iqr
    ele_bands_smallest_id <-  which.min(ele_bands$extent)
  }
  
  # If fewer than 4 correction bands give up,
  # if more than 10 keep merging until we have 10 or fewer.
  ele_bands_n <- nrow(ele_bands)
  if (ele_bands_n < 3) {
    return(NA)
  }
  if (ele_bands_n > 10) {
    ele_bands_smallest_id <-  which.min(ele_bands$extent)
    while (ele_bands_n > 10) {
      # cat("Merging bands until there are 10 or fewer of them...\n")
      ele_bands <- func_merge_elevation_band(ele_bands, ele_bands_smallest_id)
      ele_bands_n <- nrow(ele_bands)
      ele_bands_smallest_id <-  which.min(ele_bands$extent)
    }
  }
  
  # Return bands already in the format used by run_params:
  # only a numeric vector of band boundaries, including the extremes.
  return(c(ele_bands$bound_lower[1], ele_bands$bound_upper))
  
}
