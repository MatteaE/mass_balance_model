###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to merge an elevation band with the smaller      #
#                 of the two neighbors (dealing with cases where the band to be merged is either  #
#                 the lowest or the highest).                                                     #
###################################################################################################


func_merge_elevation_band <- function(ele_bands,
                                      band_to_merge_id) {
  
  ele_bands_neigh_ids <- setdiff(clamp(band_to_merge_id + c(-1,1), 1, nrow(ele_bands), useValues = TRUE), band_to_merge_id)
  ele_bands_neigh_sizes <- ele_bands$extent[ele_bands_neigh_ids]
  ele_bands_neigh_smaller_id <- ele_bands_neigh_ids[which.min(ele_bands_neigh_sizes)]
  if (ele_bands_neigh_smaller_id < band_to_merge_id) {
    ele_bands$bound_upper[ele_bands_neigh_smaller_id] <- ele_bands$bound_upper[band_to_merge_id]
  } else {
    ele_bands$bound_lower[ele_bands_neigh_smaller_id] <- ele_bands$bound_lower[band_to_merge_id]
  }
  ele_bands$extent[ele_bands_neigh_smaller_id] <- ele_bands$bound_upper[ele_bands_neigh_smaller_id] - ele_bands$bound_lower[ele_bands_neigh_smaller_id]
  ele_bands <- ele_bands[-band_to_merge_id,]
  
  return(ele_bands)
  
}
