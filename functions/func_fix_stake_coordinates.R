###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to try to rescue a stake which is provided          #
#                 with a wrong coordinate system.                                                 #
################################################################################################### 

# Algorithm:
# - create several st_point with the stake_xy coordinates and set its CRS to the supplied crs_try
# - transform each st_point to the CRS which we use
# - check if any of the transformed points fall within the dhm extent
# - if yes, return the transformed coordinates; if not, return c(NA, NA).

func_fix_stake_coordinates <- function(stake_xy,
                                       dhm_extent,
                                       crs_try,
                                       crs_target) {
  
  point_cur <- st_point(stake_xy)
  
  for (crs_id in 1:length(crs_try)) {
    point_cur_sfc <- st_sfc(point_cur, crs = crs_try[crs_id])
    point_cur_transf <- st_transform(point_cur_sfc, crs_target)
    point_transf_coords <- as.numeric(st_coordinates(point_cur_transf))
    if (all(!is.na(point_transf_coords))) {
      if ((dhm_extent[1] <= point_transf_coords[1]) && (dhm_extent[2] >= point_transf_coords[1]) && (dhm_extent[3] <= point_transf_coords[2]) && (dhm_extent[4] >= point_transf_coords[2])) {
        cat(paste0("      Just rescued a pair of coordinates! They were likely in the EPSG:", crs_try[crs_id], " coordinate system.\n"))
        return(point_transf_coords)
      }
    }
  }
  
  return(c(NA, NA))
}
