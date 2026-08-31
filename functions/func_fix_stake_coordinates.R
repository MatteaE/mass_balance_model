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

func_fix_stake_coordinates <- function(stake_id,
                                       stake_xy,
                                       dhm_extent,
                                       crs_try,
                                       crs_target) {
  
  point_cur <- st_point(stake_xy)
  
  crs_success_logi <- rep(FALSE, length(crs_try))
  coords_transf_l  <- list()
  
  for (crs_id in 1:length(crs_try)) {
    point_cur_sfc             <- st_sfc(point_cur, crs = crs_try[crs_id])
    point_cur_transf          <- st_transform(point_cur_sfc, crs_target)
    point_transf_coords       <- as.numeric(st_coordinates(point_cur_transf))
    coords_transf_l[[crs_id]] <- point_transf_coords
    if (all(!is.na(point_transf_coords))) {
      if ((dhm_extent[1] <= point_transf_coords[1]) && (dhm_extent[2] >= point_transf_coords[1]) && (dhm_extent[3] <= point_transf_coords[2]) && (dhm_extent[4] >= point_transf_coords[2])) {
        func_customlog("            Point id ", stake_id, " might be using the EPSG:", crs_try[crs_id], " coordinate system.", level = 0)
        crs_success_logi[crs_id] <- TRUE
      }
    }
  } # End loop on attempted CRS
  
  # If a single CRS works for the current point, apply it.
  # If more than one matches (unlikely but possible on very large areas), stop and complain.
  # If none matches, return NA which signals that it is not possible to rescue the point.
  crs_ok_ids <- which(crs_success_logi)
  if (length(crs_ok_ids) == 1) {
    return(coords_transf_l[[crs_ok_ids]])
  } else if (length(crs_ok_ids) > 1) {
    func_customlog("            Point id ", stake_id, " has an ambiguous coordinate system. Please check and fix its coordinates manually.", level = 2)
    func_customlog("            Provided values: ", stake_xy[1], ", ", stake_xy[2], level = 0)
    func_stop()
  } else {
    return(c(NA, NA))
  }
  
}
