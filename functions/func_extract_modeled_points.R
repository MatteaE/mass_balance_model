###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to extract the modeled series for a set of       #
#                 points, identified by their points_cells (four surrounding cells) and dx/dy,    #
#                 after running a simulation. Extraction uses bilinear interpolation.             #
#                 We have verified that the result corresponds exactly to                         #
#                 raster::extract(..., method = "bilinear"), but much faster since we avoid the   #
#                 conversions to raster.                                                          #
#                 Points can be annual stakes, winter stakes, or user-defined points.             #
###################################################################################################

# Note: the passed vec_modeled_full can be either the data in vec_massbal_cumul,
# to extract cumulative mass balance, or vec_swe_all, to extract current swe.
# The latter is used to extract SWE at user-defined points.

# We take the dx<i> and dy<i> as input,
# so that we pre-compute them just once
# per year (points don't move around
# during optimization).
func_extract_modeled_points <- function(run_params,
                                        dx1, dx2, dy1, dy2,
                                        vec_modeled_full,
                                        npoints,
                                        model_days_n,
                                        points_cells) {
  
  points_series_mod_all <- matrix(NA, nrow = model_days_n + 1, ncol = npoints) # One row per day, one column per point
  
  nval <- length(vec_modeled_full)
  
  for (point_id in 1:npoints) {
    
    # Cells are ordered like this:
    # 1 2
    # 3 4
    # with the point somewhere in the middle.
    # This means that (within the raster) cell 2 has
    # index ((cell 1) + 1), and cell 3 has index
    # ((cell 2) + (ncol - 1)), because raster cells
    # start at 1 from top-left and go row by row.
    # Repeated cells (i.e. if the point lies at
    # the same x and/or y as a cell center) cause a bug!
    # Observed if dy1 is 0: we have just two cells (1 and 2 in the square above),
    # points_cells is sorted; only cell_series3 and cell_series4 contribute due to dy1 = 0,
    # but these are derived from a same cell (weighted with two different weights).
    cell_series1 <- vec_modeled_full[points_cells[point_id, 1] + seq(0,nval-1,run_params$grid_ncells)]
    cell_series2 <- vec_modeled_full[points_cells[point_id, 2] + seq(0,nval-1,run_params$grid_ncells)]
    cell_series3 <- vec_modeled_full[points_cells[point_id, 3] + seq(0,nval-1,run_params$grid_ncells)]
    cell_series4 <- vec_modeled_full[points_cells[point_id, 4] + seq(0,nval-1,run_params$grid_ncells)]
    
    # dx1 = x distance from the two cells to the left (i.e. with lower X coordinate than the point),
    # dy1 = y distance from the two cells below (i.e. with lower Y coordinate),
    # dy2 = y distance from the two cells above (i.e. with higher Y coordinate).
    # NOTE: remember that if a point is at the edge of the glacier,
    # the four cells selected can in fact be just a same cell (the
    # one nearest to the point).
    points_series_mod_all[, point_id] <- (cell_series1 * dx2[point_id] * dy1[point_id] +
                                            cell_series2 * dx1[point_id] * dy1[point_id] +
                                            cell_series3 * dx2[point_id] * dy2[point_id] +
                                            cell_series4 * dx1[point_id] * dy2[point_id]) / (run_params$grid_cell_size^2)
    
  }
  
  return(points_series_mod_all)
  
}
