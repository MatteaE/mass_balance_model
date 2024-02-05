###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to extract the modeled series for all stakes,    #
#                 after running a simulation. Extraction uses bilinear interpolation of the four  #
#                 surrounding grid cells.                                                         #
#                 We have verified that the result corresponds exactly to                         #
#                 raster::extract(..., method = "bilinear"), but much faster since we avoid the   #
#                 conversions to raster.                                                          #
###################################################################################################


# We take the dx<i> and dy<i> as input,
# so that we pre-compute them just once
# per year (stakes don't move around
# during optimization).
func_extract_modeled_stakes <- function(run_params,
                                        dx1, dx2, dy1, dy2,
                                        vec_massbal_cumul,
                                        nstakes,
                                        model_days_n,
                                        stakes_cells) {
  
  stakes_series_mod_all <- matrix(NA, nrow = model_days_n + 1, ncol = nstakes) # One row per day, one column per stake
  
  nval <- length(vec_massbal_cumul)
  
  for (stake_id in 1:nstakes) {
    
    # Cells are ordered like this:
    # 1 2
    # 3 4
    # with the stake somewhere in the middle.
    # This means that (within the raster) cell 2 has
    # index ((cell 1) + 1), and cell 3 has index
    # ((cell 2) + (ncol - 1)), because raster cells
    # start at 1 from top-left and go row by row.
    # Repeated cells (i.e. if the stake lies at
    # the same x and/or y as a cell center) CAUSE A BUG!!!!
    # Observed if dy1 is 0: we have just two cells (1 and 2 in the square above),
    # stakes_cells is sorted; only cell_series3 and cell_series4 contribute due to dy1 = 0,
    # but these are derived from a same cell (weighted with two different weights).
    cell_series1 <- vec_massbal_cumul[stakes_cells[stake_id, 1] + seq(0,nval-1,run_params$grid_ncells)]
    cell_series2 <- vec_massbal_cumul[stakes_cells[stake_id, 2] + seq(0,nval-1,run_params$grid_ncells)]
    cell_series3 <- vec_massbal_cumul[stakes_cells[stake_id, 3] + seq(0,nval-1,run_params$grid_ncells)]
    cell_series4 <- vec_massbal_cumul[stakes_cells[stake_id, 4] + seq(0,nval-1,run_params$grid_ncells)]
    
    # dx1 = x distance from the two cells to the left (i.e. with lower X coordinate than the stake),
    # dy1 = y distance from the two cells below (i.e. with lower Y coordinate),
    # dy2 = y distance from the two cells above (i.e. with higher Y coordinate).
    # NOTE: remember that if a stake is at the edge of the glacier,
    # the four cells selected can in fact be just a same cell (the
    # one nearest to the stake).
    stakes_series_mod_all[, stake_id] <- (cell_series1 * dx2[stake_id] * dy1[stake_id] +
                                          cell_series2 * dx1[stake_id] * dy1[stake_id] +
                                          cell_series3 * dx2[stake_id] * dy2[stake_id] +
                                          cell_series4 * dx1[stake_id] * dy2[stake_id]) / (run_params$grid_cell_size^2)
    
  }
  
  return(stakes_series_mod_all)
  
}
