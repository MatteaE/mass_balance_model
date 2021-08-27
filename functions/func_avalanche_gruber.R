###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the computation of a grid of relative (normalized)           #
#                 snow distribution from elevation, slope and curvature.                          #
#                 This is the exact same algorithm (line-by-line) as the C++ version.             #
#                 It can be used for debugging, and on systems which don't support Rcpp.          #
###################################################################################################


transport_deposit_mass_R <- function(elevation_sorted_ids,
                                   grid_ncol,
                                   deposition,
                                   mass_movable,
                                   deposition_max,
                                   draining_fraction1,
                                   draining_fraction2,
                                   draining_fraction3,
                                   draining_fraction4
                                   ) {
    
    # elevation_sorted_ids <- elevation_sorted_ids - 1  C++ indexing starts from 0, R indexing starts from 1!
    n_cells <- length(elevation_sorted_ids)
    
    # Iterate over the cells from the highest to the lowest,
    # moving mass downstream according to the geometries and thresholds.
    cell_cur_id_id <- 1
    while (cell_cur_id_id <= n_cells) {

        # cat(cell_cur_id_id, "\n")
        cell_cur_id <- elevation_sorted_ids[cell_cur_id_id]
        
        # 1 = top, 2 = left, 3 = right, 4 = bottom.
        # We are iterating only on the non-border cells,
        # so the adjacent ones are always easy to compute.
        cell_adjacent_id1 <- cell_cur_id - grid_ncol
        cell_adjacent_id2 <- cell_cur_id - 1
        cell_adjacent_id3 <- cell_cur_id + 1
        cell_adjacent_id4 <- cell_cur_id + grid_ncol
        
        deposition[cell_cur_id] = min(deposition_max[cell_cur_id], mass_movable[cell_cur_id]); # Eq. 1 in Gruber (2007).
        flow1 <- (mass_movable[cell_cur_id] - deposition[cell_cur_id]) * draining_fraction1[cell_cur_id]; # Eq. 2 in Gruber (2007).
        flow2 <- (mass_movable[cell_cur_id] - deposition[cell_cur_id]) * draining_fraction2[cell_cur_id];
        flow3 <- (mass_movable[cell_cur_id] - deposition[cell_cur_id]) * draining_fraction3[cell_cur_id];
        flow4 <- (mass_movable[cell_cur_id] - deposition[cell_cur_id]) * draining_fraction4[cell_cur_id];
        
        mass_movable[cell_adjacent_id1] <- mass_movable[cell_adjacent_id1] + flow1;
        mass_movable[cell_adjacent_id2] <- mass_movable[cell_adjacent_id2] + flow2;
        mass_movable[cell_adjacent_id3] <- mass_movable[cell_adjacent_id3] + flow3;
        mass_movable[cell_adjacent_id4] <- mass_movable[cell_adjacent_id4] + flow4;
        
        cell_cur_id_id <- cell_cur_id_id + 1
    }
    
    return(deposition)
}
