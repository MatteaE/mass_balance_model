###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the one-time computation of derived grids from the DHMs, to  #
#                 be used by the avalanche module.                                                #
###################################################################################################

func_compute_avalanche_static_grids <- function(run_params, data_dhms) {
  
  cat("Computing avalanche static grids...\n")
  
  conv_deg2rad <- pi / 180

  #### Prepare data structures ####
  # We keep a lot of intermediate grids to make it easy
  # to debug problems with the avalanche routine.
  # Structure: avalanche                   list of lists
  #                     $elevation_proc    list of rasters, one element per available grid
  #                     $dz                list of lists, one element per available grid
  #                        [[1]]           list of lists, one element per direction (always 4 elements)
  # So: avalanche$elevation_proc[[4]]      is the hydrologically corrected grid for the fourth elevation raster available
  #     avalanche$dz[2]][[3]]             is the elevation-difference grid for the second elevation raster available, for the east direction
  avalanche                               <- list()

  avalanche$elevation_proc                <- list()
  avalanche$slope_proc                    <- list()
  avalanche$aspect_proc                   <- list()
  avalanche$movable_frac                  <- list()
  avalanche$flow_width                    <- list()
  avalanche$dz                            <- list()
  avalanche$draining_coeff                <- list()
  avalanche$draining_coeff_sum            <- list()
  avalanche$residual_sink_cell_ids        <- list()
  avalanche$draining_fraction             <- list()
  avalanche$deposition_max                <- list()
  avalanche$elevation_sorted_ids          <- list()


  #### Fill data structures ####
  for (grid_id in 1:data_dhms$n_grids) {
    
    # Create directional structures.
    avalanche$flow_width[[grid_id]]        <- list()
    avalanche$dz[[grid_id]]                <- list()
    avalanche$draining_coeff[[grid_id]]    <- list()
    avalanche$draining_fraction[[grid_id]] <- list()

    # Process the elevation grid to make it hydrologically correct
    # (no flat patches, no sinks).
    cat("  Pre-processing elevation grid", paste0(grid_id, "...\n"))
    avalanche$elevation_proc[[grid_id]]       <- func_elevation_preprocess(run_params, data_dhms$elevation[[grid_id]])
    
    # Compute slope and aspect.
    # We extend along the borders
    # (nearest neighbor: we replicate
    # the closest row/column)
    # to avoid having dangerous NA values.
    avalanche$slope_proc[[grid_id]]  <- terrain(avalanche$elevation_proc[[grid_id]], v = "slope", unit = "degrees")
    avalanche$slope_proc[[grid_id]][1:run_params$grid_ncol] <- avalanche$slope_proc[[grid_id]][run_params$grid_ncol + (1:run_params$grid_ncol)][,1]
    avalanche$slope_proc[[grid_id]][run_params$grid_ncells - run_params$grid_ncol + 1:run_params$grid_ncol] <- avalanche$slope_proc[[grid_id]][run_params$grid_ncells - (2*run_params$grid_ncol) + 1:run_params$grid_ncol][,1]
    avalanche$slope_proc[[grid_id]][seq(1,run_params$grid_ncells,run_params$grid_ncol)] <- avalanche$slope_proc[[grid_id]][seq(2,run_params$grid_ncells,run_params$grid_ncol)][,1]
    avalanche$slope_proc[[grid_id]][seq(run_params$grid_ncol,run_params$grid_ncells,run_params$grid_ncol)] <- avalanche$slope_proc[[grid_id]][seq(run_params$grid_ncol-1,run_params$grid_ncells,run_params$grid_ncol)][,1]
    
    avalanche$aspect_proc[[grid_id]] <- terrain(avalanche$elevation_proc[[grid_id]], v = "aspect", unit = "degrees")
    avalanche$aspect_proc[[grid_id]][1:run_params$grid_ncol] <- avalanche$aspect_proc[[grid_id]][run_params$grid_ncol + (1:run_params$grid_ncol)][,1]
    avalanche$aspect_proc[[grid_id]][run_params$grid_ncells - run_params$grid_ncol + 1:run_params$grid_ncol] <- avalanche$aspect_proc[[grid_id]][run_params$grid_ncells - (2*run_params$grid_ncol) + 1:run_params$grid_ncol][,1]
    avalanche$aspect_proc[[grid_id]][seq(1,run_params$grid_ncells,run_params$grid_ncol)] <- avalanche$aspect_proc[[grid_id]][seq(2,run_params$grid_ncells,run_params$grid_ncol)][,1]
    avalanche$aspect_proc[[grid_id]][seq(run_params$grid_ncol,run_params$grid_ncells,run_params$grid_ncol)] <- avalanche$aspect_proc[[grid_id]][seq(run_params$grid_ncol-1,run_params$grid_ncells,run_params$grid_ncol)][,1]
    
    
    
    # Movable fraction of the initial mass distribution
    # linearly increases from 0 to 1 between the lower
    # and upper slope thresholds.
    avalanche$movable_frac[[grid_id]] <- rast(avalanche$slope_proc[[grid_id]],
                                              vals = pmax(0, pmin(1, scales::rescale(values(avalanche$slope_proc[[grid_id]]),
                                                                                     to = c(0, 1),
                                                                                     from = c(run_params$movable_slope_lim_lower, run_params$movable_slope_lim_upper)))))
    # Compute flow widths to 4-neighbors (Eqs. 3-6 of Gruber, 2007).
    # Indices: 1 top, 2 left, 3 right, 4 bottom (as in Gruber, 2007, Fig. 1).
    avalanche$flow_width[[grid_id]][[1]] <- cos(conv_deg2rad * avalanche$aspect_proc[[grid_id]]) * run_params$grid_cell_size
    avalanche$flow_width[[grid_id]][[2]] <- -sin(conv_deg2rad * avalanche$aspect_proc[[grid_id]]) * run_params$grid_cell_size
    avalanche$flow_width[[grid_id]][[3]] <- -avalanche$flow_width[[grid_id]][[2]]
    avalanche$flow_width[[grid_id]][[4]] <- -avalanche$flow_width[[grid_id]][[1]]
    
    # Compute elevation difference to 4-neighbors.
    # Same indexing as above.
    avalanche$dz[[grid_id]][[1]] <- avalanche$elevation_proc[[grid_id]] - focal(avalanche$elevation_proc[[grid_id]], w = rbind(c(0,1,0),
                                                                                                                               c(0,0,0),
                                                                                                                               c(0,0,0)), expand = FALSE, fillvalue = NA_real_)
    avalanche$dz[[grid_id]][[2]] <- avalanche$elevation_proc[[grid_id]] - focal(avalanche$elevation_proc[[grid_id]], w = rbind(c(0,0,0),
                                                                                                                               c(1,0,0),
                                                                                                                               c(0,0,0)), expand = FALSE, fillvalue = NA_real_)
    avalanche$dz[[grid_id]][[3]] <- avalanche$elevation_proc[[grid_id]] - focal(avalanche$elevation_proc[[grid_id]], w = rbind(c(0,0,0),
                                                                                                                               c(0,0,1),
                                                                                                                               c(0,0,0)), expand = FALSE, fillvalue = NA_real_)
    avalanche$dz[[grid_id]][[4]] <- avalanche$elevation_proc[[grid_id]] - focal(avalanche$elevation_proc[[grid_id]], w = rbind(c(0,0,0),
                                                                                                                               c(0,0,0),
                                                                                                                               c(0,1,0)), expand = FALSE, fillvalue = NA_real_)
    
    
    # Compute draining coefficient to 4-neighbors (Eq. 7 in Gruber, 2007).
    for (dir_id in 1:4) {
      avalanche$draining_coeff[[grid_id]][[dir_id]] <- avalanche$flow_width[[grid_id]][[dir_id]] * (avalanche$dz[[grid_id]][[dir_id]] > 0) * (avalanche$flow_width[[grid_id]][[dir_id]] > 0)
    }
    
    avalanche$draining_coeff_sum[[grid_id]] <- avalanche$draining_coeff[[grid_id]][[1]] + avalanche$draining_coeff[[grid_id]][[2]] + avalanche$draining_coeff[[grid_id]][[3]] + avalanche$draining_coeff[[grid_id]][[4]]

    # Despite all the DEM pre-processing, there can be cells
    # with draining_coeff_sum = 0, i.e. no drainage possible.
    # These arise when the computed flow widths do not match
    # the computed dz (flow width is computed from aspect, which is
    # determined via curve fitting since a plane is determined
    # by 3 points, so on a regular grid with 4 neighbors aspect is not
    # well defined!). In those cases, the direction for which dz<i> > 0
    # does not match the direction for which L<i> > 0, so that no drainage
    # would be possible from those cells. These would behave as infinite
    # sinks, stealing mass as it enters but cannot exit.
    # To solve this, we take those (hopefully rare) cells and
    # we force drainage towards the most likely direction
    # (i.e., of all the directions with dz<i> > 0, the direction
    # for which flow width is least negative).
    avalanche$residual_sink_cell_ids[[grid_id]] <- which(values(avalanche$draining_coeff_sum[[grid_id]]) == 0)
    residual_sinks_n <- length(avalanche$residual_sink_cell_ids[[grid_id]])
    # cat("Residual sinks detected:", residual_sinks_n, "\n")
    
    if (residual_sinks_n) {
      for (residual_sink_id in 1:residual_sinks_n) {
        residual_sink_cell_id <- avalanche$residual_sink_cell_ids[[grid_id]][residual_sink_id]
        
        # Get dz and flow widths of problem cell (two 4-vectors).
        # There won't be corresponding elements which are both positive
        # (else the cell would not be problematic).
        # We want the element with least negative flow width,
        # and positive dz.
        # Nested sapply(): we retrieve from the nested list the 4 grids (of both dz
        # and flow width) of the current grid index (inner sapply()), then for each
        # we extract the value at cell index residual_sink_cell_id.
        cell_orig_ids <- 1:4
        cell_dzs <- unlist(sapply(avalanche$dz[[grid_id]], extract, residual_sink_cell_id))
        cell_flow_widths <- unlist(sapply(avalanche$flow_width[[grid_id]], extract, residual_sink_cell_id))

        cell_df <- data.frame(cell_orig_ids, cell_dzs, cell_flow_widths)
        cell_df_downslope <- cell_df[which(cell_df$cell_dzs >= 0),]
        cell_df_id_sel <- which.max(cell_df_downslope$cell_flow_widths)
        
        dir_id <- cell_df_downslope$cell_orig_ids[cell_df_id_sel]
        
        # Send all snow towards the selected cell.
        avalanche$draining_coeff[[grid_id]][[dir_id]][residual_sink_cell_id] <- 1
        avalanche$draining_coeff_sum[[grid_id]][residual_sink_cell_id] <- 1
      }
    }
    
    cat("    Residual sinks fixed.\n")
    
    # Compute normalized draining fractions for the 4 directions (Eq. 9 in Gruber, 2007).
    for (dir_id in 1:4) {
      avalanche$draining_fraction[[grid_id]][[dir_id]] <- avalanche$draining_coeff[[grid_id]][[dir_id]] / avalanche$draining_coeff_sum[[grid_id]]
    }
    
    # Compute max deposition (Eq. 10 in Gruber, 2007); [kg m-2].
    avalanche$deposition_max[[grid_id]] <- (1 - avalanche$slope_proc[[grid_id]] / run_params$deposition_slope_lim) * run_params$deposition_mass_lim * (avalanche$slope_proc[[grid_id]] < run_params$deposition_slope_lim)
    
    # Compute indices for loop from highest to lowest domain cell.
    # Cells are indexed by row (i.e. first all the first row, then
    # all the second row; the last cell is the bottom-right corner).
    # We exclude all cells along the border since their drainage is problematic.
    # Of course the glacier should not reach the grid border.
    elevation_sorted_ids_raw <- sort(values(avalanche$elevation_proc[[grid_id]]), decreasing = TRUE, index.return = TRUE)[[2]]
    elevation_ids_border <- unique(c(cellFromRowColCombine(avalanche$elevation_proc[[grid_id]], c(1, run_params$grid_nrow), 1:run_params$grid_ncol), cellFromRowColCombine(avalanche$elevation_proc[[grid_id]], 1:run_params$grid_nrow, c(1, run_params$grid_ncol))))
    avalanche$elevation_sorted_ids[[grid_id]] <- setdiff(elevation_sorted_ids_raw, elevation_ids_border)
    
  }

  cat("  Finished computation of avalanche static grids.\n")

  return(avalanche)

}
