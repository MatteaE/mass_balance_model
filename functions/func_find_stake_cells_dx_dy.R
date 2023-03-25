###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to find the grid cells from which to extract        #
#                 the modeled stakes series, as well as their distance from the stakes.           #
###################################################################################################

# Find (vectorized) the distance of each annual (and then winter) stake from the 4 surrounding cell centers.
# We will use this later to extract the modeled series for each stake, with bilinear filtering.
# dx1 = x distance from the two cells to the left (i.e. with lower X coordinate than the stake),
# dy1 = y distance from the two cells below (i.e. with lower Y coordinate),
# dy2 = y distance from the two cells above (i.e. with higher Y coordinate).
# We compute dy2 first so that it can also be 0 (i.e. stake aligned with center of the two
# cells on the upper row of the 4 neighbors).
# If we computed dy1 first, then if it were 0 we would be placing the stake one row below
# its actual position (because the computation uses the %% operator, so a cell which is
# vertically aligned with the grid centers gets 0, and if dy1_annual were computed as 0
# it would mean that the cell is aligned with the lower row of the 4 neighbors,
# but (see lines below) the "lower row" is in fact an artifact due to duplicates = FALSE,
# while the stake is well aligned with the *upper* row).
# duplicates = FALSE is needed to always have 4 cells, else bad things would happen.
# This is important for the bilinear filtering since we use fourCellsFromXY(..., duplicates = FALSE),
# else the filtering would fail (duplicates = FALSE returns (if needed) additional cells which have higher index,
# i.e. which are lower in the raster matrix, i.e. which would be the lower row of the 4 neighbors).
func_find_stake_cells_dx_dy <- function(year_data,
                                        data_dhms,
                                        data_dems,
                                        run_params) {
  
  dx1_annual <- (year_data$massbal_annual_meas_cur$x - (ext(data_dhms$elevation[[year_data$dhm_grid_id]])[1] - (run_params$grid_cell_size / 2))) %% run_params$grid_cell_size
  dx2_annual <- run_params$grid_cell_size - dx1_annual
  dy2_annual <- ((ext(data_dhms$elevation[[year_data$dhm_grid_id]])[3] - (run_params$grid_cell_size / 2)) - year_data$massbal_annual_meas_cur$y) %% run_params$grid_cell_size
  dy1_annual <- run_params$grid_cell_size - dy2_annual
  
  dx1_winter <- (year_data$massbal_winter_meas_cur$x - (ext(data_dhms$elevation[[year_data$dhm_grid_id]])[1] - (run_params$grid_cell_size / 2))) %% run_params$grid_cell_size
  dx2_winter <- run_params$grid_cell_size - dx1_winter
  dy2_winter <- ((ext(data_dhms$elevation[[year_data$dhm_grid_id]])[3] - (run_params$grid_cell_size / 2)) - year_data$massbal_winter_meas_cur$y) %% run_params$grid_cell_size
  dy1_winter <- run_params$grid_cell_size - dy2_winter
  
  
  # Now find indices of the cells from which to extract the modeled series.
  # We typically use the 4 neighbors, but if one or more of them are outside
  # the glacier's edge then we use only the nearest valid neighbor.
  # We ask duplicates = FALSE, else the bilinear filtering in func_extract_modeled_stakes()
  # can fail when a stake is exactly at the same (X and/or Y) coordinate as a cell center.
  # duplicates = FALSE returns four different cells. In case we have a stake exactly
  # aligned with a cell center, unless we are at the lower raster border (which we should
  # always avoid!) the additional cells returned with duplicates = FALSE (cells which would
  # not be part of the actual adjacent cells) have higher index than the "true" adjacent cells.
  cells_annual <- rowSort(fourCellsFromXY(data_dhms$elevation[[year_data$dhm_grid_id]], as.matrix(year_data$massbal_annual_meas_cur[,4:5]), duplicates = FALSE))
  cells_annual_dem_value <- matrix(data_dems$elevation[[year_data$dem_grid_id]][as.integer(t(cells_annual))][,1], ncol = 4, byrow = TRUE)
  stakes_annual_edge_ids <- integer(0)
  for (stake_id in 1:year_data$nstakes_annual) {
    stake_na_cells_logi <- is.na(cells_annual_dem_value[stake_id,])
    if (length(which(stake_na_cells_logi)) > 0) {
      cell_distances <- spDistsN1(xyFromCell(data_dhms$elevation[[year_data$dhm_grid_id]], cells_annual[stake_id,]), as.matrix(year_data$massbal_annual_meas_cur[stake_id,4:5]))
      cell_scores <- cell_distances / (!stake_na_cells_logi)
      cell_selected <- which.min(cell_scores)
      cells_annual[stake_id,] <- cells_annual[stake_id, cell_selected]
      stakes_annual_edge_ids <- append(stakes_annual_edge_ids, stake_id)
    }
  }
  stakes_annual_edge_n <- length(stakes_annual_edge_ids)
  if (stakes_annual_edge_n > 0) {
    cat("* WARNING: found", stakes_annual_edge_n, "annual measurement(s) which are at the very edge of the glacier. Bilinear extraction of their modeled series is not possible, I will use nearest neighbor.\n")
    cat("They are: ", paste0(year_data$massbal_annual_meas_cur$id[stakes_annual_edge_ids], collapse = " | "), "\n")
  }
  
  if (year_data$nstakes_winter > 0) {
    cells_winter <- rowSort(fourCellsFromXY(data_dhms$elevation[[year_data$dhm_grid_id]], as.matrix(year_data$massbal_winter_meas_cur[,4:5]), duplicates = FALSE))
    cells_winter_dem_value <- matrix(data_dems$elevation[[year_data$dem_grid_id]][as.integer(t(cells_winter))][,1], ncol = 4, byrow = TRUE)
    stakes_winter_edge_ids <- integer(0)
    for (stake_id in 1:year_data$nstakes_winter) {
      stake_na_cells_logi <- is.na(cells_winter_dem_value[stake_id,])
      if (length(which(stake_na_cells_logi)) > 0) {
        cell_distances <- spDistsN1(xyFromCell(data_dhms$elevation[[year_data$dhm_grid_id]], cells_winter[stake_id,]), as.matrix(year_data$massbal_winter_meas_cur[stake_id,4:5]))
        cell_scores <- cell_distances / (!stake_na_cells_logi)
        cell_selected <- which.min(cell_scores)
        cells_winter[stake_id,] <- cells_winter[stake_id, cell_selected]
        stakes_winter_edge_ids <- append(stakes_winter_edge_ids, stake_id)
      }
    }
    stakes_winter_edge_n <- length(stakes_winter_edge_ids)
    if (stakes_winter_edge_n > 0) {
      cat("* WARNING: found", stakes_winter_edge_n, "winter measurement(s) which are at the very edge of the glacier. Bilinear extraction of their modeled series is not possible, I will use nearest neighbor.\n")
      cat("They are: ", paste0(year_data$massbal_winter_meas_cur$id[stakes_winter_edge_ids], collapse = " | "), "\n")
    }
  }
  
  year_data$stake_dxdy <- list(annual = list(dx1_annual, dx2_annual, dy1_annual, dy2_annual),
                               winter = list(dx1_winter, dx2_winter, dy1_winter, dy2_winter))
  
  year_data$annual_stakes_cells <- cells_annual
  if (year_data$nstakes_winter > 0) {
    year_data$winter_stakes_cells <- cells_winter
  }
  
  return(year_data)
  
}
