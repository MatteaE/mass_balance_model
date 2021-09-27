###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to find the stake offsets within the model grid.    #
#                 These are used for the bilinear extraction of the modeled stake series.         #
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
func_find_stake_dxdy <- function(year_data,
                                 data_dhms,
                                 run_params) {
  
  
  dx1_annual <- (year_data$massbal_annual_meas_cur$x - (extent(data_dhms$elevation[[year_data$dhm_grid_id]])[1] - (run_params$grid_cell_size / 2))) %% run_params$grid_cell_size
  dx2_annual <- run_params$grid_cell_size - dx1_annual
  dy2_annual <- ((extent(data_dhms$elevation[[year_data$dhm_grid_id]])[3] - (run_params$grid_cell_size / 2)) - year_data$massbal_annual_meas_cur$y) %% run_params$grid_cell_size
  dy1_annual <- run_params$grid_cell_size - dy2_annual
  
  dx1_winter <- (year_data$massbal_winter_meas_cur$x - (extent(data_dhms$elevation[[year_data$dhm_grid_id]])[1] - (run_params$grid_cell_size / 2))) %% run_params$grid_cell_size
  dx2_winter <- run_params$grid_cell_size - dx1_winter
  dy2_winter <- ((extent(data_dhms$elevation[[year_data$dhm_grid_id]])[3] - (run_params$grid_cell_size / 2)) - year_data$massbal_winter_meas_cur$y) %% run_params$grid_cell_size
  dy1_winter <- run_params$grid_cell_size - dy2_winter

  
  year_data$stake_dxdy <- list(annual = list(dx1_annual, dx2_annual, dy1_annual, dy2_annual),
                               winter = list(dx1_winter, dx2_winter, dy1_winter, dy2_winter))
  
  return(year_data)
    
}



