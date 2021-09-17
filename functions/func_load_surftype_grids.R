###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for the grid(s) of surface type.         #
#                 As output we get a list with one raster per year (selected from the             #
#                 closest input available).                                                       #
#                 NOTE: we should never modify the list elements, rather work on copies.          #
###################################################################################################


# Surface type codes: 0 bare ice, 1 firn, 4 rock, 5 debris-covered ice.

func_load_surftype_grids <- function(run_params) {
  
  cat("  Loading surface type grids...\n")
  
  # Here we will put the output.
  grids_out <- list(grids = list(),
                    grid_year_id = rep(NA, run_params$n_years))
  
  grid_paths <- file.path(run_params$dir_data_surftype,
                          paste0(run_params$filename_surftype_prefix,
                                 run_params$surftype_years,
                                 run_params$filename_surftype_suffix))
  
  grid_years <- run_params$surftype_years
  
  # Load grids.
  for (grid_id in 1:length(grid_paths)) {
    grids_out$grids[[grid_id]] <- readAll(raster(grid_paths[grid_id]))
    crs(grids_out$grids[[grid_id]]) <- run_params$grids_crs
  }
  
  # For each modeled year find the closest grid year and use its grid.
  for (year_cur_id in 1:run_params$n_years) {
    year_cur <- run_params$years[year_cur_id]
    grid_year_closest_id <- which.min(abs(grid_years - year_cur))
    grids_out$grid_year_id[year_cur_id] <- grid_year_closest_id
  }
  
  return(grids_out)
  
}
