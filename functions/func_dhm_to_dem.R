###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which computes DEM grids from DHM and outline.   #
#                 A DHM is an elevation model which has no NA cells.                              #
#                 A DEM is an elevation model which has NA outside of the glacier.                #
###################################################################################################  


# NOTE: depending on which outlines we have, we may have several DEMs
# derived from a single DHM (with different cropping).
# This means that elevation_grid_id is no longer enough,
# we need to have dhm_grid_id and dem_grid_id.

func_dhm_to_dem <- function(run_params,
                            data_dhms,
                            data_outlines) {
  
  data_dems <- list(elevation = list(),
                    grid_year_id = rep(NA, run_params$n_years),
                    glacier_cell_ids = list(),
                    no_glacier_cell_ids = list(),
                    elevation_bands_ela = list(),
                    elevation_bands_plot = list())

  # Algorithm:
  #   find all unique combinations of dhms/outlines which are actually present:
  #     unique(paste(data_dhms$grid_year_id, data_outlines$outline_year_id))
  #   for each combination,
  #     compute the corresponding DEM: crop DHM with the outline
  #     save the DEM to the DEM grids list
  #     find the years the combination applies to
  #     set the combination index to the years
  #     pre-compute valid glaciated cells and elevation bands.
  # NOTE: this case is general (i.e. it also accounts for the previous ones)
  # so we can implement just this one.
  dhm_outline_combinations <- paste(data_dhms$grid_year_id, data_outlines$outline_year_id)
  dhm_outline_combinations_unique <- unique(dhm_outline_combinations)
  data_dems$n_grids <- length(dhm_outline_combinations_unique)
  data_dems$dhm_id <- rep(NA, data_dems$n_grids)
  
  for (dem_id in 1:data_dems$n_grids) {
    
    dhm_id <- as.integer(str_split(dhm_outline_combinations_unique[dem_id], fixed(" "))[[1]][1])
    outline_id <- as.integer(str_split(dhm_outline_combinations_unique[dem_id], fixed(" "))[[1]][2])
    dem_cur <- rasterize(data_outlines$outlines[[outline_id]], data_dhms$elevation[[dhm_id]], mask = TRUE)
    data_dems$elevation[[dem_id]] <- dem_cur
    dem_cur_values <- getValues(dem_cur)
    cur_combination_year_ids <- which(dhm_outline_combinations == dhm_outline_combinations_unique[dem_id])
    data_dems$grid_year_id[cur_combination_year_ids] <- dem_id
    
    # Each DEM grid can have only a single corresponding DHM
    # (but not vice-versa, because of possibly different outline polygons).
    # So we find the DHM associated to each DEM.
    data_dems$dhm_id[dem_id] <- dhm_id
    
    # We also pre-compute the glaciated/non-glaciated cells,
    # and also a re-classified raster with elevation
    # classified in user-defined elevation bands (useful for
    # the calculation of the equilibrium line altitude).
      
    # Glaciated and non-glaciated cells.
    glacier_ids_logi                        <- is.na(dem_cur_values)
    data_dems$glacier_cell_ids[[dem_id]]    <- which(!glacier_ids_logi)
    data_dems$no_glacier_cell_ids[[dem_id]] <- which(glacier_ids_logi)
    
    # Elevation bands for the ELA estimation (user-defined vertical extent, typically 10 m).
    data_dems$elevation_bands_ela[[dem_id]] <- round(dem_cur / run_params$ele_bands_ela_size) * run_params$ele_bands_ela_size
    
    # Elevation bands for the plot of modeled mass balance vs
    # elevation bands (user-defined vertical extent, typically 50 m).
    # We center them at extent/2 so that the band limits are nice.
    data_dems$elevation_bands_plot[[dem_id]] <- round((data_dems$elevation[[dem_id]] - run_params$ele_bands_plot_size/2) / run_params$ele_bands_plot_size) * run_params$ele_bands_plot_size + run_params$ele_bands_plot_size/2
      
  }
  
  return(data_dems)
  
}
