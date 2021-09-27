###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to select the grids and mass balance measurements   #
#                 of the current year.                                                            #
###################################################################################################


func_select_year_data <- function(data_all,
                                  grids_static_list,
                                  year_id,
                                  run_params) {
                                  
  # Here we put all this year's data,
  #and we return this list at the end of the function.
  year_data                                 <- list()
  
  year_data$year_id                         <- year_id
  year_data$year_cur                        <- run_params$years[year_id]
  
  # Select grids of the current year from the list of available grids.
  # We could be using different ids for DEM and DHM (since DEM = DHM + outline, it depends
  # on the available outlines); and also w.r.t. surface type because the
  # elevation grids can also be interpolated annually (unlike surface type).
  # So we have different _id variables.
  # The static avalanche grids use the same indices as the DHM ones.
  year_data$dhm_grid_id                     <- data_all$data_dhms$grid_year_id[year_id]
  year_data$dem_grid_id                     <- data_all$data_dems$grid_year_id[year_id]
  year_data$surftype_grid_id                <- data_all$data_surftype$grid_year_id[year_id]
  year_data$outline_id                      <- data_all$data_outlines$outline_year_id[year_id]
  
  # Extract avalanche grids for this year
  # (pre-computed before the start of the loop).
  year_data$grids_avalanche_cur             <- sapply(grids_static_list$grids_avalanche, `[[`, year_data$dhm_grid_id)
  
  # Compute reduced-intensity base topographic distribution of solid precipitation.
  dist_topographic_values                   <- getValues(grids_static_list$grids_snowdist_topographic[[year_data$dem_grid_id]])
  dist_topographic_values_mean              <- mean(dist_topographic_values)
  year_data$dist_topographic_values_red     <- dist_topographic_values_mean + run_params$accum_snow_dist_red_fac * (dist_topographic_values - dist_topographic_values_mean)
  
  # Extract ice albedo factor grid for this year.
  year_data$grid_ice_albedo_fact_cur_values <- getValues(grids_static_list$grids_ice_albedo_fact[[year_data$dhm_grid_id]])
  
  # Select mass balance measurements of the current year.
  massbal_annual_ids                        <- func_select_year_mb_measurements(data_all$data_massbalance_annual, year_data$year_cur)
  massbal_winter_ids                        <- func_select_year_mb_measurements(data_all$data_massbalance_winter, year_data$year_cur)
  year_data$massbal_annual_meas_cur         <- data_all$data_massbalance_annual[massbal_annual_ids,] # Empty if we have no annual stakes for the year.
  year_data$massbal_winter_meas_cur         <- data_all$data_massbalance_winter[massbal_winter_ids,] # Empty if we have no winter stakes for the year.
  
  
  # Add DEM elevation of the stakes, we use it
  # instead of reported stake elevation.
  # If a stake falls outside the DEM (glaciated)
  # cells, discard it with a warning.
  year_data$massbal_annual_meas_cur$z_dem   <- extract(data_all$data_dems$elevation[[year_data$dem_grid_id]], as.matrix(year_data$massbal_annual_meas_cur[,4:5]), method = "bilinear")
  
  stakes_annual_outside_ids <- which(is.na(year_data$massbal_annual_meas_cur$z_dem))
  stakes_annual_outside_n   <- length(stakes_annual_outside_ids)
  if (stakes_annual_outside_n > 0) {
    year_data$massbal_annual_meas_cur <- year_data$massbal_annual_meas_cur[-stakes_annual_outside_ids,]
    message(paste0("WARNING: there are ", stakes_annual_outside_n, " annual stakes which are outside the glacier outline! I am discarding them, but you should investigate!"))
  }
  
  year_data$massbal_winter_meas_cur$z_dem   <- extract(data_all$data_dems$elevation[[year_data$dem_grid_id]], as.matrix(year_data$massbal_winter_meas_cur[,4:5]), method = "bilinear")
  stakes_winter_outside_ids <- which(is.na(year_data$massbal_winter_meas_cur$z_dem))
  stakes_winter_outside_n   <- length(stakes_winter_outside_ids)
  if (stakes_winter_outside_n > 0) {
    year_data$massbal_winter_meas_cur <- year_data$massbal_winter_meas_cur[-stakes_winter_outside_ids,]
    message(paste0("WARNING: there are ", stakes_winter_outside_n, " winter stakes which are outside the glacier outline! I am discarding them, but you should investigate!"))
  }
  
  year_data$nstakes_annual   <- nrow(year_data$massbal_annual_meas_cur)
  year_data$nstakes_winter   <- nrow(year_data$massbal_winter_meas_cur)
  
  
  return(year_data)
}
