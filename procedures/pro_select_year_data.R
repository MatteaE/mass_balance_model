###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to select the grids and mass balance measurements   #
#                 of the current year.                                                            #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

# Select grids of the current year from the list of available grids.
# We could be using different ids for DEM and DHM (since DEM = DHM + outline, it depends
# on the available outlines); and also w.r.t. surface type because the
# elevation grids can also be interpolated annually (unlike surface type).
# So we have different _id variables.
# The fixed avalanche grids use the same indices as the DHM ones.
dhm_grid_id       <- data_dhms$grid_year_id[year_id]
dem_grid_id       <- data_dems$grid_year_id[year_id]
surftype_grid_id  <- data_surftype$grid_year_id[year_id]
outline_id        <- data_outlines$outline_year_id[year_id]

# Extract avalanche grids for this year
# (pre-computed before the start of the loop).
grids_avalanche_cur <- sapply(grids_avalanche, `[[`, dhm_grid_id)

# Compute reduced-intensity base topographic distribution of solid precipitation.
dist_topographic_values      <- getValues(grids_snowdist_topographic[[dem_grid_id]])
dist_topographic_values_mean <- mean(dist_topographic_values)
dist_topographic_values_red  <- dist_topographic_values_mean + run_params$accum_snow_dist_red_fac * (dist_topographic_values - dist_topographic_values_mean)

# Extract ice albedo factor grid for this year.
grid_ice_albedo_fact_cur_values <- getValues(grids_ice_albedo_fact[[dhm_grid_id]])

# Select mass balance measurements of the current year.
massbal_annual_ids <- func_select_year_measurements(data_massbalance_annual, year_cur)
nstakes_annual <- length(massbal_annual_ids)
massbal_winter_ids <- func_select_year_measurements(data_massbalance_winter, year_cur)
nstakes_winter <- length(massbal_winter_ids)
massbal_annual_meas_cur <- data_massbalance_annual[massbal_annual_ids,]
massbal_winter_meas_cur <- data_massbalance_winter[massbal_winter_ids,] # Empty if we have no winter stakes for the year.
