###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to correct the map of annual mass balance        #
#                 based on user-defined elevation bands.                                          #
#                 We do this for the mass balance modeled over the measurement period.            #
#                 This will be inaccurate if there are very different dates of stake              #
#                 measurements (e.g. stakes measured in two different campaigns of a same         #
#                 summer), because the correction disregards the time periods corresponding to    #
#                 the given bias.                                                                 #
################################################################################################### 

func_correct_massbal_elebands <- function(year_cur_params, data_dems, massbal_annual_meas_cur, mod_output_annual_cur, massbal_maps, dem_grid_id) {

  
  # First find the DEM elevation of each stake (we base the correction on this
  # instead of the reported stake elevation, for consistency in the processing).
  annual_stake_dem_elevations <- extract(data_dems$elevation[[dem_grid_id]], as.matrix(massbal_annual_meas_cur[,4:5]), method = "bilinear")
  
  # Compute model bias within each band.
  # We create two virtual bands with midpoints at the
  # lowest and highest band limits, to let the linear
  # interpolation work also in the two extreme bands.
  # Points in these two bands themselves are ignored.
  # These two bands are NOT counted in nbands.
  # This is the same as the IDL implementation.
  nbands         <- length(year_cur_params$mb_corr_ele_bands) - 1
  band_lower     <- year_cur_params$mb_corr_ele_bands[1:nbands]
  band_upper     <- year_cur_params$mb_corr_ele_bands[2:(nbands+1)]
  band_midpoints <- c(year_cur_params$mb_corr_ele_bands[1], (band_lower + band_upper) / 2, year_cur_params$mb_corr_ele_bands[nbands + 1])
  band_biases    <- numeric(nbands)
  
  for (band_id in 1:nbands) {
    band_stake_ids          <- which((annual_stake_dem_elevations > band_lower[band_id]) & (annual_stake_dem_elevations <= band_upper[band_id]))
    band_biases[band_id]    <- mean(mod_output_annual_cur$stakes_bias[band_stake_ids])
  }
  band_biases <- as.numeric(interpNA(timeSeries(band_biases), method = "linear")) # If a correction bands contains no stakes, we linearly interpolate its bias from the two surrounding bands.
  band_biases <- c(0, band_biases, 0) # The two virtual bands have 0 bias. 
  
  # Linear interpolation of bias over elevation, between the two
  # band midpoints surrounding each glaciated grid point.
  # We select the cells between two band midpoints
  # (not equal to all cells within a single band!).
  meas_period_corr <- massbal_maps$meas_period
  dem_values_cur    <- getValues(data_dems$elevation[[dem_grid_id]])
  for (band_id in 2:(nbands+2)) { # This index is relative to all bands including the two virtual ones!
    gl_cells_cur <- which((dem_values_cur <= band_midpoints[band_id]) & (dem_values_cur > band_midpoints[band_id - 1]))
    meas_period_corr[gl_cells_cur] <- meas_period_corr[gl_cells_cur] - band_biases[band_id - 1] - (band_biases[band_id] - band_biases[band_id - 1]) * ((dem_values_cur[gl_cells_cur] - band_midpoints[band_id - 1]) / (band_midpoints[band_id] - band_midpoints[band_id - 1]))
  }
  
  return(meas_period_corr)
  
}
