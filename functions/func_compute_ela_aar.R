###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the equilibrium line altitude and     #
#                 the accumulation-area ratio (ELA and AAR) after modeling one year.              #
################################################################################################### 


# The equilibrium line altitude is computed by classifying
# the glacier grid into elevation bands (with user-defined
# vertical extent) and then taking the band whose mean mass
# balance over the corrected measured period is closest to 0.
# NOTE: these bands (typically 10 vertical meters wide)
# are NOT the same as for the correction of modeled mass
# balance in elevation bands (glacier-dependent bands),
# and NOT the same as for the plot of modeled mass balance
# vs elevation bands (typically 40 vertical meters wide).
func_compute_ela_aar <- function(year_data,
                                 run_params,
                                 data_dems) {
  
  ele_bands_values <- getValues(data_dems$elevation_bands_ela[[year_data$dem_grid_id]])
  ele_bands_min <- min(ele_bands_values, na.rm = T)
  ele_bands_max <- max(ele_bands_values, na.rm = T)
  ele_bands_df <- data.frame(ele = seq(ele_bands_min, ele_bands_max, run_params$ele_bands_ela_size),
                             mb_hydro = NA)
                             # mb_corr = NA)
  
  mb_hydro_map_values <- getValues(year_data$massbal_annual_maps$hydro)
  
  for (band_id in 1:nrow(ele_bands_df)) {
    ele_bands_df$mb_hydro[band_id] <- mean(mb_hydro_map_values[ele_bands_values == ele_bands_df$ele[band_id]], na.rm=T)
  }
  ela_band_id <- which.min(abs(ele_bands_df$mb_hydro))
  ela <- ele_bands_df$ele[ela_band_id]
  
  aar <- length(which(mb_hydro_map_values >= 0)) / length(data_dems$glacier_cell_ids[[year_data$dem_grid_id]])
  
  return(c(ela = ela, aar = aar))
  
}
