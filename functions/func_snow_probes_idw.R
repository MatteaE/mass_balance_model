###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the IDW interpolation of snow probing data, to supplement    #
#                 with measurements the topographical snow distribution of elevation, aspect      #
#                 and avalanches.                                                                 #
###################################################################################################


func_snow_probes_idw <- function(year_data,
                                 run_params,
                                 data_dhms) {
  
  # SWE is in m w.e.
  snow_probes_df <- data.frame(x   = year_data$massbal_winter_meas_cur$x,
                               y   = year_data$massbal_winter_meas_cur$y,
                               swe = year_data$massbal_winter_meas_cur$massbal / 1e3)

  # Use prescribed distance exponent.
  gs <- gstat(formula=swe~1, data=snow_probes_df, set=list(idp=run_params$snow_probes_idw_exp), locations = ~x+y)
  snowdist_idw <- terra::interpolate(data_dhms$elevation[[year_data$dhm_grid_id]], gs, debug.level = 0)
  
  # writeRaster(snowdist_idw, "snowdist_idw.tif", overwrite = T)
  
  # Smooth as in the original IDL implementation.
  snowdist_idw_smooth <- focal(snowdist_idw, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, expand = FALSE, fillvalue = NA)
  
  # writeRaster(snowdist_idw_smooth, "snowdist_idw_smooth.tif", overwrite = T)
  
  return(snowdist_idw_smooth)
}
