###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the IDW interpolation of snow probing data, to supplement    #
#                 with measurements the topographical snow distribution of elevation, aspect      #
#                 and avalanches.                                                                 #
###################################################################################################


# snow_probes is an annual subset of data_massbalance_winter.
func_snow_probes_idw <- function(run_params, snow_probes, data_dhms) {
  
  snow_probes_df <- data.frame(x = snow_probes[,4],
                               y = snow_probes[,5],
                               swe = snow_probes$massbal / 1e3)

  # Use prescribed distance exponent.
  gs <- gstat(formula=swe~1, data=snow_probes_df, set=list(idp=run_params$snow_probes_idw_exp), locations = ~x+y)
  snowdist_idw <- terra::interpolate(data_dhms$elevation[[1]], gs, debug.level = 0)
  
  # writeRaster(snowdist_idw, "snowdist_idw.tif", overwrite = T)
  
  # Smooth as in the original IDL implementation.
  snowdist_idw_smooth <- focal(snowdist_idw, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, expand = FALSE, fillvalue = NA)
  
  # writeRaster(snowdist_idw_smooth, "snowdist_idw_smooth.tif", overwrite = T)
  
  return(snowdist_idw_smooth)
}
