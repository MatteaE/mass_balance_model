###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the IDW interpolation of snow probing data, to supplement    #
#                 with measurements the topographical snow distribution of elevation, aspect      #
#                 and avalanches.                                                                 #
###################################################################################################


func_snowdist_from_probes <- function(year_data,
                                      run_params,
                                      data_dhms) {
  
  # SWE is in m w.e.
  snow_probes_df <- data.frame(x   = year_data$massbal_winter_meas_cur$x,
                               y   = year_data$massbal_winter_meas_cur$y,
                               swe = year_data$massbal_winter_meas_cur$massbal / 1e3)
  
  
  # Decide on which type of interpolation to use.
  # If there are fewer than run_params$probes_snowdist_search_npoints_min
  # points of snow measurement, stick to global IDW.
  idw_sel <- run_params$probes_snowdist_idw_type
  if (nrow(snow_probes_df) < run_params$probes_snowdist_search_npoints_min) {
    idw_sel <- "global"
    func_customlog("There are fewer than the required ",
                   run_params$probes_snowdist_search_npoints_min,
                   " winter measurements for the interpolation of snow distribution -- must use global IDW", level = 1)
  }
  
  # Global, traditional IDW via gstat, with prescribed distance exponent.
  if (idw_sel == "global") {
    
    snowdist_idw <- func_snow_probes_idw_global(snow_probes_df,
                                                data_dhms$elevation[[year_data$dhm_grid_id]],
                                                run_params)
    
    # Adaptive IDW as in the IDL implementation
  } else {
    snowdist_idw <- func_snow_probes_idw_adaptive(snow_probes_df,
                                                  data_dhms$elevation[[year_data$dhm_grid_id]],
                                                  run_params)
  }
  
  
  # writeRaster(snowdist_idw, "snowdist_idw.tif", overwrite = T)
  
  # Smooth as in the original IDL implementation.
  snowdist_idw_smooth <- focal(snowdist_idw, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, expand = FALSE, fillvalue = NA)
  
  # writeRaster(snowdist_idw_smooth, "snowdist_idw_smooth.tif", overwrite = T)
  
  snowdist_idw_smooth_clamp <- clamp(snowdist_idw_smooth, lower = 0, upper = Inf, values = TRUE)
  
  return(snowdist_idw_smooth_clamp)
}
