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
    func_customlog("Year ", year_data$year_cur, ": there are fewer than the required ",
                   run_params$probes_snowdist_search_npoints_min,
                   " winter measurements for the interpolation of snow distribution -- must use global IDW", level = 1)
  }
  
  # Global, traditional IDW via gstat, with prescribed distance exponent.
  # Note: the result is NOT normalized yet.
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
  
  
  # We enforce a complete map of snow distribution.
  if (anyNA(values(snowdist_idw, mat = F))) {
    func_customlog("Year ", year_data$year_cur, ": there are NA values in the calculated map of snow distribution, please investigate!", level = 2)
    func_stop()
  }
  
  
  # Smooth with Gaussian matrix (better than square window).
  # If the matrix has just one element (too little smoothing),
  # don't do it (otherwise, focal() fails).
  fw_mat <- focalMat(snowdist_idw,
                     d = run_params$probes_snowdist_smooth_dist,
                     type = "Gauss")
  if ((nrow(fw_mat) > 1) && (ncol(fw_mat) > 1)) {
    cat("  Smoothing with", ncol(fw_mat), "x", nrow(fw_mat), "Gaussian matrix...\n")
    snowdist_idw_smooth <- focal(snowdist_idw,
                                 w = fw_mat, fun = mean, na.rm = TRUE, expand = FALSE, fillvalue = NA)
  } else {
    snowdist_idw_smooth <- snowdist_idw
  }
  # writeRaster(snowdist_idw_smooth, "snowdist_idw_smooth.tif", overwrite = T)
  
  snowdist_idw_smooth_clamp <- clamp(snowdist_idw_smooth, lower = 0, upper = Inf, values = TRUE)
  
  return(snowdist_idw_smooth_clamp)
}
