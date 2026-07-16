###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to write daily grids of SWE to geotiff.             #
###################################################################################################


func_write_daily_grids <- function(year_data,
                                   run_params,
                                   data_dems) {
  
  
  dir.create(file.path(run_params$output_dirname, "daily", year_data$year_cur, "swe"), recursive = TRUE, showWarnings = FALSE)
  
  plot_df <- data.frame(crds(data_dems$elevation[[year_data$dem_grid_id]], na.rm = FALSE))
  
  # Daily loop to produce the plots.
  # Optionally reduced frequency (e.g. weekly).
  for (day_id in 1:(year_data$model_annual_days_n + 1)) {
    
    # Plot only one every few days, to speed up.
    if (!(day_id %% run_params$write_daily_grids_frequency)) {
      
      cat("\r** Writing daily grids of SWE...", day_id, "/", year_data$model_annual_days_n+1, "**")
      cells_cur <- (day_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)
      swe_cur_r <- setValues(data_dems$elevation[[year_data$dem_grid_id]],
                             year_data$mod_output_annual_cur$vec_swe_all[cells_cur])
      writeRaster(swe_cur_r,
                  file.path(run_params$output_dirname, "daily", year_data$year_cur, "swe", paste0(sprintf("%03d", day_id), ".tif")),
                  overwrite = TRUE)
      
    } # End selection on day_id to plot only one day every few.
    
  } # End daily loop to plot SWE and surface type.
  
  cat("\n")  
}
