###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to write the output grids and time series           #
#                 after modeling a year.                                                          #
#                 It also saves (end of the file) some variables                                  #
#                 used later for the overview plots.                                              #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

cat("** Writing year output... **\n")

# Write mass balance maps (rasters).
writeRaster(massbal_annual_maps$hydro, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_hydro_", year_cur, run_params$output_grid_ext)), overwrite = TRUE)
writeRaster(massbal_annual_maps$meas_period, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_measperiod_", year_cur, run_params$output_grid_ext)), overwrite = TRUE)
writeRaster(massbal_annual_maps$meas_period_corr, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_final_", year_cur, run_params$output_grid_ext)), overwrite = TRUE)
writeRaster(massbal_annual_maps$fixed, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_fixedperiod_", year_cur, run_params$output_grid_ext)), overwrite = TRUE)

writeRaster(massbal_winter_maps$fixed, file.path(run_params$output_dirname, "annual_results", paste0("mb_winter_fixedperiod_", year_cur, run_params$output_grid_ext)), overwrite = TRUE)
if (process_winter) {
  writeRaster(massbal_winter_maps$meas_period, file.path(run_params$output_dirname, "annual_results", paste0("mb_winter_measperiod_", year_cur, run_params$output_grid_ext)), overwrite = TRUE)
}

# Write used DEM.
if (run_params$dem_write) {
  writeRaster(data_dems$elevation[[dem_grid_id]], file.path(run_params$output_dirname, "annual_results", paste0(run_params$filename_dem_prefix, year_cur, run_params$output_grid_ext)), overwrite = TRUE)
}

# Write modeled glacier-wide daily mass balance series.
model_annual_dates <- seq.Date(model_time_bounds[1], model_time_bounds[2] + 1, "1 day")
day_id_offset <- (length(model_annual_dates) - as.integer(format(model_annual_dates[length(model_annual_dates)], "%j"))) + 1
df_annual_daily <- data.frame(date   = model_annual_dates,
                              day_id = seq_along(model_annual_dates) - day_id_offset,
                              gl_massbal_bandcorr = sprintf("%.1f", mod_output_annual_cur$gl_massbal_cumul_bandcorr),
                              gl_massbal = sprintf("%.1f", mod_output_annual_cur$gl_massbal_cumul),
                              gl_accum = sprintf("%.1f", mod_output_annual_cur$gl_accum_cumul),
                              gl_melt = sprintf("%.1f", mod_output_annual_cur$gl_melt_cumul),
                              gl_melt_bandcorr = sprintf("%.1f", mod_output_annual_cur$gl_melt_cumul_bandcorr))
write.csv(df_annual_daily,
          file.path(run_params$output_dirname, "annual_results", paste0("mb_daily_series_glacier_", year_cur, ".csv")),
          quote = FALSE,
          row.names = FALSE)


# Write modeled daily mass balance series at the stakes.
df_stakes_daily <- data.frame(date = model_annual_dates,
                              stakes = apply(mod_output_annual_cur$stakes_series_mod_all, 2, sprintf, fmt="%.1f"))

names(df_stakes_daily) <- c("date", massbal_annual_meas_cur$id)
write.csv(df_stakes_daily,
          file.path(run_params$output_dirname, "annual_results", paste0("mb_daily_series_stakes_", year_cur, ".csv")),
          quote = FALSE,
          row.names = FALSE)


# Write mass balance in vertical bands.
df_ele_bands_out <- data.frame(ele_bands_plot_df$ele,
                               ele_bands_plot_df$ncells,
                               apply(ele_bands_plot_df[,3:8], 2, sprintf, fmt="%.1f"))
names(df_ele_bands_out) <- names(ele_bands_plot_df)
write.csv(df_ele_bands_out,
          file.path(run_params$output_dirname, "annual_results", paste0("mb_ele_bands_", year_cur, ".csv")),
          quote = FALSE,
          row.names = FALSE) 

# Save some values which we will use for the overview plots.
mb_series_all_dates[[year_id]] <- model_annual_dates
mb_series_all_measperiod_dates[[year_id]] <- massbal_annual_maps_data$meas_period
mb_series_all_raw[[year_id]] <- mod_output_annual_cur$gl_massbal_cumul
