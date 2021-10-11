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

func_write_year_output <- function(year_data,
                                   run_params,
                                   data_dems,
                                   overview_daily_data) {
  
  cat("\n** Writing year output... **\n")
  
  # Write mass balance maps (rasters).
  writeRaster(year_data$massbal_annual_maps$hydro * run_params$output_mult / 1000, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_hydro_", year_data$year_cur, run_params$output_grid_ext)), overwrite = TRUE)
  if (year_data$nstakes_annual > 0) {
    writeRaster(year_data$massbal_annual_maps$meas_period * run_params$output_mult / 1000, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_measperiod_", year_data$year_cur, run_params$output_grid_ext)), overwrite = TRUE)
    writeRaster(year_data$massbal_annual_maps$meas_period_corr * run_params$output_mult / 1000, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_measperiod_corrected_", year_data$year_cur, run_params$output_grid_ext)), overwrite = TRUE)
  }
  # writeRaster(year_data$massbal_annual_maps$fixed, file.path(run_params$output_dirname, "annual_results", paste0("mb_annual_fixedperiod_", year_data$year_cur, run_params$output_grid_ext)), overwrite = TRUE)
  
  writeRaster(year_data$massbal_winter_maps$fixed * run_params$output_mult / 1000, file.path(run_params$output_dirname, "annual_results", paste0("mb_winter_fixedperiod_", year_data$year_cur, run_params$output_grid_ext)), overwrite = TRUE)
  if (year_data$process_winter) {
    writeRaster(year_data$massbal_winter_maps$meas_period * run_params$output_mult / 1000, file.path(run_params$output_dirname, "annual_results", paste0("mb_winter_measperiod_", year_data$year_cur, run_params$output_grid_ext)), overwrite = TRUE)
  }
  
  # Write used DEM.
  if (run_params$dem_write) {
    writeRaster(data_dems$elevation[[year_data$dem_grid_id]], file.path(run_params$output_dirname, "annual_results", paste0(run_params$filename_dem_prefix, year_data$year_cur, run_params$output_grid_ext)), overwrite = TRUE)
  }
  
  # Write modeled glacier-wide daily mass balance series.
  # NOTE: the cumulative values refer to the value *AT THE BEGINNING* of the respective day.
  # the daily values refer to the value added *OVER* the respective day.
  # Thus, the *LAST* daily value is always 0.0 (that day is not actually
  # simulated, but we have computed a value for its *BEGINNING* (i.e.
  # the end of the previous simulated day), so we report it).
  model_annual_dates <- seq.Date(year_data$model_time_bounds[1], year_data$model_time_bounds[2] + 1, "1 day")
  day_id_offset <- (length(model_annual_dates) - as.integer(format(model_annual_dates[length(model_annual_dates)], "%j"))) + 1
  
  df_annual_daily <- data.frame(date                      = model_annual_dates,
                                day_id                    = seq_along(model_annual_dates) - day_id_offset,
                                gl_massbal_cumul_bandcorr = NA,
                                gl_massbal_cumul          = sprintf(run_params$output_fmt4, year_data$mod_output_annual_cur$gl_massbal_cumul * run_params$output_mult / 1000),
                                gl_accum_cumul            = sprintf(run_params$output_fmt4, year_data$mod_output_annual_cur$gl_accum_cumul * run_params$output_mult / 1000),
                                gl_melt_cumul             = sprintf(run_params$output_fmt4, year_data$mod_output_annual_cur$gl_melt_cumul * run_params$output_mult / 1000),
                                gl_melt_cumul_bandcorr    = NA,
                                gl_melt_daily_m3          = sprintf("%.1f", year_data$mod_output_annual_cur$gl_melt_daily * year_data$glacier_area / 1e3),
                                gl_melt_daily_m3_bandcorr = NA,
                                gl_rainfall_daily_m3      = sprintf("%.1f", year_data$mod_output_annual_cur$gl_rainfall_daily * year_data$glacier_area / 1e3))
  
  if (year_data$nstakes_annual > 0) {
    df_annual_daily$gl_massbal_cumul_bandcorr <- sprintf(run_params$output_fmt4, year_data$mod_output_annual_cur$gl_massbal_cumul_bandcorr * run_params$output_mult / 1000)
    df_annual_daily$gl_melt_cumul_bandcorr    <- sprintf(run_params$output_fmt4, year_data$mod_output_annual_cur$gl_melt_cumul_bandcorr * run_params$output_mult / 1000)
    df_annual_daily$gl_melt_daily_m3_bandcorr <- sprintf("%.1f", c(diff(year_data$mod_output_annual_cur$gl_melt_cumul_bandcorr * year_data$glacier_area / 1e3), 0.0))
  }
  
  
  write.csv(df_annual_daily,
            file.path(run_params$output_dirname, "annual_results", paste0("mb_daily_series_glacier_", year_data$year_cur, ".csv")),
            quote = FALSE,
            row.names = FALSE)
  
  
  # Write modeled daily mass balance series at the stakes.
  if (year_data$nstakes_annual > 0) {
    df_stakes_daily <- data.frame(date   = model_annual_dates,
                                  stakes = apply(year_data$mod_output_annual_cur$stakes_series_mod_all * run_params$output_mult / 1000, 2, sprintf, fmt=run_params$output_fmt4))
    
    names(df_stakes_daily) <- c("date", year_data$massbal_annual_meas_cur$id)
    write.csv(df_stakes_daily,
              file.path(run_params$output_dirname, "annual_results", paste0("mb_daily_series_stakes_", year_data$year_cur, ".csv")),
              quote = FALSE,
              row.names = FALSE)
  }
  
  
  # Write mass balance in vertical bands.
  # Note: we have disabled the fixed annual period,
  # This has changed the indices below from 3:8 to 3:7.
  # Note: df_ele_bands_out already uses the correct unit (mm or m,
  # as chosen by the user). The convertion is done in func_plot_massbal_vs_elevation().
  df_ele_bands_out <- data.frame(year_data$ele_bands_plot_df$ele,
                                 year_data$ele_bands_plot_df$ncells,
                                 apply(year_data$ele_bands_plot_df[,3:7], 2, sprintf, fmt=run_params$output_fmt4))
  names(df_ele_bands_out) <- names(year_data$ele_bands_plot_df)
  write.csv(df_ele_bands_out,
            file.path(run_params$output_dirname, "annual_results", paste0("mb_ele_bands_", year_data$year_cur, ".csv")),
            quote = FALSE,
            row.names = FALSE) 
  
  # Save some values which we will use for the overview plots.
  overview_daily_data$mb_series_all_dates[[year_data$year_id]]              <- model_annual_dates
  if(year_data$nstakes_annual > 0) {
    overview_daily_data$mb_series_all_measperiod_dates[[year_data$year_id]] <- year_data$massbal_annual_meas_period
  } else {
    overview_daily_data$mb_series_all_measperiod_dates[[year_data$year_id]] <- NA
  }
  overview_daily_data$mb_series_all_raw[[year_data$year_id]]                <- year_data$mod_output_annual_cur$gl_massbal_cumul
  
  return(overview_daily_data)
  
}
