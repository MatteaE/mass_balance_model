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
                                   data_dhms,
                                   data_dems,
                                   overview_daily_data) {
  
  cat("\n** Writing year output **\n")
  
  # Mass balance maps (rasters) -------------------------------------------------------------------
  writeRaster(year_data$massbal_annual_maps$hydro * run_params$output_mult / 1000,
              file.path(run_params$out_annual_gridded_dirpath,
                        paste0("mb_annual_hydro_", year_data$year_cur, run_params$output_grid_ext)),
              overwrite = TRUE)
  if (year_data$nstakes_annual > 0) {
    writeRaster(year_data$massbal_annual_maps$meas_period * run_params$output_mult / 1000,
                file.path(run_params$out_annual_gridded_dirpath,
                          paste0("mb_annual_measperiod_", year_data$year_cur, run_params$output_grid_ext)),
                overwrite = TRUE)
    writeRaster(year_data$massbal_annual_maps$meas_period_corr * run_params$output_mult / 1000,
                file.path(run_params$out_annual_gridded_dirpath,
                          paste0("mb_annual_measperiod_corrected_", year_data$year_cur, run_params$output_grid_ext)),
                overwrite = TRUE)
  }

  
  writeRaster(year_data$massbal_winter_maps$fixed * run_params$output_mult / 1000,
              file.path(run_params$out_annual_gridded_dirpath,
                        paste0("mb_winter_fixedperiod_", year_data$year_cur, run_params$output_grid_ext)),
              overwrite = TRUE)
  if (year_data$process_winter) {
    writeRaster(year_data$massbal_winter_maps$meas_period * run_params$output_mult / 1000,
                file.path(run_params$out_annual_gridded_dirpath,
                          paste0("mb_winter_measperiod_", year_data$year_cur, run_params$output_grid_ext)),
                overwrite = TRUE)
  }
  
  # Used DEM --------------------------------------------------------------------------------------
  if (run_params$dem_write) {
    writeRaster(data_dems$elevation[[year_data$dem_grid_id]],
                file.path(run_params$out_annual_gridded_dirpath,
                          paste0(run_params$filename_dem_prefix, year_data$year_cur, run_params$output_grid_ext)),
                overwrite = TRUE)
  }
  
  
  # Maps of snow distribution ---------------------------------------------------------------------
  # Small-scale variability (topographic)
  writeRaster(setValues(data_dhms$elevation[[year_data$dhm_grid_id]],
                        year_data$dist_topographic_values_red),
              file.path(run_params$out_annual_gridded_dirpath,
                        paste0("snowdist_topographic_", year_data$year_cur, run_params$output_grid_ext)),
              overwrite = TRUE)
  
  # Large-scale variability (probes)
  writeRaster(setValues(data_dhms$elevation[[year_data$dhm_grid_id]],
                        year_data$dist_probes_norm_values_red),
              file.path(run_params$out_annual_gridded_dirpath,
                        paste0("snowdist_probes_", year_data$year_cur, run_params$output_grid_ext)),
              overwrite = TRUE)
  
  
  
  # Modeled glacier-wide daily mass balance series ------------------------------------------------
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
                                gl_massbal_cumul          = year_data$mod_output_annual_cur$gl_massbal_cumul * run_params$output_mult / 1000,
                                gl_accum_cumul            = year_data$mod_output_annual_cur$gl_accum_cumul * run_params$output_mult / 1000,
                                gl_melt_cumul             = year_data$mod_output_annual_cur$gl_melt_cumul * run_params$output_mult / 1000,
                                gl_melt_cumul_bandcorr    = NA,
                                gl_melt_daily_m3          = year_data$mod_output_annual_cur$gl_melt_daily * year_data$glacier_area / 1e3,
                                gl_melt_daily_m3_bandcorr = NA,
                                gl_rainfall_daily_m3      = year_data$mod_output_annual_cur$gl_rainfall_daily * year_data$glacier_area / 1e3,
                                gl_scaf                   = year_data$gl_scaf_daily)
  
  
  if (year_data$nstakes_annual > 0) {
    df_annual_daily$gl_massbal_cumul_bandcorr <- year_data$mod_output_annual_cur$gl_massbal_cumul_bandcorr * run_params$output_mult / 1000
    df_annual_daily$gl_melt_cumul_bandcorr    <- year_data$mod_output_annual_cur$gl_melt_cumul_bandcorr * run_params$output_mult / 1000
    df_annual_daily$gl_melt_daily_m3_bandcorr <- c(diff(year_data$mod_output_annual_cur$gl_melt_cumul_bandcorr * year_data$glacier_area / 1e3), 0.0)
  }
  
  
  df_annual_daily_form <- func_format_df_daily(df_annual_daily,
                                               run_params = run_params)
  
  write.csv(df_annual_daily_form,
            file.path(run_params$out_daily_dirpath, paste0("mb_daily_series_glacier_", year_data$year_cur, ".csv")),
            quote = FALSE,
            row.names = FALSE)
  
  
  # Modeled glacier-wide daily mass balance, for hydrological year only ---------------------------
  # This enables comparison of the output files of years with and without measurements
  # and of years with inconsistent measurement date.
  # The cumulative time series are reset to 0 on <YYYY-1>-10-01.
  # If the modeled period is exactly the hydrological year (i.e. year without data or
  # perfect survey dates), this file is the same as mb_daily_series_glacier_<yyyy>.csv".
  date_form <- format(df_annual_daily$date, "%Y%m%d")
  ids_sel <- which(date_form == paste0((year_data$year_cur-1), "1001")):which(date_form == paste0((year_data$year_cur), "1001"))
  
  df_annual_daily_hydro <- df_annual_daily[ids_sel,]
  
  # Reset cumulative time series, for comparability with years where the modeled period is exactly the hydrological year.
  df_annual_daily_hydro$gl_massbal_cumul_bandcorr <- df_annual_daily_hydro$gl_massbal_cumul_bandcorr - df_annual_daily_hydro$gl_massbal_cumul_bandcorr[1]
  df_annual_daily_hydro$gl_massbal_cumul <- df_annual_daily_hydro$gl_massbal_cumul - df_annual_daily_hydro$gl_massbal_cumul[1]
  df_annual_daily_hydro$gl_accum_cumul <- df_annual_daily_hydro$gl_accum_cumul - df_annual_daily_hydro$gl_accum_cumul[1]
  df_annual_daily_hydro$gl_melt_cumul <- df_annual_daily_hydro$gl_melt_cumul - df_annual_daily_hydro$gl_melt_cumul[1]
  df_annual_daily_hydro$gl_melt_cumul_bandcorr <- df_annual_daily_hydro$gl_melt_cumul_bandcorr - df_annual_daily_hydro$gl_melt_cumul_bandcorr[1]
  
  df_annual_daily_hydro_form <- func_format_df_daily(df_annual_daily_hydro,
                                                     run_params = run_params)
  
  write.csv(df_annual_daily_hydro_form,
            file.path(run_params$out_daily_dirpath, paste0("mb_daily_series_glacier_hydro_", year_data$year_cur, ".csv")),
            quote = FALSE,
            row.names = FALSE)
  
  
  
  # Modeled daily mass balance series at the stakes -----------------------------------------------
  if (year_data$nstakes_annual > 0) {
    df_stakes_daily <- data.frame(date   = model_annual_dates,
                                  stakes = year_data$mod_output_annual_cur$stakes_series_mod_all * run_params$output_mult / 1000)
    names(df_stakes_daily) <- c("date", year_data$massbal_annual_meas_cur$id)
    
    df_stakes_daily_form <- data.frame(date   = model_annual_dates,
                                       stakes = apply(year_data$mod_output_annual_cur$stakes_series_mod_all * run_params$output_mult / 1000, 2, sprintf, fmt=run_params$output_fmt4))
    names(df_stakes_daily_form) <- c("date", year_data$massbal_annual_meas_cur$id)
    
    write.csv(df_stakes_daily_form,
              file.path(run_params$out_daily_dirpath, paste0("mb_daily_series_stakes_", year_data$year_cur, ".csv")),
              quote = FALSE,
              row.names = FALSE)
  }
  
  
  
  # Modeled daily mass balance series at the stakes, for hydrological year only -------------------
  # This enables comparison of the output files of years with and without measurements
  # and of years with inconsistent measurement date.
  # The cumulative time series are reset to 0 on <YYYY-1>-10-01.
  if (year_data$nstakes_annual > 0) {
    
    df_stakes_daily_hydro_form <- df_stakes_daily[ids_sel,]
    for (stake_id in 1:(ncol(df_stakes_daily_hydro_form)-1)) {
      df_stakes_daily_hydro_form[,stake_id+1] <- sprintf(run_params$output_fmt4, df_stakes_daily_hydro_form[,stake_id+1] - df_stakes_daily_hydro_form[1,stake_id+1])
    }
    
    write.csv(df_stakes_daily_hydro_form,
              file.path(run_params$out_daily_dirpath, paste0("mb_daily_series_stakes_hydro_", year_data$year_cur, ".csv")),
              quote = FALSE,
              row.names = FALSE)
    
  }
  
  
  # Modeled daily series of cumulative mass balance and SWE at the user-defined points ------------
  if (year_data$npoints_daily > 0) {
    
    dir.create(file.path(run_params$out_daily_dirpath), recursive = TRUE, showWarnings = FALSE)
    
    df_points_daily_mbcumul <- data.frame(date = model_annual_dates,
                                          points = apply(year_data$points_daily_massbal_cumul * run_params$output_mult / 1000, 2, sprintf, fmt=run_params$output_fmt4))
    names(df_points_daily_mbcumul) <- c("date", year_data$points_daily_out$id)
    write.csv(df_points_daily_mbcumul,
              file.path(run_params$out_daily_dirpath, paste0("mb_daily_series_points_", year_data$year_cur, ".csv")),
              quote = FALSE,
              row.names = FALSE)
    
    df_points_daily_swe <- data.frame(date = model_annual_dates,
                                      points = apply(year_data$points_daily_swe * run_params$output_mult / 1000, 2, sprintf, fmt=run_params$output_fmt4))
    names(df_points_daily_swe) <- c("date", year_data$points_daily_out$id)
    write.csv(df_points_daily_swe,
              file.path(run_params$out_daily_dirpath, paste0("swe_daily_series_points_", year_data$year_cur, ".csv")),
              quote = FALSE,
              row.names = FALSE)
  }
  
  
  
  # Annual mass balance in vertical bands ---------------------------------------------------------
  # Note: we have disabled the fixed annual period,
  # This has changed the indices below from 4:9 to 4:8.
  # Note: df_ele_bands_out already uses the correct unit (mm or m,
  # as chosen by the user). The convertion is done in func_plot_massbal_vs_elevation().
  df_ele_bands_out <- data.frame(year_data$ele_bands_plot_df$ele,
                                 year_data$ele_bands_plot_df$ncells,
                                 sprintf("%.4f",year_data$ele_bands_plot_df$area_km2),
                                 apply(year_data$ele_bands_plot_df[,4:8], 2, sprintf, fmt=run_params$output_fmt4))
  names(df_ele_bands_out) <- names(year_data$ele_bands_plot_df)
  write.csv(df_ele_bands_out,
            file.path(run_params$output_dirname, "annual_results", paste0("mb_ele_bands_", year_data$year_cur, ".csv")),
            quote = FALSE,
            row.names = FALSE) 
  
  
  # LOO validation results ------------------------------------------------------------------------
  if (year_data$run_loo_logi) {
    
    df_loo_out                <- year_data$df_loo_out[,setdiff(names(year_data$df_loo_out), "stake_id")]
    df_loo_out$stake_loo_bias <- sprintf(run_params$output_fmt4, df_loo_out$stake_loo_bias * run_params$output_mult/1e3)
    df_loo_out$loo_corr_fact  <- sprintf("%.5f", df_loo_out$loo_corr_fact)
    write.csv(df_loo_out,
              file.path(run_params$output_dirname, "annual_results", paste0("loo_validation_", year_data$year_cur, ".csv")),
              quote = FALSE,
              row.names = FALSE)
    
  }
  
  
  # Glacier-wide mass balance of all model realizations -------------------------------------------
  df_smb_all_out                      <- year_data$df_runs_smb[,c("run_id", "corr_fact", "run_type", "mb_annual_hydro", "mb_annual_measperiod")]
  df_smb_all_out$mb_annual_hydro      <- sprintf(run_params$output_fmt4, df_smb_all_out$mb_annual_hydro * run_params$output_mult/1e3)
  df_smb_all_out$mb_annual_measperiod <- sprintf(run_params$output_fmt4, df_smb_all_out$mb_annual_measperiod * run_params$output_mult/1e3)
  df_smb_all_out$corr_fact            <- sprintf("%.5f", df_smb_all_out$corr_fact)
  
  write.csv(df_smb_all_out,
            file.path(run_params$output_dirname, "annual_results", paste0("mb_all_runs_", year_data$year_cur, ".csv")),
            quote = FALSE,
            row.names = FALSE)
  
  
  
  # Save some values which we will use for the overview plots -------------------------------------
  overview_daily_data$mb_series_all_dates[[year_data$year_id]]              <- model_annual_dates
  if (year_data$nstakes_annual > 0) {
    overview_daily_data$mb_series_all_measperiod_dates[[year_data$year_id]] <- year_data$massbal_annual_meas_period
  } else {
    overview_daily_data$mb_series_all_measperiod_dates[[year_data$year_id]] <- NA
  }
  overview_daily_data$mb_series_all_raw[[year_data$year_id]]                <- year_data$mod_output_annual_cur$gl_massbal_cumul
  
  return(overview_daily_data)
  
}
