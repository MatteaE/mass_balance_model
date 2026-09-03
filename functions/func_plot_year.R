###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to produce all plots from a year.                   #
###################################################################################################



func_plot_year <- function(year_data,
                           year_cur_params,
                           run_params,
                           data_dems,
                           data_dhms,
                           data_outlines) {
  
  cat("\n** Producing year plots **\n")
  
  
  # time_v <- as.POSIXct(rep(NA_real_, 16))  # To measure elapsed times.
  
  # time_v[1] <- Sys.time()
  cat("  Common map elements...\n")
  plots_map_common_elements <- func_plot_map_common_elements(year_data,
                                                             data_dems,
                                                             data_dhms,
                                                             data_outlines)
  
  
  # Plot the mass balance maps --------------------------------------------------------------------
  # This returns a list with the (5 or 6, depending on whether we have winter measurements)
  # mass balance maps for the current year.
  # Then we will append to this list also the
  # other plots of the year (time series,
  # vertical distributions and so on).
  # time_v[2] <- Sys.time()
  cat("  Mass balance maps...\n")
  plots_year <- func_plot_year_mb_maps(year_data,
                                       run_params,
                                       data_dems,
                                       data_outlines,
                                       plots_map_common_elements)
  
  
  # Plot the stake weights (Voronoi cells) --------------------------------------------------------
  # time_v[3] <- Sys.time()
  if (year_data$nstakes_annual > 0) {
    cat("  Mass balance weights...\n")
    plots_weights <- func_plot_voronoi(year_data,
                                       run_params,
                                       data_dems,
                                       data_outlines,
                                       plots_map_common_elements)
    plots_year <- append(plots_year, plots_weights)
  }
  
  
  # Plot the SWE maps -----------------------------------------------------------------------------
  # time_v[4] <- Sys.time()
  cat("  SWE maps...\n")
  plots_swe <- func_plot_year_swe_maps(year_data,
                                       run_params,
                                       data_dhms,
                                       data_dems,
                                       data_outlines,
                                       plots_map_common_elements)
  plots_year <- append(plots_year, plots_swe)
  
  
  # Plot the map of avalanche effect --------------------------------------------------------------
  # time_v[5] <- Sys.time()
  cat("  Avalanche map...\n")
  plots_avalanche <- func_plot_avalanche_net_effect(year_data,
                                                    run_params,
                                                    data_dhms,
                                                    data_dems,
                                                    data_outlines,
                                                    plots_map_common_elements)
  plots_year <- append(plots_year, plots_avalanche)
  
  
  # Plot the map of snow cover duration (hydrological year) ---------------------------------------
  # time_v[6] <- Sys.time()
  cat("  Snow cover duration...\n")
  plots_snow_duration <- func_plot_snowcover_duration(year_data,
                                                      year_cur_params,
                                                      run_params,
                                                      data_dhms,
                                                      data_dems,
                                                      data_outlines,
                                                      plots_map_common_elements)
  plots_year <- append(plots_year, plots_snow_duration)
  
  
  # Plot the map of snowfall distribution ---------------------------------------------------------
  # time_v[7] <- Sys.time()
  cat("  Snowfall distribution map...\n")
  plots_snowdist <- func_plot_year_snowdist_map(year_data,
                                                run_params,
                                                data_dhms,
                                                data_outlines,
                                                plots_map_common_elements)
  plots_year <- append(plots_year, plots_snowdist)
  
  
  
  # Plot the daily meteorological series ----------------------------------------------------------
  # This also plots the result of prec_corr/100 * prec_summer_fact, that is, the daily correction to the precipitation series.
  # time_v[8] <- Sys.time()
  cat("  Meteorological series...\n")
  plot_weather_series <- func_plot_weather_series(year_data,
                                                  year_cur_params,
                                                  run_params)
  plots_year <- append(plots_year, list(plot_weather_series))
  
  
  
  # Plot the daily time series of glacier-wide mass balance ---------------------------------------
  # time_v[9] <- Sys.time()
  cat("  Mass balance time series...\n")
  plots_mb_cumul <- func_plot_massbal_cumul(year_data,
                                            run_params)
  plots_year <- append(plots_year, list(plots_mb_cumul))
  
  
  
  # Plot the daily time series of snow-covered area fraction --------------------------------------
  # time_v[10] <- Sys.time()
  cat("  Snow-covered area fraction...\n")
  plot_scaf <- func_plot_scaf(year_data,
                              run_params)
  plots_year <- append(plots_year, list(plot_scaf))
  
  
  # Plot scatterplots of model bias at the stakes -------------------------------------------------
  # Plots of bias vs elevation and vs accumulation multiplier, useful to manually inspect / improve the RMS.
  # time_v[11] <- Sys.time()
  if (year_data$nstakes_annual > 0) {
    cat("  Bias scatterplots...\n")
    plots_bias_scatterplots <- func_plot_bias_scatterplots(year_data,
                                                           data_dhms,
                                                           run_params)
    # Combine with SCAF plot, on the same page.
    plots_year[[length(plots_year)]] <- plot_grid(plotlist = list(plots_year[[length(plots_year)]],
                                                                  plots_bias_scatterplots[[1]],
                                                                  plots_bias_scatterplots[[2]]),
                                                  align = "v", ncol = 1, nrow = 3)
    # If no annual stakes are present, ensure that the SCAF plot does not get vertically warped to the full page.
  } else {
    plots_year[[length(plots_year)]] <- plot_grid(plotlist = plots_year[length(plots_year)],
                                                  align = "v", ncol = 1, nrow = 3)
  }
  
  
  # Plot mass balance versus elevation ------------------------------------------------------------
  # time_v[12] <- Sys.time()
  cat("  Mass balance altitudinal gradient...\n")
  mb_vs_ele_list <- func_plot_massbal_vs_elevation(year_data,
                                                   run_params,
                                                   data_dems)
  plots_mb_vs_ele   <- mb_vs_ele_list[["plots_mb_vs_ele_out"]]
  if (length(plots_mb_vs_ele) == 1) {
    plots_year        <- append(plots_year, plots_mb_vs_ele)
  } else {
    plots_year        <- append(append(plots_year, plots_mb_vs_ele[1]),
                                plots_mb_vs_ele[2])
  }
  
  # This data frame is used later to save some overview values.
  ele_bands_plot_df <- mb_vs_ele_list[["ele_bands_plot_df"]]
  
  
  
  # Plot modeled series of each stake -------------------------------------------------------------
  # time_v[13] <- Sys.time()
  if (year_data$nstakes_annual > 0) {
    cat("  Mass balance at the stakes...\n")
    plots_stakes <- func_plot_stakes(year_data,
                                     run_params)
    for (stakes_page_id in 1:length(plots_stakes)) {
      plots_year <- append(plots_year, list(plots_stakes[[stakes_page_id]]))
    }
  }
  
  
  # Plot LOO results ------------------------------------------------------------------------------
  # time_v[14] <- Sys.time()
  if (year_data$run_loo_logi) {
    cat("  Leave-one-out results...\n")
    plots_loo_results <- func_plot_loo_results(year_data,
                                               run_params)
    plots_year <- append(plots_year, list(plots_loo_results))
  }
  
  
  
  # Write multi-page PDF for the current year -----------------------------------------------------
  # time_v[15] <- Sys.time()
  cat("  Writing PDF file...\n")
  suppressMessages(suppressWarnings(ggexport(plotlist = plots_year,
                                             filename = file.path(run_params$output_dirname, "annual_results", paste0("massbalance_", year_data$year_cur, ".pdf")),
                                             width = 21 * run_params$size_mult,
                                             height = 29.7 * run_params$size_mult)))
  
  # time_v[16] <- Sys.time()
  
  # cat("Timings:", sprintf("%.2f", diff(time_v)), "\n")
  
  return(ele_bands_plot_df  = ele_bands_plot_df)
}
