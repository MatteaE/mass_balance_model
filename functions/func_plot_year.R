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
  
  
  # Plot the mass balance maps --------------------------------------------------------------------
  # This returns a list with the (5 or 6, depending on whether we have winter measurements)
  # mass balance maps for the current year.
  # Then we will append to this list also the
  # other plots of the year (time series,
  # vertical distributions and so on).
  cat("  Mass balance maps...\n")
  plots_year <- func_plot_year_mb_maps(year_data,
                                       run_params,
                                       data_dems,
                                       data_outlines)
  
  
  # Plot the SWE maps -----------------------------------------------------------------------------
  cat("  SWE maps...\n")
  plots_swe <- func_plot_year_swe_maps(year_data,
                                       run_params,
                                       data_dhms,
                                       data_dems,
                                       data_outlines)
  plots_year <- append(plots_year, plots_swe)
  
  
  # Plot the map of avalanche effect --------------------------------------------------------------
  cat("  Avalanche map...\n")
  plots_avalanche <- func_plot_avalanche_net_effect(year_data,
                                                    run_params,
                                                    data_dhms,
                                                    data_dems,
                                                    data_outlines)
  plots_year <- append(plots_year, plots_avalanche)
  
  
  # Plot the map of snow cover duration (hydrological year) ---------------------------------------
  cat("  Snow cover duration...\n")
  plots_snow_duration <- func_plot_snowcover_duration(year_data,
                                                      year_cur_params,
                                                      run_params,
                                                      data_dhms,
                                                      data_dems,
                                                      data_outlines)
  plots_year <- append(plots_year, plots_snow_duration)
  
  
  # Plot the map of snowfall distribution ---------------------------------------------------------
  cat("  Snowfall distribution map...\n")
  plots_snowdist <- func_plot_year_snowdist_map(year_data,
                                                run_params,
                                                data_dhms,
                                                data_outlines)
  plots_year <- append(plots_year, plots_snowdist)
  
  
  
  # Plot the daily meteorological series ----------------------------------------------------------
  # This also plots the result of prec_corr/100 * prec_summer_fact, that is, the daily correction to the precipitation series.
  cat("  Meteorological series...\n")
  plot_weather_series <- func_plot_weather_series(year_data,
                                                  year_cur_params,
                                                  run_params)
  plots_year <- append(plots_year, list(plot_weather_series))
  
  
  
  # Plot the daily time series of glacier-wide mass balance ---------------------------------------
  cat("  Mass balance time series...\n")
  plots_mb_cumul <- func_plot_massbal_cumul(year_data,
                                            run_params)
  plots_year <- append(plots_year, list(plots_mb_cumul))
  
  
  
  # Plot the daily time series of snow-covered area fraction --------------------------------------
  cat("  Snow-covered area fraction...\n")
  plot_scaf <- func_plot_scaf(year_data,
                              run_params)
  plots_year <- append(plots_year, list(plot_scaf))
  
  
  
  # Plot mass balance versus elevation ------------------------------------------------------------
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
  if (year_data$nstakes_annual > 0) {
    cat("  Mass balance at the stakes...\n")
    plots_stakes <- func_plot_stakes(year_data,
                                     run_params)
    for (stakes_page_id in 1:length(plots_stakes)) {
      plots_year <- append(plots_year, list(plots_stakes[[stakes_page_id]]))
    }
  }
  
  
  # Plot LOO results ------------------------------------------------------------------------------
  if (year_data$run_loo_logi) {
    cat("  Leave-one-out results...\n")
    plots_loo_results <- func_plot_loo_results(year_data,
                                               run_params)
    plots_year <- append(plots_year, list(plots_loo_results))
  }
  
  
  
  # Write multi-page PDF for the current year -----------------------------------------------------
  cat("  Putting it all together...\n")
  plots_year_out <- suppressWarnings(ggarrange(plotlist = plots_year, ncol = 1, nrow = 1, align = "hv"))
  suppressMessages(ggexport(plots_year_out,
                            filename = file.path(run_params$output_dirname, "annual_results", paste0("massbalance_", year_data$year_cur, ".pdf")),
                            width = 21 * run_params$size_mult,
                            height = 29.7 * run_params$size_mult))
  
  
  return(ele_bands_plot_df  = ele_bands_plot_df)
}
