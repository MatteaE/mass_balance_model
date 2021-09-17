###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to produce all plots from a year.                   #
###################################################################################################



func_plot_year <- function(year_data,
                           run_params,
                           data_dems,
                           data_outlines,
                           areaplots_list) {
  
  cat("\n** Producing year plots... **\n")
  
  #### . PLOT THE MASS BALANCE MAPS ####
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
  
  
  #### . PLOT THE DAILY TIME SERIES OF GLACIER-WIDE MASS BALANCE ####
  cat("  Mass balance time series...\n")
  plots_mb_cumul <- func_plot_massbal_cumul(year_data,
                                            run_params)
  plots_year <- append(plots_year, list(plots_mb_cumul))
  
  
  #### . PLOT MASS BALANCE VERSUS ELEVATION ####
  cat("  Mass balance altitudinal gradient...\n")
  mb_vs_ele_list <- func_plot_massbal_vs_elevation(year_data,
                                                   run_params,
                                                   data_dems)
  plots_mb_vs_ele   <- mb_vs_ele_list[["plots_mb_vs_ele_out"]]
  plots_year        <- append(plots_year, list(plots_mb_vs_ele))
  
  # This data frame is used later to save some overview values.
  ele_bands_plot_df <- mb_vs_ele_list[["ele_bands_plot_df"]]
  
  
  
  #### . PLOT MODELED SERIES OF EACH STAKE ####
  if (year_data$nstakes_annual > 0) {
    cat("  Mass balance at the stakes...\n")
    plots_stakes <- func_plot_stakes(year_data)
    for (stakes_page_id in 1:length(plots_stakes)) {
      plots_year <- append(plots_year, list(plots_stakes[[stakes_page_id]]))
    }
  }
  
  # Write multi-page PDF for the current year.
  cat("  Putting it all together...\n")
  plots_year_out <- suppressWarnings(ggarrange(plotlist = plots_year, ncol = 1, nrow = 1, align = "hv"))
  suppressMessages(ggexport(plots_year_out,
                            filename = file.path(run_params$output_dirname, "annual_results", paste0("massbalance_", year_data$year_cur, ".pdf")),
                            width = 21 * run_params$size_mult,
                            height = 29.7 * run_params$size_mult))
  
  # Save the plot of the hydrological mass balance of the year (without single stake values).
  # We will put it in a PDF file with 1 plot per year (overview_areaplot.pdf).
  areaplots_list[[year_id]] <- plots_year[[1]]
  
  return(list(areaplots_list     = areaplots_list,
              ele_bands_plot_df  = ele_bands_plot_df))
}
