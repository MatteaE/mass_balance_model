###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to produce all plots from a year.                   #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

cat("** Producing year plots... **\n")

#### . PLOT THE MASS BALANCE MAPS ####
# This returns a list with the (5 or 6, depending on whether we have winter measurements)
# mass balance maps for the current year.
# Then we will append to this list also the
# other plots of the year (time series,
# vertical distributions and so on).
plots_year <- func_plot_year_mb_maps(run_params,
                                     year_cur,
                                     data_dems,
                                     data_outlines,
                                     dem_grid_id,
                                     outline_id,
                                     massbal_annual_maps,
                                     massbal_winter_maps,
                                     massbal_annual_values,
                                     massbal_winter_values,
                                     massbal_annual_meas_period,
                                     massbal_winter_meas_period,
                                     massbal_annual_meas_cur,
                                     process_winter)


#### . PLOT THE DAILY TIME SERIES OF GLACIER-WIDE MASS BALANCE ####
plots_mb_cumul <- func_plot_massbal_cumul(run_params,
                                          process_winter,
                                          massbal_annual_meas_cur,
                                          massbal_winter_meas_cur,
                                          mod_output_annual_cur,
                                          model_annual_bounds)
plots_year <- append(plots_year, list(plots_mb_cumul))


#### . PLOT MASS BALANCE VERSUS ELEVATION ####
plots_mb_vs_ele <- func_plot_massbal_vs_elevation(run_params,
                                                  data_dems,
                                                  massbal_annual_maps,
                                                  massbal_winter_maps,
                                                  dem_grid_id,
                                                  massbal_annual_meas_cur)
plots_year <- append(plots_year, list(plots_mb_vs_ele))



#### . PLOT MODELED SERIES OF EACH STAKE ####
plots_stakes <- func_plot_stakes(model_annual_days_n,
                                 model_annual_bounds,
                                 nstakes_annual,
                                 mod_output_annual_cur,
                                 massbal_annual_meas_cur)
for (stakes_page_id in 1:length(plots_stakes)) {
  plots_year <- append(plots_year, list(plots_stakes[[stakes_page_id]]))
}

# Write multi-page PDF for the current year.
plots_year_out <- ggarrange(plotlist = plots_year, ncol = 1, nrow = 1, align = "hv")
suppressMessages(ggexport(plots_year_out,
                          filename = file.path(run_params$output_dirname, "annual_results", paste0("massbalance_", year_cur, ".pdf")),
                          width = 21 * run_params$size_mult,
                          height = 29.7 * run_params$size_mult))

# Save the plot of the final mass balance of the year (without single stake values).
# We will put it in a PDF file with 1 plot per year (overview_areaplot.pdf).
overview_areaplots[[year_id]] <- plots_year[[4]]

