###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to plot and write the model output overview.        #
###################################################################################################


func_plot_write_overview <- function(df_overview,
                                     overview_daily_data,
                                     run_params) {
  
  cat("\n** Drawing overview plots... **\n")
  
  df_overview$mb_cumul <- cumsum(df_overview$mb_annual_hydro)
  
  # Generate overview plots.
  overview_plots <- func_plot_overview(df_overview,
                                       run_params,
                                       overview_daily_data)
  
  suppressMessages(ggexport(overview_plots,
                            filename = file.path(run_params$output_dirname, "overview.pdf"),
                            width = 21 * run_params$size_mult,
                            height = 29.7 * run_params$size_mult))
  
  # Different treatment if we have a single modeled year, else the data.frame is built wrong.
  if (nrow(df_overview) > 1) {
    df_overview_out <- data.frame(year = df_overview$year,
                                  apply(df_overview[,2:7], 2, sprintf, fmt="%.3f"),
                                  df_overview[,8],
                                  sprintf("%.1f", df_overview[,9]),
                                  apply(df_overview[,10:13], 2, sprintf, fmt="%.3f"),
                                  df_overview[,14],
                                  sprintf("%.2f", df_overview[,15]))
  } else {
    df_overview_out <- data.frame(year = df_overview$year,
                                  sprintf(df_overview[1,2], fmt="%.3f"),
                                  sprintf(df_overview[1,3], fmt="%.3f"),
                                  sprintf(df_overview[1,4], fmt="%.3f"),
                                  sprintf(df_overview[1,5], fmt="%.3f"),
                                  sprintf(df_overview[1,6], fmt="%.3f"),
                                  sprintf(df_overview[1,7], fmt="%.3f"),
                                  df_overview[,8],
                                  sprintf(df_overview[1,9], fmt="%.1f"),
                                  sprintf(df_overview[1,10], fmt="%.3f"),
                                  sprintf(df_overview[1,11], fmt="%.3f"),
                                  sprintf(df_overview[1,12], fmt="%.3f"),
                                  sprintf(df_overview[1,13], fmt="%.3f"),
                                  df_overview[,14],
                                  sprintf(df_overview[1,15], fmt="%.2f"))
  }
  names(df_overview_out) <- names(df_overview)
  write.csv(df_overview_out,
            file.path(run_params$output_dirname, "overview.csv"),
            quote = FALSE,
            row.names = FALSE)
  
  # Save to a separate file the annual maps of final annual mass balance
  # (same as already saved within each annual PDF).
  overview_areaplot <- suppressWarnings(ggarrange(plotlist = overview_areaplots, ncol = 1, nrow = 1, align = "hv"))
  suppressMessages(ggexport(overview_areaplot,
                            filename = file.path(run_params$output_dirname, "overview_areaplot.pdf"),
                            width = 21 * run_params$size_mult,
                            height = 29.7 * run_params$size_mult)) 
}
