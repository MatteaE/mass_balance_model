###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to plot and write the model output overview.        #
###################################################################################################


func_plot_write_overview <- function(overview_annual,
                                     run_params) {
  
  cat("\n** Drawing overview plots... **\n")
  
  overview_annual$summary_df$mb_cumul <- cumsum(overview_annual$summary_df$mb_annual_hydro)
  
  # Generate overview plots.
  overview_plots <- func_plot_overview(overview_annual,
                                       run_params)
  
  suppressMessages(ggexport(overview_plots,
                            filename = file.path(run_params$output_dirname, "overview.pdf"),
                            width = 21 * run_params$size_mult,
                            height = 29.7 * run_params$size_mult))
  
  # Different treatment if we have a single modeled year, else the data.frame is built wrong.
  if (nrow(overview_annual$summary_df) > 1) {
    overview_annual$summary_df_out <- data.frame(year = overview_annual$summary_df$year,
                                                 apply(overview_annual$summary_df[,2:7], 2, sprintf, fmt="%.3f"),
                                                 overview_annual$summary_df[,8],
                                                 sprintf("%.1f", overview_annual$summary_df[,9]),
                                                 apply(overview_annual$summary_df[,10:13], 2, sprintf, fmt="%.3f"),
                                                 overview_annual$summary_df[,14],
                                                 sprintf("%.2f", overview_annual$summary_df[,15]))
  } else {
    overview_annual$summary_df_out <- data.frame(year = overview_annual$summary_df$year,
                                                 sprintf(overview_annual$summary_df[1,2], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,3], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,4], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,5], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,6], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,7], fmt="%.3f"),
                                                 overview_annual$summary_df[,8],
                                                 sprintf(overview_annual$summary_df[1,9], fmt="%.1f"),
                                                 sprintf(overview_annual$summary_df[1,10], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,11], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,12], fmt="%.3f"),
                                                 sprintf(overview_annual$summary_df[1,13], fmt="%.3f"),
                                                 overview_annual$summary_df[,14],
                                                 sprintf(overview_annual$summary_df[1,15], fmt="%.2f"))
  }
  names(overview_annual$summary_df_out) <- names(overview_annual$summary_df)[1:(ncol(overview_annual$summary_df)-2)]
  write.csv(overview_annual$summary_df_out,
            file.path(run_params$output_dirname, "overview.csv"),
            quote = FALSE,
            row.names = FALSE)
  
  
  # Save to a separate file the annual maps of hydrological mass balance.
  # We extract them from the annual PDFs and merge them.
  overview_areaplot_pdf_path <- file.path(run_params$output_dirname, "overview_areaplot.pdf")
  for (year_id in 1:run_params$n_years) {
    annual_pdf_path <- file.path(run_params$output_dirname, "annual_results", paste0("massbalance_", run_params$years[year_id], ".pdf"))
    # Extract first page of the annual PDF (i.e. hydro mass balance map).
    invisible(pdf_subset(annual_pdf_path, pages = 1, output = paste0("hydro_mb_", year_id, ".pdf")))
  }
  invisible(pdf_combine(paste0("hydro_mb_", 1:run_params$n_years, ".pdf"), output = overview_areaplot_pdf_path))
  invisible(file.remove(paste0("hydro_mb_", 1:run_params$n_years, ".pdf")))
}
