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
  
  # Prepare data frame for text output.
  overview_annual$summary_df_out <- data.frame(year = overview_annual$summary_df$year,
                                               sprintf(overview_annual$summary_df$pdd_sum_hydro_aws, fmt="%.0f"),
                                               sprintf(overview_annual$summary_df$mb_annual_meas_corr, fmt=run_params$output_fmt1),
                                               sprintf(overview_annual$summary_df$mb_annual_meas, fmt=run_params$output_fmt1),
                                               sprintf(overview_annual$summary_df$mb_annual_hydro, fmt=run_params$output_fmt1),
                                               sprintf(overview_annual$summary_df$mb_winter_meas, fmt=run_params$output_fmt1),
                                               sprintf(overview_annual$summary_df$mb_winter_fixed, fmt=run_params$output_fmt1),
                                               overview_annual$summary_df$ela,
                                               sprintf("%.1f", overview_annual$summary_df$aar),
                                               overview_annual$summary_df$snowcover_days_min,
                                               sprintf("%.1f", overview_annual$summary_df$snowcover_days_mean),
                                               sprintf(run_params$output_fmt1, overview_annual$summary_df$rmse),
                                               sprintf(run_params$output_fmt1, overview_annual$summary_df$loo_rmse),
                                               sprintf(overview_annual$summary_df$melt_factor, fmt="%.3f"),
                                               sprintf(overview_annual$summary_df$rad_fact_ice, fmt="%.3f"),
                                               sprintf(overview_annual$summary_df$rad_fact_snow, fmt="%.3f"),
                                               overview_annual$summary_df$prec_corr,
                                               sprintf(overview_annual$summary_df$mb_cumul, fmt=run_params$output_fmt1))
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
