###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the generation of the final overview plots.                  #
###################################################################################################  

func_plot_overview <- function(overview_annual,
                               run_params) {
  
  base_size <- 16 # For the plots
  
  theme_overview_plots <- theme_bw(base_size = base_size) +
                          theme(axis.title.x = element_blank(),
                                plot.title = element_text(hjust = 0.5),
                                text = element_text(face = "bold"))
  
  plots <- list()
  
  # Show 4-5 years on the x axis, using only integer values.
  x_breaks <- seq(overview_annual$summary_df$year[1], overview_annual$summary_df$year[length(overview_annual$summary_df$year)], by = max(1, floor(length(overview_annual$summary_df$year) / 4)))
  
  # Do we have a single year to show?
  single_year <- nrow(overview_annual$summary_df) == 1
  
  # Time series of annual mass balance over the measurement
  # period, corrected within elevation bands.
  # Also horizontal line with mean over the period.
  # If we model just one year, add point plot so that something is visible.
  # We do this only if all years have mass balance measurements, else it is confusing.
  if (all(overview_annual$summary_df$year_has_data) == TRUE) {
    single_year_point <- NULL
    if (single_year) {single_year_point <- geom_point(aes(x = year, y = mb_annual_meas_corr))}
    plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
      geom_line(aes(x = year, y = mb_annual_meas_corr), size = 1) +
      geom_segment(x = overview_annual$summary_df$year[1], xend = overview_annual$summary_df$year[length(overview_annual$summary_df$year)],
                   y = mean(overview_annual$summary_df$mb_annual_meas_corr), yend = mean(overview_annual$summary_df$mb_annual_meas_corr),
                   linetype = "dashed", size = 1) +
      single_year_point +
      scale_x_continuous(breaks = x_breaks) +
      ylab("Mass balance [m w.e.]") +
      ggtitle("Mass balance (measurement period + local correction)") +
      theme_overview_plots
  }

  
  # Time series of other annual mass balances:
  # over the measurement period with no correction,
  # over the hydrological year,
  # over a fixed (user-defined) period.
  # If we model just one year, add point plot so that something is visible.
  single_year_point1 <- NULL
  single_year_point2 <- NULL
  single_year_point3 <- NULL
  if (single_year) {
    single_year_point1 <- geom_point(aes(x = year, y = mb_annual_meas), color = "#FF00FF")
    single_year_point2 <- geom_point(aes(x = year, y = mb_annual_hydro), color = "#0000FF")
    single_year_point3 <- geom_point(aes(x = year, y = mb_annual_fixed), color = "#00FFFF")
  }
  plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
    geom_line(aes(x = year, y = mb_annual_meas), color = "#FF00FF", size = 1) +
    geom_line(aes(x = year, y = mb_annual_hydro), color = "#0000FF", size = 1) +
    geom_line(aes(x = year, y = mb_annual_fixed), color = "#00FFFF", size = 1) +
    single_year_point1 +
    single_year_point2 +
    single_year_point3 +
    ylab("Mass balance [m w.e.]") +
    scale_y_continuous(expand = expansion(0.3, 0)) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle("Annual mass balance (no local correction)") +
    annotation_custom(grobTree(textGrob("Measurement period", x=0.05, y = 0.19, hjust = 0,
                                        gp=gpar(col="#FF00FF", fontsize = base_size * 1., fontface="bold")))) +
    annotation_custom(grobTree(textGrob("Hydrological year", x=0.05, y = 0.12, hjust = 0,
                                        gp=gpar(col="#0000FF", fontsize = base_size * 1., fontface="bold")))) +
    annotation_custom(grobTree(textGrob("Fixed period", x=0.05, y = 0.05, hjust = 0,
                                        gp=gpar(col="#00FFFF", fontsize = base_size * 1., fontface="bold")))) +
    theme_overview_plots
  
  
  # Time series of winter mass balances:
  # over a fixed (user-defined) winter period,
  # over the measurement period (only if winter measurements are available).
  # If we model just one year, add point plot so that something is visible.
  single_year_point1 <- NULL
  single_year_point2 <- NULL
  if (single_year) {
    single_year_point1 <- geom_point(aes(x = year, y = mb_winter_fixed), color = "#00FFFF")
    if(any(!is.na(overview_annual$summary_df$mb_winter_meas))) {
      single_year_point2 <- geom_point(aes(x = year, y = mb_winter_meas), color = "#FF00FF")
    }
  }
  plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
    {if(any(!is.na(overview_annual$summary_df$mb_winter_meas))) geom_line(aes(x = year, y = mb_winter_meas), color = "#FF00FF", size = 1)} +
    geom_line(aes(x = year, y = mb_winter_fixed), color = "#00FFFF", size = 1) +
    single_year_point1 +
    single_year_point2 +
    ylab("Mass balance [m w.e.]") +
    scale_y_continuous(expand = expansion(0.3, 0)) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle("Winter mass balance") +
    {if(any(!is.na(overview_annual$summary_df$mb_winter_meas))) annotation_custom(grobTree(textGrob("Measurement period", x=0.05, y = 0.12, hjust = 0,
                                        gp=gpar(col="#FF00FF", fontsize = base_size * 1., fontface="bold"))))} +
    annotation_custom(grobTree(textGrob("Fixed period", x=0.05, y = 0.05, hjust = 0,
                                        gp=gpar(col="#00FFFF", fontsize = base_size * 1., fontface="bold")))) +
    theme_overview_plots
  
  
  # Time series of ELA.
  # If we model just one year, add point plot so that something is visible.
  single_year_point <- NULL
  if (single_year) {single_year_point <- geom_point(aes(x = year, y = ela))}
  plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
    geom_line(aes(x = year, y = ela), size = 1) +
    single_year_point +
    ylab("Equilibrium Line Altitude [m a.s.l.]") +
    scale_y_continuous(expand = expansion(0.5, 0)) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle("Equilibrium Line Altitude") +
    theme_overview_plots
  
  
  # Time series of AAR.
  # If we model just one year, add point plot so that something is visible.
  single_year_point <- NULL
  if (single_year) {single_year_point <- geom_point(aes(x = year, y = aar))}
  plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
    geom_line(aes(x = year, y = aar), size = 1) +
    single_year_point +
    ylab("Accumulation-Area Ratio [%]") +
    scale_y_continuous(expand = expansion(0.5, 0), limits = c(NA, 100)) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle("Accumulation-Area Ratio") +
    theme_overview_plots
  
  
  # Time series of RMSE.
  # If we model just one year, add point plot so that something is visible.
  single_year_point <- NULL
  if (single_year) {single_year_point <- geom_point(aes(x = year, y = rmse))}
  plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
    geom_line(aes(x = year, y = rmse), size = 1) +
    single_year_point +
    ylab("RMSE [m w.e.]") +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.2))) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle("Root-Mean-Square Error") +
    theme_overview_plots
  
  
  # Time series of the melt parameters.
  # If we model just one year, add point plot so that something is visible.
  single_year_point1 <- NULL
  single_year_point2 <- NULL
  single_year_point3 <- NULL
  if (single_year) {
    single_year_point1 <- geom_point(aes(x = year, y = rad_fact_snow), color = "#0000FF")
    single_year_point2 <- geom_point(aes(x = year, y = melt_factor), color = "#FF00FF")
    single_year_point3 <- geom_point(aes(x = year, y = rad_fact_ice), color = "#00FFFF")
  }
  plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
    geom_line(aes(x = year, y = rad_fact_snow), color = "#0000FF", size = 1) +
    geom_line(aes(x = year, y = melt_factor), color = "#FF00FF", size = 1.25) + # Different size since the melt factor is sometimes the same as the rad_fact_ice.
    geom_line(aes(x = year, y = rad_fact_ice), color = "#00FFFF", size = 0.5) +
    single_year_point1 +
    single_year_point2 +
    single_year_point3 +
    ylab("Parameter value [different units]") +
    scale_y_continuous(expand = expansion(0.5, 0)) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle("Melt parameters") +
    annotation_custom(grobTree(textGrob("Degree-day melt factor", x=0.05, y = 0.19, hjust = 0,
                                        gp=gpar(col="#FF00FF", fontsize = base_size * 1., fontface="bold")))) +
    annotation_custom(grobTree(textGrob("Radiation factor (snow)", x=0.05, y = 0.12, hjust = 0,
                                        gp=gpar(col="#0000FF", fontsize = base_size * 1., fontface="bold")))) +
    annotation_custom(grobTree(textGrob("Radiation factor (ice)", x=0.05, y = 0.05, hjust = 0,
                                        gp=gpar(col="#00FFFF", fontsize = base_size * 1., fontface="bold")))) +
    theme_overview_plots
  
  
  # Time series of the precipitation correction.
  # We use a slightly more complex formula for the
  # y-axis limits so that when we have a single value
  # the limits still make sense.
  # If we model just one year, add point plot so that something is visible.
  single_year_point <- NULL
  if (single_year) {single_year_point <- geom_point(aes(x = year, y = prec_corr), color = "#0000FF")}
  plots[[length(plots)+1]] <- ggplot(overview_annual$summary_df) +
    geom_line(aes(x = year, y = prec_corr), color = "#0000FF", size = 1) +
    single_year_point +
    ylab("Precipitation correction [%]") +
    scale_y_continuous(limits = c(min(min(overview_annual$summary_df$prec_corr), mean(overview_annual$summary_df$prec_corr) - 0.1 * mean(overview_annual$summary_df$prec_corr)), max(max(overview_annual$summary_df$prec_corr), mean(overview_annual$summary_df$prec_corr + 0.1 * overview_annual$summary_df$prec_corr)))) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle("Precipitation correction factor") +
    theme_overview_plots
  
  

  # Time series of cumulative hydrological year mass balance.
  x_breaks_cumul <- seq(overview_annual$summary_df$year[1]-1, overview_annual$summary_df$year[length(overview_annual$summary_df$year)], by = max(1, floor((length(overview_annual$summary_df$year)+1) / 4)))
  df_lines <- data.frame(year_start = overview_annual$summary_df$year - 1,
                         year_end   = overview_annual$summary_df$year,
                         mb_start   = c(0, overview_annual$summary_df$mb_cumul[1:(nrow(overview_annual$summary_df)-1)]),
                         mb_end     = overview_annual$summary_df$mb_cumul,
                         has_data   = as.character(overview_annual$summary_df$year_has_data))
  if (!all(overview_annual$summary_df$year_has_data)) {
    theme_mbcumul_legend <- theme(legend.position = c(0.14,0.102),
                                  legend.title = element_blank(),
                                  legend.margin = margin(0,0,0,0),
                                  legend.spacing = unit(0, "pt"),
                                  legend.box.margin = margin(0,0,0,0),
                                  legend.box.spacing = unit(0,"pt"),
                                  legend.background = element_blank(),
                                  legend.box.background = element_blank())
  } else {
    theme_mbcumul_legend <- theme(legend.position = "none")
  }
plots[[length(plots)+1]] <- ggplot(data.frame(year = overview_annual$summary_df$year,
                                              mb_cumul = overview_annual$summary_df$mb_cumul,
                                                has_data = as.character(overview_annual$summary_df$year_has_data))) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
    geom_segment(data = df_lines, aes(x = year_start, xend = year_end,
                                      y = mb_start, yend = mb_end,
                                      linetype = has_data),
                 color = "#FF0000", size = 1) +
    geom_point(aes(x = year, y = mb_cumul, shape = has_data), color = "#FF0000", size = 3, stroke = 1.2) +
    scale_y_continuous(breaks = pretty(c(0, overview_annual$summary_df$mb_cumul))) +
    scale_x_continuous(breaks = x_breaks_cumul) +
    scale_shape_manual(values = c("TRUE" = 17, "FALSE" = 2),
                       labels = c("TRUE" = "Year measured", "FALSE" = "Year not measured")) +
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "11"),
                          labels = c("TRUE" = "Year measured", "FALSE" = "Year not measured")) +
    ylab("Cumulative mass balance [m w.e.]") +
    ggtitle("Cumulative mass balance (hydrological years)") +
    theme_overview_plots +
    theme_mbcumul_legend
  
  
  # Time series of cumulative hydrological year mass balance,
  # with also daily mass balance and modeling period boundaries.
  # Vertical blue lines: hydrological year boundaries.
  # Vertical dashed purple lines: measurement period start.
  # Vertical dotted purple lines: measurement period end.
  first_year_hydro_start_id <- pmatch(as.Date(paste0(run_params$first_year-1, "/10/1")), table = overview_annual$daily_data_list$mb_series_all_dates[[1]])
  mb_first_year_hydro_start <- overview_annual$daily_data_list$mb_series_all_raw[[1]][first_year_hydro_start_id]
  mb_series_all <- overview_annual$daily_data_list$mb_series_all_raw
  mb_series_all[[1]] <- mb_series_all[[1]] - mb_first_year_hydro_start
  if (run_params$n_years > 1) {
    for (year_id in 2:run_params$n_years) {
      # Note: the two ids below are used to align the two
      # hydrological year values (YYYY-1 end with YYYY start).
      year_prev_hydro_end_id <- pmatch(as.Date(paste0(run_params$years[year_id]-1, "/10/1")), table = overview_annual$daily_data_list$mb_series_all_dates[[year_id-1]])
      year_cur_hydro_start_id <- pmatch(as.Date(paste0(run_params$years[year_id]-1, "/10/1")), table = overview_annual$daily_data_list$mb_series_all_dates[[year_id]])
      year_prev_mb <- mb_series_all[[year_id-1]][year_prev_hydro_end_id]
      year_cur_mb <- mb_series_all[[year_id]][year_cur_hydro_start_id]
      mb_series_all[[year_id]] <- mb_series_all[[year_id]] + year_prev_mb - year_cur_mb
    }
  }  
  mb_all_lengths <- sapply(mb_series_all, FUN = length) # Length of each annual simulation [days].
  mb_all_df <- data.frame(day = as.Date(unlist(overview_annual$daily_data_list$mb_series_all_dates), origin = as.Date("1970/1/1")),
                          mb = unlist(mb_series_all)/1e3,
                          year_id = as.factor(rep(1:length(mb_series_all), mb_all_lengths)))
  
  mb_cumul_df <- data.frame(year = as.Date(paste0(c(overview_annual$summary_df$year[1]-1, overview_annual$summary_df$year), "/10/1")),
                            mb_cumul = c(0, overview_annual$summary_df$mb_cumul))
  plots[[length(plots)+1]] <- ggplot() +
    geom_vline(xintercept = as.Date(paste0(c(run_params$years[1]-1,run_params$years), "/10/1")), color = "#0000FF") +
    geom_vline(xintercept = sapply(overview_annual$daily_data_list$mb_series_all_measperiod_dates, `[`, 1), color = "#FF00FF", linetype = "dashed") +
    geom_vline(xintercept = sapply(overview_annual$daily_data_list$mb_series_all_measperiod_dates, `[`, 2), color = "#FF00FF", linetype = "dotted") +
    geom_line(data = mb_all_df, aes(x = day, y = mb, group = year_id)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
    geom_line(data = mb_cumul_df,  aes(x = year, y = mb_cumul), color = "#FF0000", size = 1) +
    geom_point(data = mb_cumul_df, aes(x = year, y = mb_cumul), color = "#FF0000", shape = 2, size = 3, stroke = 1.2) +
    scale_y_continuous(breaks = pretty(c(max(mb_all_df$mb), overview_annual$summary_df$mb_cumul))) +
    scale_x_date(date_labels = "%Y/%m") +
    ylab("Cumulative mass balance [m w.e.]") +
    ggtitle("Cumulative mass balance (hydrological years)") +
    theme_overview_plots
  
  
  overview_plots <- suppressWarnings(ggarrange(plotlist = plots, ncol = 1, nrow = 3, align = "hv"))
  
  return(overview_plots)
  
}
