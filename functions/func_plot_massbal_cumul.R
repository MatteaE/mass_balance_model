###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the generation of the two plots of mass balance evolution    #
#                 over a year: overall mass balance alone, and also with accumulation and         #
#                 ablation.                                                                       #
###################################################################################################  

func_plot_massbal_cumul <- function(year_data,
                                    run_params) {
  
  # Here will go the two plots of annual mass balance
  # ((1) mass balance only, and (2) also with accumulation
  # and ablation components).
  plots_mb <- list()
  
  
  # Prepare the data for plotting.
  massbal_cumul_df <- data.frame(date  = seq.Date(year_data$model_time_bounds[1]-1, year_data$model_time_bounds[2], by = "1 day"),
                                 mb    = year_data$mod_output_annual_cur$gl_massbal_cumul * run_params$output_mult,
                                 melt  = year_data$mod_output_annual_cur$gl_melt_cumul * run_params$output_mult,
                                 accum = year_data$mod_output_annual_cur$gl_accum_cumul * run_params$output_mult)
  day_id_offset <- (length(massbal_cumul_df$date) - as.integer(format(massbal_cumul_df$date[length(massbal_cumul_df$date)], "%j"))) + 1
  massbal_cumul_df$day_id <- seq_along(massbal_cumul_df$date) - day_id_offset # So that day_id = 1 is Jan 1.
  
  
  # Setup month labels.
  months_labels_all <- format(massbal_cumul_df$date, "%b")
  months_doy <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  
  # Select the day at the middle of each month.
  # If the simulation starts after day 15 of the first month,
  # the first item of months_labels_ids refers to the second
  # month of the simulation.
  months_labels_ids <- which(as.integer(format(massbal_cumul_df$date, "%j")) %in% months_doy)
  months_labels_df <- data.frame(day_id = massbal_cumul_df$day_id[months_labels_ids],
                                 label  = months_labels_all[months_labels_ids])
  # Don't add label for first month unless it is
  # represented by at least 28 days, and same for last month.
  # To do this, we remove the first label if the first month
  # of the data frame has fewer than 28 days and the first day
  # of the simulation is before the 15th of the month
  # (else the month is already not present, since we use the
  # middle of the month).
  months_cur_rle <- rle(as.integer(format(massbal_cumul_df$date, "%m")))
  if ((months_cur_rle$lengths[1] < 28) && (as.integer(format(massbal_cumul_df$date[1], "%d")) < 15)) { 
    months_labels_df <- months_labels_df[-1,]
  }
  if ((months_cur_rle$lengths[length(months_cur_rle$lengths)] < 28) && (as.integer(format(massbal_cumul_df$date[nrow(massbal_cumul_df)], "%d")) > 15)) { # Same, for last month.
    months_labels_df <- months_labels_df[-nrow(months_labels_df),]
  }
  
  day_id_hydro1 <- massbal_cumul_df$day_id[which(format(massbal_cumul_df$date, "%Y-%m-%d") == paste0(format(massbal_cumul_df$date[1], "%Y"), "-10-01"))] # day_id of the hydrological year start.
  day_id_hydro2 <- massbal_cumul_df$day_id[which(format(massbal_cumul_df$date, "%Y-%m-%d") == paste0(as.integer(format(massbal_cumul_df$date[1], "%Y")) + 1, "-09-30"))] # day_id of the hydrological year start.
  
  if (year_data$nstakes_annual > 0) {
    day_id_meas1 <- massbal_cumul_df$day_id[which(format(massbal_cumul_df$date, "%Y-%m-%d") == min(year_data$massbal_annual_meas_cur$start_date, na.rm = T))] # day_id of the first annual stake start.
    day_id_meas2 <- massbal_cumul_df$day_id[which(format(massbal_cumul_df$date, "%Y-%m-%d") == max(year_data$massbal_annual_meas_cur$end_date, na.rm = T))] # day_id of the last annual stake end.
  }
  
  if (year_data$process_winter) {
    day_id_meas1_winter <- massbal_cumul_df$day_id[which(format(massbal_cumul_df$date, "%Y-%m-%d") == min(year_data$massbal_winter_meas_cur$start_date, na.rm = T))] # day_id of the first winter stake start.
    day_id_meas2_winter <- massbal_cumul_df$day_id[which(format(massbal_cumul_df$date, "%Y-%m-%d") == max(year_data$massbal_winter_meas_cur$end_date, na.rm = T))] # day_id of the last winter stake end.
  }
  
  base_size <- 16 # For the plots
  theme_mbcumul_plots <- theme_bw(base_size = base_size) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank())
  
  # Generate plot of mass balance alone.
  plots_mb[[1]] <- ggplot(massbal_cumul_df) +
    annotate("text", x = months_labels_df$day_id, y = -Inf, label = months_labels_df$label, vjust = -1, fontface = "bold", size = 5) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "longdash", size = 0.5) +
    geom_vline(xintercept = c(day_id_hydro1, day_id_hydro2), linetype = "solid", size = 0.5, color = "#0000FF") +
    {if (year_data$nstakes_annual > 0) geom_vline(xintercept = c(day_id_meas1, day_id_meas2), linetype = "solid", size = 0.5, color = "#FF00FF")} +
    {if (year_data$process_winter) geom_vline(xintercept = c(day_id_meas1_winter, day_id_meas2_winter), linetype = "solid", size = 0.5, color = "#FF00FF")} +
    geom_line(aes(x = day_id, y = mb / 1e3), size = 0.7) +
    # geom_vline(xintercept = c(massbal_cumul_df$day_id[months_labels_ids] - 14, massbal_cumul_df$day_id[months_labels_ids[length(months_labels_ids)]] + 16)) +
    scale_x_continuous(expand = expansion(mult = 0.02)) +
    scale_y_continuous(breaks = pretty(massbal_cumul_df$mb/1e3)) +
    ylab(paste0("Mass balance [", run_params$output_unit, " w.e.]")) +
    theme_mbcumul_plots
  
  
  # Generate plot of mass balance with accumulation and ablation.
  plots_mb[[2]] <- ggplot(massbal_cumul_df) +
    annotate("text", x = months_labels_df$day_id, y = -Inf, label = months_labels_df$label, vjust = -1, fontface = "bold", size = 5) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "longdash", size = 0.5) +
    geom_vline(xintercept = c(day_id_hydro1, day_id_hydro2), linetype = "solid", size = 0.5, color = "#0000FF") +
    {if (year_data$nstakes_annual > 0) geom_vline(xintercept = c(day_id_meas1, day_id_meas2), linetype = "solid", size = 0.5, color = "#FF00FF")} +
    {if (year_data$process_winter) geom_vline(xintercept = c(day_id_meas1_winter, day_id_meas2_winter), linetype = "solid", size = 0.5, color = "#FF00FF")} +
    geom_line(aes(x = day_id, y = mb / 1e3), size = 0.7) +
    geom_line(aes(x = day_id, y = -melt / 1e3), color = "#FF0000", size = 0.7) +
    geom_line(aes(x = day_id, y = accum / 1e3), color = "#0000FF", size = 0.7) +
    # geom_vline(xintercept = c(massbal_cumul_df$day_id[months_labels_ids] - 14, massbal_cumul_df$day_id[months_labels_ids[length(months_labels_ids)]] + 16)) +
    scale_x_continuous(expand = expansion(mult = 0.02)) +
    scale_y_continuous(breaks = pretty(c(massbal_cumul_df$mb, -massbal_cumul_df$melt, massbal_cumul_df$accum)/1e3)) +
    ylab(paste0("Mass balance [", run_params$output_unit, " w.e.]")) +
    theme_mbcumul_plots
  
  
  # Generate plot of daily melt, could be compared to a hydrograph.
  massbal_daily_df <- data.frame(date  = seq.Date(year_data$model_time_bounds[1], year_data$model_time_bounds[2], by = "1 day"),
                                 melt  = year_data$mod_output_annual_cur$gl_melt_daily[1:(length(year_data$mod_output_annual_cur$gl_melt_daily)-1)] * year_data$glacier_area / 1e3,
                                 rain  = year_data$mod_output_annual_cur$gl_rainfall_daily[1:(length(year_data$mod_output_annual_cur$gl_rainfall_daily)-1)] * year_data$glacier_area / 1e3,
                                 day_id = massbal_cumul_df$day_id[2:nrow(massbal_cumul_df)])
  
  fluxes_divs <- 10^(c(0,3:12))
  fluxes_div_id <- which.min(abs((max(massbal_daily_df$melt)/fluxes_divs)/100 - 1))
  fluxes_div <- fluxes_divs[fluxes_div_id]
  massbal_daily_df$melt <- massbal_daily_df$melt / fluxes_div
  massbal_daily_df$rain <- massbal_daily_df$rain / fluxes_div
  fluxes_div_lab = ""
  if (fluxes_div == 1) {
    plot_ylab <- "<b>Water fluxes [m<sup>3</sup> day<sup>-1</sup>]</b>"
  } else {
    plot_ylab <- paste0("<b>Water fluxes [10<sup>", round(log10(fluxes_div)), "</sup> m<sup>3</sup> day<sup>-1</sup>", "]</b>")
  }
  
  plots_mb[[3]] <- ggplot(massbal_daily_df) +
    annotate("text", x = months_labels_df$day_id, y = Inf, label = months_labels_df$label, vjust = 2, fontface = "bold", size = 5) +
    geom_vline(xintercept = 0, linetype = "longdash", size = 0.5) +
    geom_vline(xintercept = c(day_id_hydro1, day_id_hydro2), linetype = "solid", size = 0.5, color = "#0000FF") +
    {if (year_data$nstakes_annual > 0) geom_vline(xintercept = c(day_id_meas1, day_id_meas2), linetype = "solid", size = 0.5, color = "#FF00FF")} +
    {if (year_data$process_winter) geom_vline(xintercept = c(day_id_meas1_winter, day_id_meas2_winter), linetype = "solid", size = 0.5, color = "#FF00FF")} +
    # geom_line(aes(x = day_id, y = rain), color = "#0000FF", size = 0.7) +
    # geom_line(aes(x = day_id, y = melt), color = "#FF0000", size = 0.7) +
    geom_line(aes(x = day_id, y = rain, color = "rain"), size = 0.7) +
    geom_line(aes(x = day_id, y = melt, color = "melt"), size = 0.7) +
    scale_color_manual(breaks = c("melt", "rain"),
                       values = c("rain" = "#00FFFF", "melt" = "#FF0000"),
                       labels = c("rain" = "Rainfall", "melt" = "Melt")) +
    scale_x_continuous(expand = expansion(mult = 0.02)) +
    scale_y_continuous(breaks = pretty(massbal_daily_df$melt, n = 5), expand = expansion(mult = c(0,0.1))) +
    ylab(plot_ylab) +
    theme_mbcumul_plots +
    theme(legend.position = c(0.45,0.8),
          legend.justification = 0.5,
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.title = element_blank(),
          axis.title.y = element_markdown())
  
  # Align panels.
  plots_mb_out <- plot_grid(plotlist = plots_mb, align = "hv", ncol = 1, nrow = 3)
  
  return(plots_mb_out)
}
