###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the plotting routine for the series of cumulative mass       #
#                 balance of each stake.                                                          #
################################################################################################### 

func_plot_stakes <- function(year_data) {
  
  plots_stakes <- list()
  
  day_id_offset <- (year_data$model_annual_days_n + 1 - as.integer(format(year_data$model_time_bounds[2], "%j"))) + 1
  day_ids <- 1:(year_data$model_annual_days_n+1) - day_id_offset # So that day_id = 1 is Jan 1.
  
  month_starts <- seq.Date(from = as.Date(paste0(format(year_data$model_time_bounds[1], "%Y/%m"), "/01")),
                           to   = as.Date(paste0(format(year_data$model_time_bounds[2], "%Y/%m"), "/01")),
                           by   = "1 month")
  month_start_ids <- as.integer(month_starts[2:length(month_starts)] - year_data$model_time_bounds[1]) + 1 - day_id_offset
  
  # Compute vertical offset of each series.
  # We need this to make all plotting regions
  # equal, since we need the largest extent
  # of all stake series.
  stake_offset <- numeric(year_data$nstakes_annual)
  for (annual_stake_id in 1:year_data$nstakes_annual) {
    stake_offset[annual_stake_id] <- year_data$mod_output_annual_cur$stakes_series_mod_all[year_data$mod_output_annual_cur$stakes_start_ids_corr[annual_stake_id],annual_stake_id]
  }
  
  # Compute plot ranges. Same for all plots.
  # We have to extend the range to include all
  # measured values as well as all modeled values,
  # accounting for the stake_offset which is used
  # to set the modeled stake series to 0 at the date
  # of stake measurement.
  stakes_mb_lims <- (range(c(year_data$mod_output_annual_cur$stakes_mb_meas, range(year_data$mod_output_annual_cur$stakes_series_mod_all) + c(min(0,-max(stake_offset)), max(0,-min(stake_offset)))))) / 1e3
  
  
  # Setup month labels.
  days <- seq.Date(year_data$model_time_bounds[1]-1, year_data$model_time_bounds[2], by = "1 day")
  months_labels_all <- format(days, "%b")
  months_doy <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  
  months_labels_ids <- which(as.integer(format(days, "%j")) %in% months_doy) # Select the day at the middle of each month.
  months_labels_df <- data.frame(day_id = day_ids[months_labels_ids],
                                 label = months_labels_all[months_labels_ids])
  # Don't add label for the first month unless it is
  # represented by at least 28 days, and same for the last month.
  # Also see func_plot_massbal_cumul for an explanation of the if() conditions.
  months_cur_rle <- rle(as.integer(format(days, "%m")))
  if ((months_cur_rle$lengths[1] < 28) && (as.integer(format(days[1], "%d")) < 15)) { 
    months_labels_df <- months_labels_df[-1,]
  }
  if ((months_cur_rle$lengths[length(months_cur_rle$lengths)] < 28) && (as.integer(format(days[length(days)], "%d")) > 15)) { # Same, for last month.
    months_labels_df <- months_labels_df[-nrow(months_labels_df),]
  }
  
  
  # Loop to plot all stakes.
  for (annual_stake_id in 1:year_data$nstakes_annual) {
    
    stake_mod_series <- year_data$mod_output_annual_cur$stakes_series_mod_all[,annual_stake_id]
    stake_start_id <- year_data$mod_output_annual_cur$stakes_start_ids_corr[annual_stake_id]
    stake_end_id <- year_data$mod_output_annual_cur$stakes_end_ids[annual_stake_id]
    stake_mod_series_offset <- stake_mod_series - stake_mod_series[stake_start_id]
    stake_mod_df <- data.frame(day_id = day_ids, mb = stake_mod_series_offset)
    stake_meas_df <- data.frame(day_id = stake_end_id - day_id_offset,
                                mb = year_data$mod_output_annual_cur$stakes_mb_meas[annual_stake_id])
    base_size <- 12 # For the plots
    theme_stakes_plots <- theme_bw(base_size = base_size) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.length.x = unit(0, "pt"),
            plot.title = element_text(hjust = 0.5),
            text = element_text(face = "bold"),
            panel.grid = element_blank(),
            plot.margin = margin(0.2,0.2,0.2,0.2,"cm"))
    
    plots_stakes[[annual_stake_id]] <-
      ggplot(stake_mod_df) +
      geom_hline(yintercept = 0, linetype = "longdash", size = 0.3) +
      geom_vline(xintercept = c(stake_start_id, stake_end_id) - day_id_offset, linetype = "longdash", color = "#FF00FF", size = 0.4) +
      geom_vline(xintercept = month_start_ids, linetype = "dashed", color = "#C0C0C0", size = 0.2) +
      annotate("text", x = months_labels_df$day_id, y = -Inf, label = months_labels_df$label, vjust = -1, fontface = "bold", size = base_size * 0.2) +
      geom_line(aes(x = day_id, y = mb/1e3)) +
      geom_point(data = stake_meas_df, aes(x = day_id, y = mb/1e3), shape = 5, stroke = 1.2, size = 1) +
      annotation_custom(grobTree(textGrob(paste0(year_data$massbal_annual_meas_cur$id[annual_stake_id], ": ", sprintf("%+.2f", -year_data$mod_output_annual_cur$stakes_bias[annual_stake_id]/1e3), " m w.e."), x=0.05, y = 0.3, hjust = 0,
                                          gp=gpar(fontsize = base_size, fontface="bold")))) +
      scale_x_continuous(expand = expansion(0,0)) +
      scale_y_continuous(limits = stakes_mb_lims, breaks = pretty(stakes_mb_lims, n = 3), expand = expansion(mult = c(0.12,0.07))) +
      ylab("Mass balance [m w.e.]") +
      theme_stakes_plots
  }
  
  # Each element of this list holds up to
  # 10 aligned plots (i.e. one full page
  # of the output PDF).
  plots_stakes_out <- list()
  n_pages_out <- ceiling(year_data$nstakes_annual / 10)
  for (page_id in 1:n_pages_out) {
    plots_stakes_out[[page_id]] <- plot_grid(plotlist = plots_stakes[((page_id-1)*10+1):(page_id*10)], align = "hv", ncol = 2)
  }
  
  return(plots_stakes_out)
  
}
