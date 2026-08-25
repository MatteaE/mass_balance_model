###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the generation of the plot of daily SCAF time series.        #
###################################################################################################  

func_plot_scaf <- function(year_data,
                           run_params) {
  
  # Prepare the data for plotting.
  scaf_df        <- data.frame(date    = seq.Date(year_data$model_time_bounds[1]-1, year_data$model_time_bounds[2], by = "1 day"),
                               scaf    = year_data$gl_scaf_daily)
  day_id_offset  <- (length(scaf_df$date) - as.integer(format(scaf_df$date[length(scaf_df$date)], "%j"))) + 1
  scaf_df$day_id <- seq_along(scaf_df$date) - day_id_offset # So that day_id = 0 is Jan 1.
  
  # Setup vertical lines dividing months.
  month_starts    <- seq.Date(from = as.Date(paste0(format(year_data$model_time_bounds[1], "%Y/%m"), "/01")),
                              to   = as.Date(paste0(format(year_data$model_time_bounds[2], "%Y/%m"), "/01")),
                              by   = "1 month")
  month_start_ids <- setdiff(as.integer(month_starts[2:length(month_starts)] - year_data$model_time_bounds[1]) + 2 - day_id_offset, 0)
  
  # Setup month labels.
  months_labels_all <- format(scaf_df$date, "%b")
  months_doy <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  # Select the day at the middle of each month.
  # If the simulation starts after day 15 of the first month,
  # the first item of months_labels_ids refers to the second
  # month of the simulation.
  months_labels_ids <- which(as.integer(format(scaf_df$date, "%j")) %in% months_doy)
  # In the SCAF plot, there is a little margin added to the sides.
  # So, we only draw month labels if they have at least a few days
  # worth of space between the label center (middle of the month)
  # and the margin.
  months_labels_ids <- months_labels_ids[which((months_labels_ids >= 4) & (months_labels_ids <= year_data$model_annual_days_n-3))]
  months_labels_df  <- data.frame(day_id = scaf_df$day_id[months_labels_ids],
                                  label  = months_labels_all[months_labels_ids])

    
  base_size <- 16 # For the plots
  theme_scaf_plot <- theme_bw(base_size = base_size) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank())
  
  plot_scaf <- ggplot(scaf_df) +
    annotate("text", x = months_labels_df$day_id, y = -Inf, label = months_labels_df$label, hjust = 0.45, vjust = -1, fontface = "bold", size = 5) +
    # geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.5) +
    {if (run_params$show_month_lines) geom_vline(xintercept = month_start_ids, linetype = "dashed", color = "#C0C0C0", linewidth = 0.4)} +
    geom_line(aes(x = day_id, y = scaf), linewidth = 0.7) +
    scale_x_continuous(expand = expansion(mult = 0.02)) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0,100,25),
                       expand = expansion(mult = c(0.10,0.05))) +
    ylab(paste0("Snow-covered area fraction [%]")) +
    theme_scaf_plot
  
  
  return(plot_scaf)
}
