func_plot_weather_series <- function(year_data,
                                     run_params) {
  
  plots <- list()
  
  base_size <- 16 # For the plot
  theme_weather_plot <- theme_bw(base_size = base_size) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.length.x = unit(0, "pt"),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank())
  
  date_df <- data.frame(date  = seq.Date(year_data$model_time_bounds[1]-1, year_data$model_time_bounds[2], by = "1 day"))
  day_id_offset <- (length(date_df$date) - as.integer(format(date_df$date[length(date_df$date)], "%j"))) + 1
  date_df$day_id <- seq_along(date_df$date) - day_id_offset # So that day_id = 0 is Jan 1.
  
  # Setup month labels.
  months_labels_all <- format(date_df$date, "%b")
  months_doy <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  
  months_labels_ids <- which(as.integer(format(date_df$date, "%j")) %in% months_doy)
  months_labels_df <- data.frame(day_id = date_df$day_id[months_labels_ids],
                                 label  = months_labels_all[months_labels_ids])
  months_cur_rle <- rle(as.integer(format(date_df$date, "%m")))
  if ((months_cur_rle$lengths[1] < 28) && (as.integer(format(date_df$date[1], "%d")) < 15)) { 
    months_labels_df <- months_labels_df[-1,]
  }
  if ((months_cur_rle$lengths[length(months_cur_rle$lengths)] < 28) && (as.integer(format(date_df$date[nrow(date_df)], "%d")) > 15)) { # Same, for last month.
    months_labels_df <- months_labels_df[-nrow(months_labels_df),]
  }
  months_labels_df$date <- date_df$date[match(months_labels_df$day_id, date_df$day_id)]
  
  # Air temperature red/blue ribbon.
  dat_Tair <- year_data$weather_series_annual_cur[,c(1,6)]
  for (i in 2:nrow(dat_Tair)) {
    if (dat_Tair$t2m_mean[i-1] * dat_Tair$t2m_mean[i] < 0) {
      dat_Tair <- rbind(dat_Tair[1:(i-1),], data.frame(timestamp = dat_Tair$timestamp[i-1] + (dat_Tair$timestamp[i] - dat_Tair$timestamp[i-1])/2, t2m_mean = 0), dat_Tair[i:nrow(dat_Tair),])
    }
  }
  dat_Tair$Tair_pos <- pmax(dat_Tair$t2m_mean,0)
  dat_Tair$Tair_neg <- pmin(dat_Tair$t2m_mean,0)
  
  # Setup vertical lines dividing months.
  month_starts <- setdiff(intersect(dat_Tair$timestamp,
                                    seq.Date(from = as.Date(paste0(format(year_data$model_time_bounds[1], "%Y/%m"), "/01")),
                                             to   = as.Date(paste0(format(year_data$model_time_bounds[2], "%Y/%m"), "/01")),
                                             by   = "1 month")),
                          as.Date(paste0(format(year_data$model_time_bounds[2], "%Y"), "/01/01")))                          
  
  plots[[1]] <- ggplot(dat_Tair) +
    geom_ribbon(aes(x = timestamp, ymin = 0, ymax = Tair_pos), fill = "#FF0000") +
    geom_ribbon(aes(x = timestamp, ymin = Tair_neg, ymax = 0), fill = "#0000FF") +
    geom_vline(xintercept = date_df$date[date_df$day_id == 0], linetype = "dashed", linewidth = 0.5) +
    {if (run_params$show_month_lines) geom_vline(xintercept = month_starts, linetype = "dashed", color = "#C0C0C0", linewidth = 0.4)} +
    annotate("text", x = months_labels_df$date, y = -Inf, label = months_labels_df$label, vjust = -1, fontface = "bold", size = 5) +
    scale_x_date(expand = expansion(0,0)) +
    ylab("AWS daily mean air temperature [\u00B0C]") +
    theme_weather_plot
  
  
  # Daily and monthly precipitation.
  months_cur_rle_new <- rle(as.integer(format(date_df$date[2:nrow(date_df)], "%m")))
  dat_precip <- year_data$weather_series_annual_cur[,c(1,7)]
  dat_precip_monthly <- data.frame(month      = months_cur_rle_new$values,
                                   date_start = dat_precip$timestamp[1], # Initialize already with Date object, to have correct time.
                                   date_end   = dat_precip$timestamp[1],
                                   precip_sum = NA,
                                   id_end     = cumsum(months_cur_rle_new$lengths))
  dat_precip_monthly$id_start                 <- c(1, (dat_precip_monthly$id_end + 1)[1:(nrow(dat_precip_monthly)-1)])
  for (month_id in 1:nrow(dat_precip_monthly)) {
    dat_precip_monthly$date_start[month_id] <- dat_precip$timestamp[dat_precip_monthly$id_start[month_id]]
    dat_precip_monthly$date_end[month_id]   <- dat_precip$timestamp[dat_precip_monthly$id_end[month_id]] + 1 # +1 to have touching rectangles.
    dat_precip_monthly$precip_sum[month_id] <- sum(dat_precip$precip[dat_precip_monthly$id_start[month_id]:dat_precip_monthly$id_end[month_id]])
  }
  skip_first <- (dat_precip_monthly$precip_sum[1] == 0) && ((dat_precip_monthly$id_end[1] - dat_precip_monthly$id_start[1]) < 10)
  if (skip_first) {dat_precip_monthly <- dat_precip_monthly[-1,]}
  id_last <- nrow(dat_precip_monthly)
  skip_last <- (dat_precip_monthly$precip_sum[id_last] == 0) && ((dat_precip_monthly$id_end[id_last] - dat_precip_monthly$id_start[id_last]) < 10)
  if (skip_last) {dat_precip_monthly <- dat_precip_monthly[-id_last,]}
  
  plots[[2]] <- ggplot(dat_precip) +
    geom_rect(data = dat_precip_monthly, aes(xmin = date_start, xmax = date_end, ymin = 0, ymax = precip_sum),
              fill = "#A4CBE0", color = "#00000000") +
    geom_col(aes(x = timestamp, y = precip), fill = "#000088", color = "#00004400") +
    geom_vline(xintercept = date_df$date[date_df$day_id == 0], linetype = "dashed", linewidth = 0.5) +
    {if (run_params$show_month_lines) geom_vline(xintercept = month_starts, linetype = "dashed", color = "#C0C0C0", linewidth = 0.4)} +
    annotate("text", x = months_labels_df$date, y = Inf, label = months_labels_df$label, vjust = 2, fontface = "bold", size = 5) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_x_date(expand = expansion(0,0)) +
    ylab("AWS daily and monthly precipitation [mm]") +
    theme_weather_plot
  
  # Align panels.
  plots_out <- plot_grid(plotlist = plots, align = "hv", ncol = 1, nrow = 2)
  
  return(plots_out)
} 
