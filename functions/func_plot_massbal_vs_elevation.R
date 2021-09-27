###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the plotting routine for annual mass balance distribution    #
#                 versus elevation, both in elevation bands and as a scatterplot for all cells.   #
################################################################################################### 

# The function returns a 2-element list:
# the plotting df and the resulting plots.
# The plotting df is used later to save some
# overview values.
func_plot_massbal_vs_elevation <- function(year_data,
                                           run_params,
                                           data_dems) {
  
  if (year_data$nstakes_annual > 0) {
    mb_meas_period_corr_values <- getValues(year_data$massbal_annual_maps$meas_period_corr)
  }
  
  plots_mb_vs_ele <- list()
  
  #### Plot #1: all the mass balance profiles with elevation bands ####
  # Also the number of cells in each elevation band.
  ele_bands_plot_values <- getValues(data_dems$elevation_bands_plot[[year_data$dem_grid_id]])
  ele_bands_plot_min    <- min(ele_bands_plot_values, na.rm = T)
  ele_bands_plot_max    <- max(ele_bands_plot_values, na.rm = T)
  ele_bands_plot_df     <- data.frame(ele                 = seq(ele_bands_plot_min, ele_bands_plot_max, run_params$ele_bands_plot_size),
                                      ncells              = NA,
                                      mb_annual_meas_corr = NA,
                                      mb_annual_meas      = NA,
                                      mb_annual_hydro     = NA,
                                      # mb_annual_fixed     = NA,
                                      mb_winter_fixed     = NA,
                                      mb_winter_meas      = NA)
  for (band_id in 1:nrow(ele_bands_plot_df)) {
    band_cell_ids                                  <- which(ele_bands_plot_values == ele_bands_plot_df$ele[band_id])
    ele_bands_plot_df$ncells[band_id]              <- length(band_cell_ids)
    if (year_data$nstakes_annual > 0) {
      ele_bands_plot_df$mb_annual_meas_corr[band_id] <- mean(mb_meas_period_corr_values[band_cell_ids])
      ele_bands_plot_df$mb_annual_meas[band_id]      <- mean(getValues(year_data$massbal_annual_maps$meas_period)[band_cell_ids])
    }
    ele_bands_plot_df$mb_annual_hydro[band_id]     <- mean(getValues(year_data$massbal_annual_maps$hydro)[band_cell_ids])
    # ele_bands_plot_df$mb_annual_fixed[band_id]     <- mean(getValues(year_data$massbal_annual_maps$fixed)[band_cell_ids])
    ele_bands_plot_df$mb_winter_fixed[band_id]     <- mean(getValues(year_data$massbal_winter_maps$fixed)[band_cell_ids])
    if (year_data$process_winter) {
      ele_bands_plot_df$mb_winter_meas[band_id]    <- mean(getValues(year_data$massbal_winter_maps$meas_period)[band_cell_ids])
    }
  }
  
  # Convert the data frame into a shape suitable for multi-color plot.
  # The na.omit() also removes the empty mb_winter_meas values if we don't have winter measurements.
  ele_bands_plot_df_melt <- na.omit(melt(ele_bands_plot_df, id.vars = c("ele", "ncells")))
  # Re-order the data frame so that the final mass balance profile is plotted on top of the others.
  ele_bands_plot_df_melt$variable <- factor(ele_bands_plot_df_melt$variable, levels = c("mb_annual_meas", "mb_annual_hydro", # DEV NOTE: first one would have been "mb_annual_fixed", but we have disabled that period.
                                                                                        "mb_winter_fixed", "mb_winter_meas",
                                                                                        "mb_annual_meas_corr"))
  
  base_size <- 16 # For the plot
  theme_elebands_plot <- theme_bw(base_size = base_size) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.position = c(0.4,0.85),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.title = element_blank())
  
  # Data on the number of cells within each elevation band.
  # We plot it as a histogram.
  dat_ncells <- data.frame(ele    = c(ele_bands_plot_df$ele[1] - rep(25,2), rep(ele_bands_plot_df$ele, each = 2) + 25),
                           ncells = c(0, rep(ele_bands_plot_df$ncells, each = 2), 0))
  
  # Below: you can use geom_rect() instead of geom_polygon_pattern()
  # if there is a problem with package ggpattern.
  plots_mb_vs_ele[[1]] <-  ggplot(ele_bands_plot_df_melt) +
    # geom_rect(data = ele_bands_plot_df, aes(xmin = ele - run_params$ele_bands_plot_size/2, xmax = ele + run_params$ele_bands_plot_size/2, ymin = min(ele_bands_plot_df_melt$value) / 1e3, ymax = ncells * (max(ele_bands_plot_df_melt$value) - min(ele_bands_plot_df_melt$value)) / (1e3 * 4 * max(ncells)) + min(ele_bands_plot_df_melt$value) / 1e3)) +
    geom_polygon_pattern(data = dat_ncells,
                         aes(x = ele, y = ncells * (max(ele_bands_plot_df_melt$value) - min(ele_bands_plot_df_melt$value)) / (1e3 * 4 * max(ncells)) + min(ele_bands_plot_df_melt$value) / 1e3),
                         fill = "#FFFFFF", color = "#000000",
                         pattern_fill = "#000000", pattern_colour = "#000000",
                         pattern_angle = 35, pattern_size = 0.1, pattern_spacing = 0.02, pattern_density = 0.05) +
    geom_hline(yintercept = 0, size = 0.4) +
    geom_line(aes(x = ele, y = value / 1e3, color = variable), size = 1) +
    scale_color_manual(breaks = c("mb_annual_meas", "mb_annual_hydro", "mb_winter_fixed", "mb_winter_meas", "mb_annual_meas_corr"), # DEV NOTE: first one would have been "mb_annual_fixed", but we have disabled that period.
                       values = c("#FF0000", "#FF9000", "#0000FF", "#8080FF", "#000000"), # "#8C00D4" was the first color, for mb_annual_fixed.
                       labels = c("Annual, measurement period", "Annual, hydrological year", # DEV NOTE: first one would have been "Annual, fixed dates", but we have disabled that period.
                                  "Winter, fixed dates", "Winter, measurement period", "Annual, final")) +
    scale_y_continuous(breaks = pretty(ele_bands_plot_df_melt$value / 1e3), expand = expansion(0,0)) +
                       # Optional: secondary horizontal axis with the number of cells for each elevation band (not strictly necessary).
                       #sec.axis = sec_axis(~ (. - min(ele_bands_plot_df_melt$value/1e3)) * 4 * max(ele_bands_plot_df$ncells) / ((max(ele_bands_plot_df_melt$value) - min(ele_bands_plot_df_melt$value))/1e3)  )) +
    scale_x_continuous(expand = expansion(0,0)) +
    coord_flip() +
    xlab("Elevation [m a.s.l.]") +
    ylab("Mass balance [m w.e.]") +
    theme_elebands_plot
  
  
  
  #### Plot #2: scatterplot of (uncorrected) mass balance over the measurement period, vs elevation ####
  # Do this only if we have stake measurements, else it's useless.
  if (year_data$nstakes_annual > 0) {
    # Also the stake measurements, **standardized over the measurement period**.
    # The reported BIAS and RMS are computed over the whole measurement period,
    # comparing the stake standardized mass balance to the model result over the
    # measurement period. BIAS and RMS are the same as over each individual stake
    # period, since stake standardization uses the model output which by definition
    # cannot add BIAS or RMS w.r.t. the model output itself.
    df_bias_rms <- data.frame(meas = year_data$massbal_annual_meas_cur$massbal_standardized/1e3,
                              mod = extract(year_data$massbal_annual_maps$meas_period, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear") / 1e3)
    stakes_bias <- mean(df_bias_rms$mod - df_bias_rms$meas)
    stakes_rms <- sqrt(mean((df_bias_rms$mod - df_bias_rms$meas)^2))
    
    stakes_mod_massbal_meas_period <- year_data$mod_output_annual_cur$stakes_series_mod_all[year_data$massbal_annual_meas_period_ids[2],] - year_data$mod_output_annual_cur$stakes_series_mod_all[year_data$massbal_annual_meas_period_ids[1],]
    
    # This data.frame contains only the mass balance values on glaciated cells.
    df_scatterplot <- data.frame(ele = data_dems$elevation[[year_data$dem_grid_id]][data_dems$glacier_cell_ids[[year_data$dem_grid_id]]],
                                 mb = getValues(year_data$massbal_annual_maps$meas_period)[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]])
    
    df_stakes <- data.frame(z    = year_data$massbal_annual_meas_cur$z_dem,
                            meas = year_data$massbal_annual_meas_cur$massbal_standardized,
                            mod  = stakes_mod_massbal_meas_period)
    
    theme_scatterplot_ele <- theme_bw(base_size = base_size) +
      theme(text = element_text(face = "bold"),
            panel.grid = element_blank())
    
    plots_mb_vs_ele[[2]] <- ggplot(df_scatterplot) +
      annotation_custom(grobTree(textGrob(paste0("Bias: ", sprintf("%+.3f", stakes_bias), " m w.e."), x=0.02, y = 0.95, hjust = 0,
                                          gp=gpar(fontsize = base_size, fontface="bold")))) +
      annotation_custom(grobTree(textGrob(paste0("RMS: ", sprintf("%.3f", stakes_rms), " m w.e."), x=0.02, y = 0.87, hjust = 0,
                                          gp=gpar(fontsize = base_size, fontface="bold")))) +
      geom_point(aes(x = ele, y = mb/1e3), color = "#FF0000", size = 0.5, stroke = 0) +
      geom_point(data = df_stakes, aes(x = z, y = meas/1e3), shape = 3, stroke = 1.5, size = 0) +
      geom_segment(data = df_stakes, aes(x = z, xend = z, y = meas/1e3, yend = mod/1e3)) +
      coord_flip() +
      scale_x_continuous(breaks = pretty(df_scatterplot$ele), expand = expansion(mult = 0.05)) +
      scale_y_continuous(expand = expansion(mult = 0.05)) +
      xlab("Elevation [m a.s.l.]") +
      ylab("Mass balance [m w.e.]") +
      theme_scatterplot_ele
  }
  
  
  plots_mb_vs_ele_out <- plot_grid(plotlist = plots_mb_vs_ele, align = "hv", ncol = 1, nrow = 2)
  
  return(list(ele_bands_plot_df   = ele_bands_plot_df,
              plots_mb_vs_ele_out = plots_mb_vs_ele_out))
  
}
