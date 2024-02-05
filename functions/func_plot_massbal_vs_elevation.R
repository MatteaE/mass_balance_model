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
    mb_meas_period_corr_values <- values(year_data$massbal_annual_maps$meas_period_corr)[,1]
  }
  
  plots_mb_vs_ele <- list()
  
  #### Plot #1: line plot of all mass balance profiles. Also bar chart of hypsometry --------------
  ele_bands_plot_values <- values(data_dems$elevation_bands_plot[[year_data$dem_grid_id]])[,1]
  ele_bands_plot_min    <- min(ele_bands_plot_values, na.rm = T)
  ele_bands_plot_max    <- max(ele_bands_plot_values, na.rm = T)
  ele_bands_plot_df     <- data.frame(ele                 = seq(ele_bands_plot_min, ele_bands_plot_max, run_params$ele_bands_plot_size),
                                      ncells              = NA_integer_,
                                      area_km2            = NA_real_,
                                      mb_annual_meas_corr = NA_real_,
                                      mb_annual_meas      = NA_real_,
                                      mb_annual_hydro     = NA_real_,
                                      # mb_annual_fixed     = NA_real_,
                                      mb_winter_fixed     = NA_real_,
                                      mb_winter_meas      = NA_real_)
  for (band_id in 1:nrow(ele_bands_plot_df)) {
    band_cell_ids                                  <- which(ele_bands_plot_values == ele_bands_plot_df$ele[band_id])
    ele_bands_plot_df$ncells[band_id]              <- length(band_cell_ids)
    ele_bands_plot_df$area_km2[band_id]            <- ele_bands_plot_df$ncells[band_id] * (run_params$grid_cell_size * run_params$grid_cell_size) / 1e6
    if (year_data$nstakes_annual > 0) {
      ele_bands_plot_df$mb_annual_meas_corr[band_id] <- mean(mb_meas_period_corr_values[band_cell_ids]) * run_params$output_mult / 1000
      ele_bands_plot_df$mb_annual_meas[band_id]      <- mean(values(year_data$massbal_annual_maps$meas_period)[band_cell_ids]) * run_params$output_mult / 1000
    }
    ele_bands_plot_df$mb_annual_hydro[band_id]     <- mean(values(year_data$massbal_annual_maps$hydro)[band_cell_ids]) * run_params$output_mult / 1000
    # ele_bands_plot_df$mb_annual_fixed[band_id]     <- mean(values(year_data$massbal_annual_maps$fixed)[band_cell_ids]) * run_params$output_mult / 1000
    ele_bands_plot_df$mb_winter_fixed[band_id]     <- mean(values(year_data$massbal_winter_maps$fixed)[band_cell_ids]) * run_params$output_mult / 1000
    if (year_data$process_winter) {
      ele_bands_plot_df$mb_winter_meas[band_id]    <- mean(values(year_data$massbal_winter_maps$meas_period)[band_cell_ids]) * run_params$output_mult / 1000
    }
  }
  
  # Convert the data frame into a shape suitable for multi-color plot.
  # The na.omit() also removes the empty mb_winter_meas values if we don't have winter measurements.
  ele_bands_plot_df_melt <- na.omit(melt(ele_bands_plot_df,
                                         id.vars = c("ele", "ncells"),
                                         measure.vars = intersect(names(ele_bands_plot_df), # intersect() because we want to drop the area_km2 column.
                                                                  c("mb_annual_meas", "mb_annual_hydro",
                                                                    "mb_winter_fixed", "mb_winter_meas",
                                                                    "mb_annual_meas_corr"))))
  # Re-order the data frame so that the final (meas_corr) mass balance profile is plotted on top of the others.
  ele_bands_plot_df_melt$variable <- factor(ele_bands_plot_df_melt$variable, levels = c("mb_annual_meas", "mb_annual_hydro", # DEV NOTE: first one would have been "mb_annual_fixed", but we have disabled that period.
                                                                                        "mb_winter_fixed", "mb_winter_meas",
                                                                                        "mb_annual_meas_corr"))
  
  base_size <- 16 # For the plot
  theme_elebands_plot <- theme_bw(base_size = base_size) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.position = c(0.5,0.85),
          legend.justification = 0.5,
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
    # geom_rect(data = ele_bands_plot_df, aes(xmin = ele - run_params$ele_bands_plot_size/2, xmax = ele + run_params$ele_bands_plot_size/2, ymin = min(ele_bands_plot_df_melt$value), ymax = ncells * (max(ele_bands_plot_df_melt$value) - min(ele_bands_plot_df_melt$value)) / (4 * max(ncells)) + min(ele_bands_plot_df_melt$value))) +
    geom_polygon_pattern(data = dat_ncells,
                         aes(x = ele, y = ncells * (max(ele_bands_plot_df_melt$value) - min(ele_bands_plot_df_melt$value)) / (4 * max(ncells)) + min(ele_bands_plot_df_melt$value)),
                         fill = "#FFFFFF", color = "#000000",
                         pattern_fill = "#000000", pattern_colour = "#000000",
                         pattern_angle = 35, pattern_size = 0.1, pattern_spacing = 0.02, pattern_density = 0.05) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_line(aes(x = ele, y = value, color = variable), linewidth = 1) +
    scale_color_manual(breaks = c("mb_annual_meas", "mb_annual_hydro", "mb_winter_fixed", "mb_winter_meas", "mb_annual_meas_corr"), # DEV NOTE: first one would have been "mb_annual_fixed", but we have disabled that period.
                       values = c("#FF0000", "#FF9000", "#0000FF", "#8080FF", "#000000"), # "#8C00D4" was the first color, for mb_annual_fixed.
                       labels = c("Annual, measurement period", "Annual, hydrological year", # DEV NOTE: first one would have been "Annual, fixed dates", but we have disabled that period.
                                  "Winter, fixed dates", "Winter, measurement period", "Annual, measurement period + contour-line")) +
    scale_y_continuous(breaks = pretty(ele_bands_plot_df_melt$value), expand = expansion(mult = c(0,0.05),0)) +
                       # Optional: secondary horizontal axis with the number of cells for each elevation band (not strictly necessary).
                       #sec.axis = sec_axis(~ (. - min(ele_bands_plot_df_melt$value)) * 4 * max(ele_bands_plot_df$ncells) / ((max(ele_bands_plot_df_melt$value) - min(ele_bands_plot_df_melt$value)))  )) +
    scale_x_continuous(expand = expansion(0,0)) +
    coord_flip() +
    xlab("Elevation [m a.s.l.]") +
    ylab(paste0("Mass balance [", run_params$output_unit, " w.e.]")) +
    theme_elebands_plot
  
  
  
  #### Plot #2: scatterplot of annual mass balance ------------------------------------------------
  # We plot the not-band-corrected model result over the measurement period, and the stakes standardized over the same period.
  # Do this only if we have stake measurements, else it's useless.
  if (year_data$nstakes_annual > 0) {
    # We plot the stake measurements, in black, **standardized over the measurement period**.
    # The reported RMS and BIAS refer to the not-standardized stakes: simply each stake
    # compared over the respective period).
    # Like this, there is consistency between the map and profile plots, and also,
    # the stake is compared to the proper cells (stakes at the edges use
    # nearest-neighbor instead of bilinear).
    stakes_bias <- year_data$mod_output_annual_cur$global_bias / 1e3
    stakes_rms <- year_data$mod_output_annual_cur$global_rms / 1e3
    
    stakes_mod_massbal_meas_period <- year_data$mod_output_annual_cur$stakes_series_mod_all[year_data$massbal_annual_meas_period_ids[2],] - year_data$mod_output_annual_cur$stakes_series_mod_all[year_data$massbal_annual_meas_period_ids[1],]
    
    # This data.frame contains only the mass balance values on glacierized cells.
    df_scatterplot <- data.frame(ele = data_dems$elevation[[year_data$dem_grid_id]][data_dems$glacier_cell_ids[[year_data$dem_grid_id]]][,1],
                                 mb = values(year_data$massbal_annual_maps$meas_period)[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]] * run_params$output_mult)
    
    df_stakes <- data.frame(z    = year_data$massbal_annual_meas_cur$z_dem,
                            meas = year_data$massbal_annual_meas_cur$massbal_standardized * run_params$output_mult,
                            mod  = stakes_mod_massbal_meas_period * run_params$output_mult)
    
    theme_scatterplot_ele <- theme_bw(base_size = base_size) +
      theme(text = element_text(face = "bold"),
            panel.grid = element_blank())
    
    plots_mb_vs_ele[[2]] <- ggplot(df_scatterplot) +
      annotation_custom(grobTree(textGrob(paste0("Bias: ", sprintf(run_params$output_fmt3, stakes_bias*run_params$output_mult), " ", run_params$output_unit, " w.e."), x=0.02, y = 0.95, hjust = 0,
                                          gp=gpar(fontsize = base_size, fontface="bold")))) +
      annotation_custom(grobTree(textGrob(paste0("RMS: ", sprintf(run_params$output_fmt1, stakes_rms*run_params$output_mult), " ", run_params$output_unit, " w.e."), x=0.02, y = 0.87, hjust = 0,
                                          gp=gpar(fontsize = base_size, fontface="bold")))) +
      geom_point(aes(x = ele, y = mb/1e3), color = "#FF0000", size = 0.5, stroke = 0) +
      geom_point(data = df_stakes, aes(x = z, y = meas/1e3), shape = 3, stroke = 1.5, size = 0) +
      geom_segment(data = df_stakes, aes(x = z, xend = z, y = meas/1e3, yend = mod/1e3)) +
      coord_flip() +
      scale_x_continuous(breaks = pretty(df_scatterplot$ele), expand = expansion(mult = 0.05)) +
      scale_y_continuous(expand = expansion(mult = 0.05)) +
      xlab("Elevation [m a.s.l.]") +
      ylab(paste0("Annual mass balance [", run_params$output_unit, " w.e.]")) +
      theme_scatterplot_ele
  }
  
  
  #### Plot #3: scatterplot of winter mass balance ------------------------------------------------
  # We plot the not-band-corrected model over the winter measurement period, and the stakes standardized over the same period.
  # Do this only if we have stake measurements, else it's useless.
  if (year_data$nstakes_winter > 0) {
    # We plot the stake measurements, in black, **standardized over the winter measurement period**.
    # The reported RMS and BIAS (computed within func_massbal_postprocess())
    # are from the annual modeling (thus referred to the not-standardized
    # stakes, rather, each stake compared over the respective period).
    # Like this, there is consistency between the map and profile plots.
    # And also, the stake is compared to the proper cells (stakes at the edges use
    # nearest-neighbor instead of bilinear).
    stakes_bias <- year_data$mod_output_annual_cur$global_bias_winter / 1e3
    stakes_rms <- year_data$mod_output_annual_cur$global_rms_winter / 1e3
    
    # Computed within func_massbal_postprocess().
    stakes_mod_massbal_meas_period <- year_data$mod_output_annual_cur$stakes_winter_measperiod_mb
    
    # This data.frame contains only the mass balance values on glacierized cells.
    df_scatterplot <- data.frame(ele = data_dems$elevation[[year_data$dem_grid_id]][data_dems$glacier_cell_ids[[year_data$dem_grid_id]]][,1],
                                 mb  = values(year_data$massbal_winter_maps$meas_period)[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]] * run_params$output_mult)
    
    df_stakes <- data.frame(z    = year_data$massbal_winter_meas_cur$z_dem,
                            meas = year_data$massbal_winter_meas_cur$massbal_standardized * run_params$output_mult,
                            mod  = stakes_mod_massbal_meas_period * run_params$output_mult)
    
    theme_scatterplot_ele <- theme_bw(base_size = base_size) +
      theme(text = element_text(face = "bold"),
            panel.grid = element_blank())
    
    plots_mb_vs_ele[[3]] <- ggplot(df_scatterplot) +
      annotation_custom(grobTree(textGrob(paste0("Bias: ", sprintf(run_params$output_fmt3, stakes_bias*run_params$output_mult), " ", run_params$output_unit, " w.e."), x=0.02, y = 0.95, hjust = 0,
                                          gp=gpar(fontsize = base_size, fontface="bold")))) +
      annotation_custom(grobTree(textGrob(paste0("RMS: ", sprintf(run_params$output_fmt1, stakes_rms*run_params$output_mult), " ", run_params$output_unit, " w.e."), x=0.02, y = 0.87, hjust = 0,
                                          gp=gpar(fontsize = base_size, fontface="bold")))) +
      geom_point(aes(x = ele, y = mb/1e3), color = "#0000FF", size = 0.5, stroke = 0) +
      geom_point(data = df_stakes, aes(x = z, y = meas/1e3), shape = 3, stroke = 1.5, size = 0) +
      geom_segment(data = df_stakes, aes(x = z, xend = z, y = meas/1e3, yend = mod/1e3)) +
      coord_flip() +
      scale_x_continuous(breaks = pretty(df_scatterplot$ele), expand = expansion(mult = 0.05)) +
      scale_y_continuous(limits = c(min(0.0, df_scatterplot$mb, df_stakes$meas), NA_real_),
                         expand = expansion(mult = 0.05)) +
      xlab("Elevation [m a.s.l.]") +
      ylab(paste0("Winter mass balance [", run_params$output_unit, " w.e.]")) +
      theme_scatterplot_ele
  }
  
  # df_bias_rms <- data.frame(meas = year_data$massbal_annual_meas_cur$massbal_standardized / 1e3,
                            # mod  = extract(year_data$massbal_annual_maps$meas_period, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear")[,1] / 1e3)
  
  
  # Arrange plots into pages according
  # to plot number (we can have 1, 2 or 3 plots,
  # we want max 2 plots per page, with fixed height
  # in all cases).
  if (length(plots_mb_vs_ele) < 3) {
    plots_mb_vs_ele_out <- list(plot_grid(plotlist = plots_mb_vs_ele, align = "hv", ncol = 1, nrow = 2))
  } else {
    plots_mb_vs_ele_out <- list(plot_grid(plotlist = plots_mb_vs_ele[1:2], align = "hv", ncol = 1, nrow = 2),
                                plot_grid(plotlist = plots_mb_vs_ele[3], align = "hv", ncol = 1, nrow = 2))
  }
  
  return(list(ele_bands_plot_df   = ele_bands_plot_df,
              plots_mb_vs_ele_out = plots_mb_vs_ele_out))
  
}
