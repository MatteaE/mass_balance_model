###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which plots modeled mass balance maps.           #
################################################################################################### 


# NOTE: in ggplot2, the geom_sf() command
# which plots the glacier outline is forcing
# the glacier image proportions so that the
# glacier is not distorted.
# This means that the output images can get white
# margins (either above/below or left/right,
# depending on whether the glacier is larger in the
# X or in the Y coordinate).
# Without geom_sf(), the glacier is distorted
# until the image is filled.
func_plot_year_mb_maps <- function(year_data,
                                   run_params,
                                   data_dems,
                                   data_outlines) {
  
  base_size <- 16 # For the plots.
  grid_extent <- ext(data_dems$elevation[[year_data$dem_grid_id]])
  grid_area   <- (grid_extent[2] - grid_extent[1]) * (grid_extent[4] - grid_extent[3])
  # Empirical multiplier to reduce label and line size when the modeled extent is very big.
  # Useful for huge glaciers and multi-glacier (e.g. catchment) simulations.
  extent_size_multiplier <- max(0.1, exp(-(max(0,(grid_area-5e6))^2)/5e17))
  
  # Empirical top margin to keep plots inside page borders
  # when the glacier is tall (aspect ratio > 1.07).
  margin_top <- min(80, max(0, (data_outlines$aspect_ratio[[year_data$outline_id]] - 1.05) * 1200))
  theme_map_massbal <- theme_void(base_size = base_size) +
                       theme(legend.position = "bottom",
                             legend.key.width = unit(3, "cm"),
                             legend.key.height = unit(0.25, "cm"),
                             legend.box.margin = margin(0,0,5,0),
                             legend.title = element_text(vjust = 1, face = "bold", size = 16),
                             legend.text = element_text(face = "bold", size = 12),
                             plot.margin = margin(margin_top,0,0,0, unit = "pt"))
  
  contour_label_textsize <- 4
  contour_linesize <- 0.4
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  y_line_mult <- min(1.5, max(1, (data_outlines$aspect_ratio[[year_data$outline_id]] + 1.5) / 2))
  y_line1 <- 1 + (0.21 / y_line_mult)
  y_line2 <- 1 + (0.12 / y_line_mult)
  y_line3 <- 1 + (0.06 / y_line_mult)
  y_line4 <- 1 + (0.00 / y_line_mult)
  
  palette_RdBu_ext <- c("#33000F", RColorBrewer::brewer.pal(11, "RdBu")[c(1:4,6,8:11)], "#011830")
  # Values exceeding +/- max_mb will be clamped.
  # We need set this so that the colors are well distributed
  # in the scale (else they are too dark or washed out).
  max_mb <- abs(2*run_params$mb_colorscale_breaks[1] - run_params$mb_colorscale_breaks[2])
  
  plot_df_base <- data.frame(crds(data_dems$elevation[[year_data$dem_grid_id]], na.rm = FALSE))
  elevation_df <- data.frame(plot_df_base, z = values(data_dems$elevation[[year_data$dem_grid_id]])[,1])
  
  plots <- list()
  
  #### HYDROLOGICAL YEAR ####
  mb_hydro_lab <- sprintf(run_params$output_fmt1, year_data$massbal_annual_values[["hydro.mean"]] * run_params$output_mult / 1000.)
  plot_df <- plot_df_base
  plot_df$massbal <- values(year_data$massbal_annual_maps$hydro)[,1]
  plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
    geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
    geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off") +
    {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
    {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob("Hydrological year: 10/01 - 09/30",
                                        x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold(b[n]*" = "*.(mb_hydro_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                        x = 0.05, y = y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                      limits = max_mb*c(-1,1),
                      breaks = run_params$mb_colorscale_breaks) +
    theme_map_massbal

  
  
  #### MEASUREMENT PERIOD - ANNUAL ####
  if (year_data$nstakes_annual > 0) {
    
    mb_meas_period_annual_lab <- paste(format(year_data$massbal_annual_meas_period, "%m/%d"), collapse = " - ")
    mb_meas_annual_lab <- sprintf(run_params$output_fmt1,year_data$massbal_annual_values[["meas_period.mean"]] * run_params$output_mult / 1000.)
    plot_df <- plot_df_base
    plot_df$massbal <- values(year_data$massbal_annual_maps$meas_period)[,1]
    plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
      {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Measurement period (annual): ", mb_meas_period_annual_lab),
                                          x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold(b[n]*" = "*.(mb_meas_annual_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    
    #### MEASUREMENT PERIOD - ANNUAL, WITH STAKES ####
    # Also RMS.
    plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
      geom_point(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
      {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
      {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y, label = sprintf(run_params$output_fmt2, massbal_standardized*run_params$output_mult/1e3)), size = 3*extent_size_multiplier, fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Measurement period (annual): ", mb_meas_period_annual_lab),
                                          x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold(b[n]*" = "*.(mb_meas_annual_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      annotation_custom(grobTree(textGrob(paste0("RMS: ", sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$global_rms*run_params$output_mult/1e3), " ", run_params$output_unit, " w.e."),
                                          x = 0.05, y=y_line4, hjust = 0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    
    
    #### MEASUREMENT PERIOD - ANNUAL, CORRECTED WITH CONTOUR LINE METHOD ####
    mb_meas_corr_annual_lab <- sprintf(run_params$output_fmt1,year_data$massbal_annual_values[["meas_period_corr.mean"]] * run_params$output_mult / 1000.)
    plot_df <- plot_df_base
    plot_df$massbal <- values(year_data$massbal_annual_maps$meas_period_corr)[,1]
    plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
      {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Measurement period (annual, corrected): ", mb_meas_period_annual_lab),
                                          x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold(b[n]*" = "*.(mb_meas_corr_annual_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    
    #### MEASUREMENT PERIOD - ANNUAL CORRECTED, WITH STAKES ####
    rmse_bandcorr <- sqrt(mean((year_data$massbal_annual_meas_cur$massbal_standardized - extract(year_data$massbal_annual_maps$meas_period_corr, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear")[,1])^2))
    plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
      geom_point(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
      {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
      {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y, label = sprintf(run_params$output_fmt2, massbal_standardized*run_params$output_mult/1e3)), size = 3*extent_size_multiplier, fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Measurement period (annual, corrected): ", mb_meas_period_annual_lab),
                                          x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold(b[n]*" = "*.(mb_meas_corr_annual_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      annotation_custom(grobTree(textGrob(paste0("RMS: ", sprintf(run_params$output_fmt1, rmse_bandcorr * run_params$output_mult/1e3), " ", run_params$output_unit, " w.e."),
                                          x = 0.05, y=y_line4, hjust = 0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
  } # End of if (year_data$nstakes_annual > 0)
  
  
  #### [DISABLED] USER-DEFINED FIXED PERIOD - ANNUAL ####
  # mb_fixed_period_annual_lab <- paste(run_params$massbal_fixed_annual_start, run_params$massbal_fixed_annual_end, sep = " - ")
  # mb_fixed_annual_lab <- sprintf(run_params$output_fmt1,year_data$massbal_annual_values[["fixed.mean"]] * run_params$output_mult / 1000.)
  # plot_df <- plot_df_base
  # plot_df$massbal <- values(year_data$massbal_annual_maps$fixed)[,1]
  # plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
  #   geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
  #   geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
  #   coord_sf(clip = "off") +
  #   geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize) +
  #   geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold") +
  #   annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
  #                                       x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
  #   annotation_custom(grobTree(textGrob(paste0("Fixed period (annual): ", mb_fixed_period_annual_lab),
  #                                       x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
  #   annotation_custom(grobTree(textGrob(bquote(bold(b[n]*" = "*.(mb_fixed_annual_lab)*" "*.(run_params$output_unit)*" w.e.")),
  #                                       x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
  #   labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
  #        subtitle = " ") +
  #   scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
  #                     limits = max_mb*c(-1,1),
  #                     breaks = run_params$mb_colorscale_breaks) +
  #   theme_map_massbal

  
  
  #### USER-DEFINED FIXED PERIOD - WINTER ####
  mb_fixed_period_winter_lab <- paste(run_params$massbal_fixed_winter_start, run_params$massbal_fixed_winter_end, sep = " - ")
  mb_fixed_winter_lab <- sprintf(run_params$output_fmt1,year_data$massbal_winter_values[["fixed.mean"]] * run_params$output_mult / 1000.)
  plot_df <- plot_df_base
  plot_df$massbal <- values(year_data$massbal_winter_maps$fixed)[,1]
  plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
    geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
    geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off") +
    {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
    {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Fixed period (winter): ", mb_fixed_period_winter_lab),
                                        x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold(b[w]*" = "*.(mb_fixed_winter_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                        x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                      limits = max_mb*c(-1,1),
                      breaks = run_params$mb_colorscale_breaks) +
    theme_map_massbal

  
  if (year_data$process_winter) {
    #### MEASUREMENT PERIOD - WINTER ####
    mb_meas_period_winter_lab <- paste(format(year_data$massbal_winter_meas_period, "%m/%d"), collapse = " - ")
    mb_meas_winter_lab <- sprintf(run_params$output_fmt1,year_data$massbal_winter_values[["meas_period.mean"]] * run_params$output_mult / 1000.)
    plot_df <- plot_df_base
    plot_df$massbal <- values(year_data$massbal_winter_maps$meas_period)[,1]
    plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
      {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Measurement period (winter): ", mb_meas_period_winter_lab),
                                          x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold(b[w]*" = "*.(mb_meas_winter_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    
    
    
    #### WIP DEV: MEASUREMENT PERIOD - WINTER, WITH WINTER STAKES ####
    plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
      geom_point(data = year_data$massbal_winter_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
      {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
      {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_winter_meas_cur, aes(x = x, y = y, label = sprintf(run_params$output_fmt2, massbal*run_params$output_mult/1e3)), size = 3*extent_size_multiplier, fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05,  y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Measurement period (winter): ", mb_meas_period_winter_lab),
                                          x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold(b[w]*" = "*.(mb_meas_winter_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    
  }
  
  return(plots)
  
}
