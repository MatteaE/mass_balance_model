###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to make plots of winter and annual Voronoi      #
#                 cells of the mass balance points.                                               #
###################################################################################################


func_plot_voronoi <- function(year_data,
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
  theme_map_voronoi <- theme_void(base_size = base_size) +
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
  
  palette_cur <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#2024A4", "#A30688")
  
  val_breaks <- c(0.0,0.5,0.7,0.9,1.0,1.1,1.3,1.5,2.0,3.0)
  val_labels <- sprintf("%.1f", val_breaks)
  val_labels[1] <- ""
  
  plot_df_base <- data.frame(crds(data_dems$elevation[[year_data$dem_grid_id]], na.rm = FALSE))
  elevation_df <- data.frame(plot_df_base, z = values(data_dems$elevation[[year_data$dem_grid_id]], mat = F))
  
  plots <- list()
  
  
  # Annual point weights --------------------------------------------------------------------------
  cells_sf <- sf::st_as_sf(year_data$voronoi_annual_v)
  cells_sf$weight <- year_data$massbal_annual_meas_cur$area_weight
  
  
  mb_meas_period_annual_lab <- paste(format(year_data$massbal_annual_meas_period, "%m/%d"), collapse = " - ")
  
  plot_df <- plot_df_base
  plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
    geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
    geom_sf(data = cells_sf, aes(fill = weight), linewidth = outline_linesize) +
    coord_sf(clip = "off") +
    {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
    geom_point(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
    {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
    {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y, label = sprintf(run_params$output_fmt2, massbal_meas_standardized*run_params$output_mult/1e3)), size = 3*extent_size_multiplier, fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Measurement period (annual): ", mb_meas_period_annual_lab),
                                        x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Area-weighting factor: ", sprintf("%.2f", run_params$optim_annual_areaweight_fact)),
                                        x=0.05, y=y_line3, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name   = paste0("Point weight [-]"),
                      colors = palette_cur,
                      limits = c(0,max(val_breaks) + 0.00001), # This ensures that all steps are drawn.
                      breaks = val_breaks,
                      labels = val_labels,
                      oob    = scales::oob_squish,
                      values = val_breaks/max(val_breaks)) +
    theme_map_voronoi
  
  
  # Winter point weights --------------------------------------------------------------------------
  if (year_data$process_winter) {
    
    cells_sf <- sf::st_as_sf(year_data$voronoi_winter_v)
    cells_sf$weight <- year_data$massbal_winter_meas_cur$area_weight
    
    
    mb_meas_period_winter_lab <- paste(format(year_data$massbal_winter_meas_period, "%m/%d"), collapse = " - ")
    
    plot_df <- plot_df_base
    plots[[length(plots)+1]] <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
      geom_sf(data = cells_sf, aes(fill = weight), linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
      geom_point(data = year_data$massbal_winter_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
      {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
      {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_winter_meas_cur, aes(x = x, y = y, label = sprintf(run_params$output_fmt2, massbal_meas_standardized*run_params$output_mult/1e3)), size = 3*extent_size_multiplier, fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Measurement period (winter): ", mb_meas_period_winter_lab),
                                          x=0.05,  y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Area-weighting factor: ", sprintf("%.2f", run_params$optim_winter_areaweight_fact)),
                                          x=0.05, y=y_line3, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name   = paste0("Point weight [-]"),
                        colors = palette_cur,
                        limits = c(0,max(val_breaks) + 0.00001), # This ensures that all steps are drawn.
                        breaks = val_breaks,
                        labels = val_labels,
                        oob    = scales::oob_squish,
                        values = val_breaks/max(val_breaks)) +
      theme_map_voronoi
    
  }
  
  
  return(plots)
  
}
