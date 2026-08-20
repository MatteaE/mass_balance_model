###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which computes and plots the map of snow         #
#                 cover duration.                                                                 #
################################################################################################### 

func_plot_snowcover_duration <- function(year_data,
                                         year_cur_params,
                                         run_params,
                                         data_dhms,
                                         data_dems,
                                         data_outlines) {
  
  
  base_size <- 16 # For the plots.
  grid_extent <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])
  grid_area   <- (grid_extent[2] - grid_extent[1]) * (grid_extent[4] - grid_extent[3])
  grid_aspect_ratio <- (grid_extent[4] - grid_extent[3]) / (grid_extent[2] - grid_extent[1])
  # Empirical multiplier to reduce label and line size when the modeled extent is very big.
  # Useful for huge glaciers and multi-glacier (e.g. catchment) simulations.
  extent_size_multiplier <- max(0.1, exp(-(max(0,(grid_area-5e6))^2)/5e17))
  
  # Empirical top margin to keep plots inside page borders
  # when the glacier is tall (aspect ratio > 1.07).
  margin_top <- min(80, max(0, (grid_aspect_ratio - 1.05) * 1200))
  theme_map_snowcover <- theme_void(base_size = base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(0.25, "cm"),
          legend.box.margin = margin(0,0,5,0),
          legend.title = element_text(vjust = 0, face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 12),
          plot.margin = margin(margin_top,0,0,0, unit = "pt"))
  
  contour_label_textsize <- 4
  contour_linesize <- 0.25
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  y_line_mult <- min(1.5, max(1, (grid_aspect_ratio + 1.5) / 2))
  y_line1 <- 1 + (0.21 / y_line_mult)
  y_line2 <- 1 + (0.12 / y_line_mult)
  y_line3 <- 1 + (0.06 / y_line_mult)
  y_line4 <- 1 + (0.00 / y_line_mult)
  
  
  # Prepare the actual data
  id_hydro_start      <- which(year_data$weather_series_annual_cur$timestamp == year_cur_params$hydro_start)
  id_hydro_end        <- which(year_data$weather_series_annual_cur$timestamp == (year_cur_params$hydro_end - 1)) # No +1 here, we want a 365 (or 366) days period.
  
  # 1 row per modeled day, 1 column per DHM cell
  mat_snowcover_logi_hydro <- matrix(year_data$mod_output_annual_cur$vec_surftype_all == 2,
                                     nrow = year_data$model_annual_days_n + 1,
                                     byrow = TRUE)[id_hydro_start:id_hydro_end,]
  snowcover_days_n_vec <- colSums(mat_snowcover_logi_hydro)
  
  
  # palette_RdPu_adj <- c(RColorBrewer::brewer.pal(9, "RdPu")[c(2:8)], "#310063")
  palette_cur <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#2024A4", "#A30688")
  val_breaks <- c(0,seq(245,365,15))
  val_labels <- as.character(val_breaks)
  val_labels[1] <- ""
  
  plot_df_base <- data.frame(crds(data_dhms$elevation[[year_data$dhm_grid_id]], na.rm = FALSE))
  elevation_df <- data.frame(plot_df_base, z = values(data_dhms$elevation[[year_data$dhm_grid_id]], mat = F))
  
  
  plots <- list()
  
  plot_df <- plot_df_base
  plot_df$snowcover_days <- snowcover_days_n_vec
  snowcover_onglacier_lab_mean <- sprintf("%.1f", mean(plot_df$snowcover_days[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]))
  snowcover_onglacier_lab_min  <- as.character(min(plot_df$snowcover_days[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]))
  
  
  plots[[length(plots)+1]] <- ggplot(plot_df) +
    geom_raster(aes(x = x, y = y, fill = snowcover_days)) +
    geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off") +
    {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = contour_linesize)} +
    {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*extent_size_multiplier, stroke.color = "#FFFFFF", size = contour_label_textsize*extent_size_multiplier, min.size = 15, fontface = "bold")} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Snow cover duration (hydrological year)"),
                                        x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold("On-glacier: mean = "*.(snowcover_onglacier_lab_mean)*" "*"days, min = "*.(snowcover_onglacier_lab_min)*" "*"days")),
                                        x = 0.05, y = y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name   = paste0("Snow cover\nduration [d]"),
                      colors = palette_cur,
                      limits = c(0,366),
                      breaks = val_breaks,
                      labels = val_labels,
                      oob    = scales::oob_squish,
                      values = val_breaks/max(val_breaks)) +
    theme_map_snowcover
  
  return(plots)
  
  
}
