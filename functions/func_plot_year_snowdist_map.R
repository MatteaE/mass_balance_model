###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which plots the map of snow distribution.        #
################################################################################################### 


func_plot_year_snowdist_map <- function(year_data,
                                        run_params,
                                        data_dhms,
                                        data_outlines,
                                        plots_map_common_elements) {
  
  
  base_size <- 16 # For the plots.
  
  # Empirical top margin to keep plots inside page borders
  # when the glacier is tall (aspect ratio > 1.07).
  margin_top <- min(80, max(0, (plots_map_common_elements$dhm_grid_aspect_ratio - 1.05) * 1200))
  theme_map_mult <- theme_void(base_size = base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(0.25, "cm"),
          legend.box.margin = margin(0,0,5,0),
          legend.title = element_text(vjust = 0, face = "bold", size = 16, margin = margin(0,20,0,0,unit = "pt")),
          legend.text = element_text(face = "bold", size = 12),
          plot.margin = margin(margin_top,0,0,0, unit = "pt"))
  
  # palette_RdPu_adj <- c(RColorBrewer::brewer.pal(9, "RdPu")[c(2:8)], "#310063")
  palette_cur <- RColorBrewer::brewer.pal(10, "BrBG")
  
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  y_line_mult <- min(1.5, max(1, (plots_map_common_elements$dhm_grid_aspect_ratio + 1.5) / 2))
  y_line1 <- 1 + (0.21 / y_line_mult)
  y_line2 <- 1 + (0.12 / y_line_mult)
  y_line3 <- 1 + (0.06 / y_line_mult)
  y_line4 <- 1 + (0.00 / y_line_mult)
  
  dist_final_values <- year_data$dist_topographic_values_red * year_data$dist_probes_norm_values_red
  
  val_breaks <- c(0.0, 0.5, 0.75, 0.9, 0.95, 1.0, 1.05, 1.10, 1.25, 1.5, 2.0)*100
  val_labels <- as.character(val_breaks)
  val_labels[length(val_labels)] <- ""
  
  xlim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[1:2]
  ylim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[3:4]
  
  plots <- list()
  
  #### COMBINED TOPOGRAPHY and PROBES ####
  plot_df <- plots_map_common_elements$dhm_plot_df_base
  plot_df$snowdist_percent <- dist_final_values*100
  
  plots[[length(plots)+1]] <- ggplot(plot_df) +
    geom_raster(aes(x = x, y = y, fill = snowdist_percent)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Snow distribution multiplier"),
                                        x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold("Topographic factor"*" = "*.(sprintf("%.2f", run_params$topographic_snowdist_fact))*", probes factor"*" = "*.(sprintf("%.2f",run_params$probes_snowdist_fact)))),
                                        x = 0.05, y = y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name = paste0("Multiplier [%]\n"),
                      colors = palette_cur,
                      limits = c(0,200),
                      breaks = val_breaks,
                      labels = val_labels,
                      oob = scales::oob_squish,
                      values = val_breaks/max(val_breaks)) +
    theme_map_mult
  
  return(plots)
  
}
