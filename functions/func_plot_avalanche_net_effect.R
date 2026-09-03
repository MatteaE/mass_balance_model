###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which plots the net effect of avalanches over    #
#                 the year.                                                                       #
################################################################################################### 


func_plot_avalanche_net_effect <- function(year_data,
                                           run_params,
                                           data_dhms,
                                           data_dems,
                                           data_outlines,
                                           plots_map_common_elements) {
  
  
  base_size <- 16 # For the plots.
  
  # Empirical top margin to keep plots inside page borders
  # when the glacier is tall (aspect ratio > 1.07).
  margin_top <- min(80, max(0, (plots_map_common_elements$dhm_grid_aspect_ratio - 1.05) * 1200))
  theme_map_avalanches <- theme_void(base_size = base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(0.25, "cm"),
          legend.box.margin = margin(0,0,5,0),
          legend.title = element_text(vjust = 0, face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 12),
          plot.margin = margin(margin_top,0,0,0, unit = "pt"))
  
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  y_line_mult <- min(1.5, max(1, (plots_map_common_elements$dhm_grid_aspect_ratio + 1.5) / 2))
  y_line1 <- 1 + (0.21 / y_line_mult)
  y_line2 <- 1 + (0.12 / y_line_mult)
  y_line3 <- 1 + (0.06 / y_line_mult)
  y_line4 <- 1 + (0.00 / y_line_mult)
  
  palette_RdBu_ext <- c("#33000F", RColorBrewer::brewer.pal(11, "RdBu")[c(1:4,6,8:11)], "#011830")
  # Values exceeding +/- max_mb will be clamped.
  # We need set this so that the colors are well distributed
  # in the scale (else they are too dark or washed out).
  max_mb <- abs(2*run_params$mb_colorscale_breaks[1] - run_params$mb_colorscale_breaks[2])
  
  xlim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[1:2]
  ylim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[3:4]
  
  plot_df <- plots_map_common_elements$dhm_plot_df_base
  
  plots <- list()


  #### TOTAL EFFECT ON GLACIER ####
  plot_df$avalanche_effect <- year_data$mod_output_annual_cur$avalanche_net
  avalanche_onglacier_lab <- sprintf(run_params$output_fmt3, mean(plot_df$avalanche_effect[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
  if (length(run_params$model_avalanche_dates) == 0) {
    label_avalanche_dates <- "No avalanches defined"
  } else {
    label_avalanche_dates <- paste0("Avalanche dates: ", paste0(run_params$model_avalanche_dates, collapse = " - "))
  }
  # We only plot those cells whose net effect is nonzero.
  plot_df$avalanche_effect[which(abs(plot_df$avalanche_effect) < run_params$avalanche_effect_threshold)] <- NA
  plots[[length(plots)+1]] <- ggplot(plot_df) +
    geom_raster(aes(x = x, y = y, fill = avalanche_effect * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    {if (year_data$nstakes_annual > 0) geom_point(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0)} +
    {if (year_data$nstakes_annual > 0) geom_shadowtext(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y, label = id), size = 3*plots_map_common_elements$dhm_extent_size_multiplier, fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(label_avalanche_dates,
                                        x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold("Total net effect on glacier"*" = "*.(avalanche_onglacier_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                        x = 0.05, y = y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name = paste0("Net avalanche\neffect [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                      limits = max_mb*c(-1,1),
                      breaks = run_params$mb_colorscale_breaks,
                      na.value = "#FFFFFF00") +
    theme_map_avalanches
  
  
  return(plots)
  
}
