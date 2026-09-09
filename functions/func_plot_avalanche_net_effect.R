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
  
  
  if (run_params$output_unit == "m") {
    colorbar_width <- 2.8
    textsize_mult <- 1
  } else {
    colorbar_width <- 3
    textsize_mult <- 0.8
  }
  
  theme_map_avalanches <- theme_void(base_size = plots_map_common_elements$base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(colorbar_width*plots_map_common_elements$base_size/16, "cm"),
          legend.key.height = unit(0.25*plots_map_common_elements$base_size/16, "cm"),
          legend.box.margin = margin(-40,0,5,0)*plots_map_common_elements$base_size/16,
          legend.title = element_text(vjust = 0.5, face = "bold", size = plots_map_common_elements$base_size,
                                      margin = margin(-14,14,7,7,"pt")*plots_map_common_elements$base_size/16),
          legend.text = element_text(face = "bold", size = plots_map_common_elements$base_size*0.75*textsize_mult),
          plot.margin = margin(0,0,0,0, unit = "pt"))
  
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  
  palette_RdBu_ext <- c("#33000F", RColorBrewer::brewer.pal(11, "RdBu")[c(1:4,6,8:11)], "#011830")
  # Values exceeding +/- max_mb will be clamped.
  # We need set this so that the colors are well distributed
  # in the scale (else they are too dark or washed out).
  max_mb <- abs(2*run_params$mb_colorscale_breaks[1] - run_params$mb_colorscale_breaks[2])
  
  xlim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[1:2]
  ylim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[3:4]
  
  plot_df <- plots_map_common_elements$dhm_plot_df_base
  
  plot_pages <- list()


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
  pl_cur <- ggplot(plot_df) +
    geom_raster(aes(x = x, y = y, fill = avalanche_effect * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    {if (year_data$nstakes_annual > 0) geom_point(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0)} +
    {if (year_data$nstakes_annual > 0) geom_shadowtext(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y, label = id), size = 3*plots_map_common_elements$dhm_extent_size_multiplier, fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
    scale_fill_stepsn(name = paste0("\n\n\nNet avalanche\neffect [", run_params$output_unit, " w.e.]\n\n"), colors = palette_RdBu_ext,
                      limits = max_mb*c(-1,1),
                      breaks = run_params$mb_colorscale_breaks,
                      na.value = "#FFFFFF00") +
    theme_map_avalanches
  
  
  title_cur <- func_make_map_title(
    list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
         label_avalanche_dates,
         bquote(bold("Total net effect on glacier"*" = "*.(avalanche_onglacier_lab)*" "*.(run_params$output_unit)*" w.e."))),
    base_size = plots_map_common_elements$base_size)
  
  
  plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, plots_map_common_elements$dhm_grid_aspect_ratio,
                                                                            title_h = 3.0, legend_h = 1.2))
  
  
  return(plot_pages)
  
}
