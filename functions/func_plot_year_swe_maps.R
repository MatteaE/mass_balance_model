###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which plots modeled SWE maps at key moments.     #
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

# Compared to func_plot_year_mb_maps, this one plots the full DHM extent,
# because SWE can be exchanged with the glacier surroundings (avalanches).

func_plot_year_swe_maps <- function(year_data,
                                    run_params,
                                    data_dhms,
                                    data_dems,
                                    data_outlines,
                                    plots_map_common_elements) {
  
  base_size <- 16 # For the plots.
  
  # Empirical top margin to keep plots inside page borders
  # when the glacier is tall (aspect ratio > 1.07).
  margin_top <- min(80, max(0, (plots_map_common_elements$dhm_grid_aspect_ratio - 1.05) * 1200))
  theme_map_swe <- theme_void(base_size = base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(0.25, "cm"),
          legend.box.margin = margin(0,0,5,0),
          legend.title = element_text(vjust = 0, face = "bold", size = 16, margin = margin(0,20,0,0,unit = "pt")),
          legend.text = element_text(face = "bold", size = 12),
          plot.margin = margin(margin_top,0,0,0, unit = "pt"))
  
  # palette_RdPu_adj <- c(RColorBrewer::brewer.pal(9, "RdPu")[c(2:8)], "#310063")
  palette_swe <- c("#CDFFCC", "#99F1B3", "#53BCA0", "#3296B3", "#0770AE", "#00358F", "#D30688", "#FF00FF")
  # palette_cur <- palette_RdPu_adj
  palette_cur <- palette_swe
  
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  y_line_mult <- min(1.5, max(1, (plots_map_common_elements$dhm_grid_aspect_ratio + 1.5) / 2))
  y_line1 <- 1 + (0.21 / y_line_mult)
  y_line2 <- 1 + (0.12 / y_line_mult)
  y_line3 <- 1 + (0.06 / y_line_mult)
  y_line4 <- 1 + (0.00 / y_line_mult)
  
  
  # Values exceeding +/- max_swe will be clamped.
  swe_positive_ids <- which(year_data$mod_output_annual_cur$vec_swe_all > 0)
  if (length(swe_positive_ids) > 0) {
    max_swe <- max(400, round(quantile(year_data$mod_output_annual_cur$vec_swe_all[swe_positive_ids], 0.98) / 400) * 400) * run_params$output_mult/1000
    # No snow ever? Unlikely, but set a default value for max_swe.
  } else {
    max_swe <- 400 * run_params$output_mult/1000
  }
  swe_breaks <- c(0.000, 0.025, 0.050, 0.125, 0.250, 0.375, 0.500, 0.750, 1.000)*max_swe
  swe_labels <- sprintf(run_params$output_fmt2, swe_breaks)
  swe_labels[length(swe_labels)] <- ""
  
  xlim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[1:2]
  ylim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[3:4]
  
  plot_df <- plots_map_common_elements$dhm_plot_df_base # This gets reused in subsequent SWE plots.
  
  plots <- list()

    
  #### HYDROLOGICAL YEAR START ####
  plot_df$swe <- values(year_data$swe_annual_maps$hydro_start, mat = F)
  swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]], na.rm = T) * run_params$output_mult / 1000.)
  plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
  plots[[length(plots)+1]] <- ggplot(plot_df) +
    geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Hydrological year start: ", year_data$year_cur-1, "/", run_params$hydro_start_mmdd),
                                        x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                        x = 0.05, y = y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name = paste0("SWE [", run_params$output_unit, " w.e.]\n"),
                      colors = palette_cur,
                      limits = c(0,max_swe),
                      breaks = swe_breaks,
                      labels = swe_labels,
                      oob = scales::oob_squish,
                      values = swe_breaks/max(swe_breaks),
                      na.value = "#FFFFFF00") +
    theme_map_swe
  
  
  
  #### HYDROLOGICAL YEAR END ####
  plot_df$swe <- values(year_data$swe_annual_maps$hydro_end, mat = F)
  swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
  plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
  plots[[length(plots)+1]] <- ggplot(plot_df[which(plot_df$swe > 0),]) +
    geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Hydrological year end: ", year_data$year_cur, "/", run_params$hydro_end_mmdd),
                                        x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                        x = 0.05, y = y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name = paste0("SWE [", run_params$output_unit, " w.e.]\n"),
                      colors = palette_cur,
                      limits = c(0,max_swe),
                      breaks = swe_breaks,
                      labels = swe_labels,
                      oob = scales::oob_squish,
                      values = swe_breaks/max(swe_breaks),
                      na.value = "#FFFFFF00") +
    theme_map_swe
  
  
  
  
  
  if (year_data$nstakes_annual > 0) {
    
    #### ANNUAL MEASUREMENT PERIOD START ####
    meas_period_annual_start_lab <- format(year_data$massbal_annual_meas_period[1], "%Y/%m/%d")
    plot_df$swe <- values(year_data$swe_annual_maps$meas_period_start, mat = F)
    swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
    plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
    plots[[length(plots)+1]] <- ggplot(plot_df[which(plot_df$swe > 0),]) +
      geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off",
               xlim = xlim,
               ylim = ylim) +
      {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
      {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Annual measurement period start: ", meas_period_annual_start_lab),
                                          x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SWE [", run_params$output_unit, " w.e.]\n"),
                        colors = palette_cur,
                        limits = c(0,max_swe),
                        breaks = swe_breaks,
                        labels = swe_labels,
                        oob = scales::oob_squish,
                        values = swe_breaks/max(swe_breaks),
                        na.value = "#FFFFFF00") +
      theme_map_swe
    
    
    #### ANNUAL MEASUREMENT PERIOD END ####
    meas_period_annual_end_lab <- format(year_data$massbal_annual_meas_period[2], "%Y/%m/%d")
    plot_df$swe <- values(year_data$swe_annual_maps$meas_period_end, mat = F)
    swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
    plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
    plots[[length(plots)+1]] <- ggplot(plot_df[which(plot_df$swe > 0),]) +
      geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off",
               xlim = xlim,
               ylim = ylim) +
      {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
      {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
      annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                          x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(paste0("Annual measurement period end: ", meas_period_annual_end_lab),
                                          x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
      annotation_custom(grobTree(textGrob(bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                          x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
      labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
           subtitle = " ") +
      scale_fill_stepsn(name = paste0("SWE [", run_params$output_unit, " w.e.]\n"),
                        colors = palette_cur,
                        limits = c(0,max_swe),
                        breaks = swe_breaks,
                        labels = swe_labels,
                        oob = scales::oob_squish,
                        values = swe_breaks/max(swe_breaks),
                        na.value = "#FFFFFF00") +
      theme_map_swe
  }
  
  
  
  #### WINTER FIXED PERIOD END ####
  fixed_winter_end_lab <- format(year_data$massbal_winter_fixed_period[2], "%Y/%m/%d")
  plot_df$swe <- values(year_data$swe_winter_maps$fixed_end, mat = F)
  swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
  plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
  plots[[length(plots)+1]] <- ggplot(plot_df[which(plot_df$swe > 0),]) +
    geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult / 1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    annotation_custom(grobTree(textGrob(paste0(year_data$year_cur-1, "/", year_data$year_cur),
                                        x=0.05, y=y_line1, hjust=0, gp = gpar(fontsize = 2 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(paste0("Winter fixed period end: ", fixed_winter_end_lab),
                                        x=0.05, y=y_line2, hjust=0, gp = gpar(fontsize = 1 * base_size, fontface = "bold")))) +
    annotation_custom(grobTree(textGrob(bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e.")),
                                        x = 0.05, y=y_line3, hjust = 0, gp = gpar(fontsize = 1 * base_size)))) +
    labs(title    = " ", # Empty title to preserve spacing. We add the real title just above, with annotation_custom().
         subtitle = " ") +
    scale_fill_stepsn(name = paste0("SWE [", run_params$output_unit, " w.e.]\n"),
                      colors = palette_cur,
                      limits = c(0,max_swe),
                      breaks = swe_breaks,
                      labels = swe_labels,
                      oob = scales::oob_squish,
                      values = swe_breaks/max(swe_breaks),
                      na.value = "#FFFFFF00") +
    theme_map_swe
  
  
  
  return(plots)
  
}
