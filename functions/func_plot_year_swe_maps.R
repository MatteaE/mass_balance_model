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
  
  colorbar_width <- 2.8
  if (run_params$output_unit == "m") {
    margin_box_right <- 7
    margin_title_right <- 28
  } else {
    margin_box_right <- 14
    margin_title_right <- 21
  }
  theme_map_swe <- theme_void(base_size = plots_map_common_elements$base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(colorbar_width*plots_map_common_elements$base_size/16, "cm"),
          legend.key.height = unit(0.25*plots_map_common_elements$base_size/16, "cm"),
          legend.box.margin = margin(-40,margin_box_right,5,0)*plots_map_common_elements$base_size/16,
          legend.title = element_text(vjust = 0.5, face = "bold", size = plots_map_common_elements$base_size,
                                      margin = margin(-14,margin_title_right,7,7,"pt")*plots_map_common_elements$base_size/16),
          legend.text = element_text(face = "bold", size = plots_map_common_elements$base_size*0.75),
          plot.margin = margin(0,0,0,0, unit = "pt"))
  
  # palette_RdPu_adj <- c(RColorBrewer::brewer.pal(9, "RdPu")[c(2:8)], "#310063")
  palette_swe <- c("#CDFFCC", "#99F1B3", "#53BCA0", "#3296B3", "#0770AE", "#00358F", "#D30688", "#FF00FF")
  # palette_cur <- palette_RdPu_adj
  palette_cur <- palette_swe
  
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  
  
  # Values exceeding +/- max_swe will be clamped.
  swe_positive_ids <- which(year_data$mod_output_annual_cur$vec_swe_all > 0)
  if (length(swe_positive_ids) > 0) {
    max_swe <- quantile(year_data$mod_output_annual_cur$vec_swe_all[swe_positive_ids], 0.98)
    
    # No snow ever? Unlikely, but set a default value for max_swe.
  } else {
    max_swe <- 400
  }
  
  mult_allowed <- c(0.4, 0.8, 1.2, 1.6, 2.0, 2.4, 3.2, 4.0, 4.8, 6, 8, 10, 12, 16, 20, 24, 30) * 1000 # Still in mm w.e.
  max_swe <- mult_allowed[which.min(abs(max_swe - mult_allowed))] * run_params$output_mult/1000
  
  swe_breaks <- c(0.000, 0.025, 0.050, 0.125, 0.250, 0.375, 0.500, 0.750, 1.000)*max_swe
  swe_labels <- sprintf(run_params$output_fmt2, swe_breaks)
  swe_labels[length(swe_labels)] <- ""
  
  xlim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[1:2]
  ylim <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])[3:4]
  
  plot_df <- plots_map_common_elements$dhm_plot_df_base # This gets reused in subsequent SWE plots.
  
  plot_pages  <- list()
  
  
  #### HYDROLOGICAL YEAR START ####
  plot_df$swe <- values(year_data$swe_annual_maps$hydro_start, mat = F)
  swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]], na.rm = T) * run_params$output_mult / 1000.)
  plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
  pl_cur <- ggplot(plot_df) +
    geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    scale_fill_stepsn(name = paste0("\n\n\nSWE [", run_params$output_unit, " w.e.]\n\n\n"),
                      colors = palette_cur,
                      limits = c(0,max_swe),
                      breaks = swe_breaks,
                      labels = swe_labels,
                      oob = scales::oob_squish,
                      values = swe_breaks/max(swe_breaks),
                      na.value = "#FFFFFF00") +
    theme_map_swe
  
  
  title_cur <- func_make_map_title(
    list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
         paste0("Hydrological year start: ", year_data$year_cur-1, "/", run_params$hydro_start_mmdd),
         bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e."))),
    base_size = plots_map_common_elements$base_size)
  
  
  plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, plots_map_common_elements$dhm_grid_aspect_ratio,
                                                                            title_h = 3.0, legend_h = 1.2))
  
  
  
  #### HYDROLOGICAL YEAR END ####
  plot_df$swe <- values(year_data$swe_annual_maps$hydro_end, mat = F)
  swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
  plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
  pl_cur <- ggplot(plot_df[which(plot_df$swe > 0),]) +
    geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    scale_fill_stepsn(name = paste0("\n\n\nSWE [", run_params$output_unit, " w.e.]\n\n\n"),
                      colors = palette_cur,
                      limits = c(0,max_swe),
                      breaks = swe_breaks,
                      labels = swe_labels,
                      oob = scales::oob_squish,
                      values = swe_breaks/max(swe_breaks),
                      na.value = "#FFFFFF00") +
    theme_map_swe
  
  
  title_cur <- func_make_map_title(
    list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
         paste0("Hydrological year end: ", year_data$year_cur, "/", run_params$hydro_end_mmdd),
         bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e."))),
    base_size = plots_map_common_elements$base_size)
  
  
  plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, plots_map_common_elements$dhm_grid_aspect_ratio,
                                                                            title_h = 3.0, legend_h = 1.2))
  
  
  
  if (year_data$nstakes_annual > 0) {
    
    #### ANNUAL MEASUREMENT PERIOD START ####
    meas_period_annual_start_lab <- format(year_data$massbal_annual_meas_period[1], "%Y/%m/%d")
    plot_df$swe <- values(year_data$swe_annual_maps$meas_period_start, mat = F)
    swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
    plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
    pl_cur <- ggplot(plot_df[which(plot_df$swe > 0),]) +
      geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off",
               xlim = xlim,
               ylim = ylim) +
      {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
      {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
      scale_fill_stepsn(name = paste0("\n\n\nSWE [", run_params$output_unit, " w.e.]\n\n\n"),
                        colors = palette_cur,
                        limits = c(0,max_swe),
                        breaks = swe_breaks,
                        labels = swe_labels,
                        oob = scales::oob_squish,
                        values = swe_breaks/max(swe_breaks),
                        na.value = "#FFFFFF00") +
      theme_map_swe
    
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Annual measurement period start: ", meas_period_annual_start_lab),
           bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e."))),
      base_size = plots_map_common_elements$base_size)
    
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, plots_map_common_elements$dhm_grid_aspect_ratio,
                                                                              title_h = 3.0, legend_h = 1.2))
    
    
    
    #### ANNUAL MEASUREMENT PERIOD END ####
    meas_period_annual_end_lab <- format(year_data$massbal_annual_meas_period[2], "%Y/%m/%d")
    plot_df$swe <- values(year_data$swe_annual_maps$meas_period_end, mat = F)
    swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
    plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
    pl_cur <- ggplot(plot_df[which(plot_df$swe > 0),]) +
      geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off",
               xlim = xlim,
               ylim = ylim) +
      {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
      {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
      scale_fill_stepsn(name = paste0("\n\n\nSWE [", run_params$output_unit, " w.e.]\n\n\n"),
                        colors = palette_cur,
                        limits = c(0,max_swe),
                        breaks = swe_breaks,
                        labels = swe_labels,
                        oob = scales::oob_squish,
                        values = swe_breaks/max(swe_breaks),
                        na.value = "#FFFFFF00") +
      theme_map_swe
    
    
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Annual measurement period end: ", meas_period_annual_end_lab),
           bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e."))),
      base_size = plots_map_common_elements$base_size)
    
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, plots_map_common_elements$dhm_grid_aspect_ratio,
                                                                              title_h = 3.0, legend_h = 1.2))
    
    
  } # End if there are annual measurements
  
  
  
  #### WINTER FIXED PERIOD END ####
  fixed_winter_end_lab <- format(year_data$massbal_winter_fixed_period[2], "%Y/%m/%d")
  plot_df$swe <- values(year_data$swe_winter_maps$fixed_end, mat = F)
  swe_lab <- sprintf(run_params$output_fmt1, mean(plot_df$swe[data_dems$glacier_cell_ids[[year_data$dem_grid_id]]]) * run_params$output_mult / 1000.)
  plot_df$swe[which(plot_df$swe == 0)] <- NA_real_
  pl_cur <- ggplot(plot_df[which(plot_df$swe > 0),]) +
    geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult / 1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off",
             xlim = xlim,
             ylim = ylim) +
    {if (run_params$show_contours) plots_map_common_elements$dhm_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dhm_ele_text_contours} +
    scale_fill_stepsn(name = paste0("\n\n\nSWE [", run_params$output_unit, " w.e.]\n\n\n"),
                      colors = palette_cur,
                      limits = c(0,max_swe),
                      breaks = swe_breaks,
                      labels = swe_labels,
                      oob = scales::oob_squish,
                      values = swe_breaks/max(swe_breaks),
                      na.value = "#FFFFFF00") +
    theme_map_swe
  
  
  title_cur <- func_make_map_title(
    list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
         paste0("Winter fixed period end: ", fixed_winter_end_lab),
         bquote(bold("Mean on-glacier SWE"*" = "*.(swe_lab)*" "*.(run_params$output_unit)*" w.e."))),
    base_size = plots_map_common_elements$base_size)
  
  
  plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, plots_map_common_elements$dhm_grid_aspect_ratio,
                                                                            title_h = 3.0, legend_h = 1.2))
  
  
  
  return(plot_pages)
  
}
