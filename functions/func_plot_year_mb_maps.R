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
                                   data_outlines,
                                   plots_map_common_elements) {
  
  
  if (run_params$output_unit == "m") {
    margin_title_right <- 21
    colorbar_width <- 2.8
    textsize_mult <- 1
  } else {
    margin_title_right <- 7
    colorbar_width <- 3
    textsize_mult <- 0.8
  }
  
  theme_map_massbal <- theme_void(base_size = plots_map_common_elements$base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(colorbar_width*plots_map_common_elements$base_size/16, "cm"),
          legend.key.height = unit(0.25*plots_map_common_elements$base_size/16, "cm"),
          legend.box.margin = margin(0,0,0,0),
          legend.title = element_text(vjust = 1, face = "bold", size = plots_map_common_elements$base_size,
                                      margin = margin(0,margin_title_right,0,7,"pt")*plots_map_common_elements$base_size/16),
          legend.text = element_text(face = "bold", size = plots_map_common_elements$base_size*0.75*textsize_mult),
          plot.margin = margin(0,0,0,0, unit = "pt"))
  
  outline_linesize <- 0.7 * run_params$outlines_linesize_mult
  
  palette_RdBu_ext <- c("#33000F", RColorBrewer::brewer.pal(11, "RdBu")[c(1:4,6,8:11)], "#011830")
  # Values exceeding +/- max_mb will be clamped.
  # We need set this so that the colors are well distributed
  # in the scale (else they are too dark or washed out).
  max_mb <- abs(2*run_params$mb_colorscale_breaks[1] - run_params$mb_colorscale_breaks[2])
  
  plot_pages  <- list()
  
  # This one will be recycled for each plot.
  plot_df <- plots_map_common_elements$dem_plot_df_base
  
  
  #### HYDROLOGICAL YEAR ####
  mb_hydro_lab <- sprintf(run_params$output_fmt1, year_data$massbal_annual_values[["hydro.mean"]] * run_params$output_mult / 1000.)
  plot_df$massbal <- values(year_data$massbal_annual_maps$hydro, mat = F)
  pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
    geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off") +
    {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
    scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                      limits = max_mb*c(-1,1),
                      breaks = run_params$mb_colorscale_breaks) +
    theme_map_massbal
  
  
  title_cur <- func_make_map_title(
    list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
         paste0("Hydrological year: ", run_params$hydro_start_mmdd, " - ", run_params$hydro_end_mmdd),
         bquote(bold(b[n]*" = "*.(mb_hydro_lab)*" "*.(run_params$output_unit)*" w.e."))),
    base_size = plots_map_common_elements$base_size)
  
  
  plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
  
  
  
  #### MEASUREMENT PERIOD - ANNUAL ####
  if (year_data$nstakes_annual > 0) {
    
    mb_meas_period_annual_lab <- paste(format(year_data$massbal_annual_meas_period, "%m/%d"), collapse = " - ")
    mb_meas_annual_lab <- sprintf(run_params$output_fmt1,year_data$massbal_annual_values[["meas_period.mean"]] * run_params$output_mult / 1000.)
    plot_df$massbal <- values(year_data$massbal_annual_maps$meas_period, mat = F) # Directly recycle plot_df from before, overwriting its massbal.
    pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
      {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Measurement period (annual): ", mb_meas_period_annual_lab),
           bquote(bold(b[n]*" = "*.(mb_meas_annual_lab)*" "*.(run_params$output_unit)*" w.e."))),
      base_size = plots_map_common_elements$base_size)
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
    
    
    
    #### MEASUREMENT PERIOD - ANNUAL, WITH STAKES ####
    # Also RMS, with LOO RMS in addition if available and unweighted
    # (arithmetic) RMS if the main one is area-weighted
    if (year_data$annual_bias_weighted_logi) {
      if (!is.null(year_data$global_loo_rms)) { # Weighted, LOO and unweighted RMS
        rms_txt <- paste0("RMS (", run_params$output_unit, " w.e.): ",
                          sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$weighted_rms*run_params$output_mult/1e3),
                          " (LOO: ", sprintf(run_params$output_fmt1, year_data$weighted_loo_rms*run_params$output_mult/1e3), " -",
                          " Unweighted: ", sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$global_rms*run_params$output_mult/1e3), ")")
      } else { # Weighted and unweighted RMS
        rms_txt <- paste0("RMS (", run_params$output_unit, " w.e.): ",
                          sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$weighted_rms*run_params$output_mult/1e3),
                          " (Unweighted: ", sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$global_rms*run_params$output_mult/1e3), ")")
      }
    } else { # Unweighted and LOO RMS (in this case, weighted and unweighted are the same because weights of 1.0 are used for all stakes).
      if (!is.null(year_data$global_loo_rms)) {
        rms_txt <- paste0("RMS (", run_params$output_unit, " w.e.): ",
                          sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$global_rms*run_params$output_mult/1e3),
                          " (LOO: ", sprintf(run_params$output_fmt1, year_data$global_loo_rms*run_params$output_mult/1e3), ")")
      } else { # Unweighted RMS only
        rms_txt <- paste0("RMS: ",
                          sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$global_rms*run_params$output_mult/1e3), " ", run_params$output_unit, " w.e.")
      }
    }
    
    
    pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
      geom_point(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
      {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
      {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_annual_meas_cur,
                                                         aes(x = x, y = y,
                                                             label = sprintf(run_params$output_fmt2,
                                                                             massbal_meas_standardized*run_params$output_mult/1e3)),
                                                         size = 3*plots_map_common_elements$dem_extent_size_multiplier,
                                                         fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Measurement period (annual): ", mb_meas_period_annual_lab),
           bquote(bold(b[n]*" = "*.(mb_meas_annual_lab)*" "*.(run_params$output_unit)*" w.e.")),
           rms_txt),
      base_size = plots_map_common_elements$base_size)
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
    
    
    
    #### MEASUREMENT PERIOD - ANNUAL, CORRECTED WITH CONTOUR LINE METHOD ####
    mb_meas_corr_annual_lab <- sprintf(run_params$output_fmt1,year_data$massbal_annual_values[["meas_period_corr.mean"]] * run_params$output_mult / 1000.)
    plot_df$massbal <- values(year_data$massbal_annual_maps$meas_period_corr, mat = F)
    pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
      {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Measurement period (annual, corrected): ", mb_meas_period_annual_lab),
           bquote(bold(b[n]*" = "*.(mb_meas_corr_annual_lab)*" "*.(run_params$output_unit)*" w.e."))),
      base_size = plots_map_common_elements$base_size)
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
    
    
    
    #### MEASUREMENT PERIOD - ANNUAL CORRECTED, WITH STAKES ####
    global_rmse_bandcorr   <- sqrt(mean((year_data$massbal_annual_meas_cur$massbal_meas_standardized - extract(year_data$massbal_annual_maps$meas_period_corr, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear")[,1])^2))
    weighted_rmse_bandcorr <- sqrt(mean(year_data$massbal_annual_meas_cur$area_weight*((year_data$massbal_annual_meas_cur$massbal_meas_standardized - extract(year_data$massbal_annual_maps$meas_period_corr, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear")[,1])^2)))
    if (year_data$annual_bias_weighted_logi) {
      rms_txt <- paste0("RMS (", run_params$output_unit, " w.e.): ",
                        sprintf(run_params$output_fmt1, weighted_rmse_bandcorr*run_params$output_mult/1e3),
                        " (Unweighted: ", sprintf(run_params$output_fmt1, global_rmse_bandcorr*run_params$output_mult/1e3), ")")
    } else {
      rms_txt <- paste0("RMS: ",
                        sprintf(run_params$output_fmt1, global_rmse_bandcorr*run_params$output_mult/1e3), " ", run_params$output_unit, " w.e.")
    }
    pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult / 1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
      geom_point(data = year_data$massbal_annual_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
      {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
      {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_annual_meas_cur,
                                                         aes(x = x, y = y,
                                                             label = sprintf(run_params$output_fmt2,
                                                                             massbal_meas_standardized*run_params$output_mult/1e3)),
                                                         size = 3*plots_map_common_elements$dem_extent_size_multiplier,
                                                         fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Measurement period (annual, corrected): ", mb_meas_period_annual_lab),
           bquote(bold(b[n]*" = "*.(mb_meas_corr_annual_lab)*" "*.(run_params$output_unit)*" w.e.")),
           rms_txt),
      base_size = plots_map_common_elements$base_size)
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
    
    
  } # End of if (year_data$nstakes_annual > 0)
  
  
  
  #### USER-DEFINED FIXED PERIOD - WINTER ####
  mb_fixed_period_winter_lab <- paste(run_params$massbal_fixed_winter_start, run_params$massbal_fixed_winter_end, sep = " - ")
  mb_fixed_winter_lab <- sprintf(run_params$output_fmt1,year_data$massbal_winter_values[["fixed.mean"]] * run_params$output_mult / 1000.)
  plot_df$massbal <- values(year_data$massbal_winter_maps$fixed, mat = F)
  pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
    geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
    geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
    coord_sf(clip = "off") +
    {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
    {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
    scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                      limits = max_mb*c(-1,1),
                      breaks = run_params$mb_colorscale_breaks) +
    theme_map_massbal
  
  
  title_cur <- func_make_map_title(
    list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
         paste0("Fixed period (winter): ", mb_fixed_period_winter_lab),
         bquote(bold(b[w]*" = "*.(mb_fixed_winter_lab)*" "*.(run_params$output_unit)*" w.e."))),
    base_size = plots_map_common_elements$base_size)
  
  plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
  
  
  
  if (year_data$process_winter) {
    #### MEASUREMENT PERIOD - WINTER ####
    mb_meas_period_winter_lab <- paste(format(year_data$massbal_winter_meas_period, "%m/%d"), collapse = " - ")
    mb_meas_winter_lab <- sprintf(run_params$output_fmt1,year_data$massbal_winter_values[["meas_period.mean"]] * run_params$output_mult / 1000.)
    plot_df$massbal <- values(year_data$massbal_winter_maps$meas_period, mat = F)
    pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
      {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Measurement period (winter): ", mb_meas_period_winter_lab),
           bquote(bold(b[w]*" = "*.(mb_meas_winter_lab)*" "*.(run_params$output_unit)*" w.e."))),
      base_size = plots_map_common_elements$base_size)
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
    
    
    
    #### MEASUREMENT PERIOD - WINTER, WITH WINTER STAKES ####
    if (year_data$winter_bias_weighted_logi) {
      rms_txt <- paste0("RMS (", run_params$output_unit, " w.e.): ",
                        sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$weighted_rms_winter*run_params$output_mult/1e3),
                        " (Unweighted: ", sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$global_rms_winter*run_params$output_mult/1e3), ")")
    } else {
      rms_txt <- paste0("RMS: ",
                        sprintf(run_params$output_fmt1, year_data$mod_output_annual_cur$global_rms_winter*run_params$output_mult/1e3), " ", run_params$output_unit, " w.e.")
    }
    pl_cur <- ggplot(plot_df[data_dems$glacier_cell_ids[[year_data$dem_grid_id]],]) +
      geom_raster(aes(x = x, y = y, fill = massbal * run_params$output_mult/1000)) +
      geom_sf(data = plots_map_common_elements$outl_sf, fill = NA, color = "#202020", linewidth = outline_linesize) +
      coord_sf(clip = "off") +
      {if (run_params$show_contours) plots_map_common_elements$dem_ele_contours} +
      geom_point(data = year_data$massbal_winter_meas_cur, aes(x = x, y = y), shape = 3, stroke = 1.5, size = 0) +
      {if (run_params$show_contour_labels) plots_map_common_elements$dem_ele_text_contours} +
      {if (run_params$show_stake_labels) geom_shadowtext(data = year_data$massbal_winter_meas_cur,
                                                         aes(x = x, y = y,
                                                             label = sprintf(run_params$output_fmt2,
                                                                             massbal*run_params$output_mult/1e3)),
                                                         size = 3*plots_map_common_elements$dem_extent_size_multiplier,
                                                         fontface = "bold", color = "#000000", hjust = -0.12, vjust = -0.12, bg.color = "#FFFFFF")} +
      scale_fill_stepsn(name = paste0("SMB [", run_params$output_unit, " w.e.]"), colors = palette_RdBu_ext,
                        limits = max_mb*c(-1,1),
                        breaks = run_params$mb_colorscale_breaks) +
      theme_map_massbal
    
    title_cur <- func_make_map_title(
      list(paste0(year_data$year_cur-1, "/", year_data$year_cur),
           paste0("Measurement period (winter): ", mb_meas_period_winter_lab),
           bquote(bold(b[w]*" = "*.(mb_meas_winter_lab)*" "*.(run_params$output_unit)*" w.e.")),
           rms_txt),
      base_size = plots_map_common_elements$base_size)
    
    plot_pages[[length(plot_pages)+1]] <- suppressWarnings(func_make_map_page(title_cur, pl_cur, data_outlines$aspect_ratio[[year_data$outline_id]]))
    
  }
  
  return(plot_pages)
  
}
