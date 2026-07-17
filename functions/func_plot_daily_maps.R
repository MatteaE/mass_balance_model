###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to produce daily plots of SWE and surface type.     #
#                 These can be directly turned into a nice animation.                             #
###################################################################################################


func_plot_daily_maps <- function(year_data,
                                 run_params,
                                 data_dhms,
                                 data_dems,
                                 data_outlines) {
  
  
  
  # dir.create(file.path(run_params$out_daily_dirpath, "gridded", year_data$year_cur, "massbal_plots"), recursive = TRUE)
  dir.create(file.path(run_params$out_daily_dirpath, "gridded", year_data$year_cur, "swe_plots"), recursive = TRUE)
  dir.create(file.path(run_params$out_daily_dirpath, "gridded", year_data$year_cur, "surftype_plots"), recursive = TRUE)

  base_size <- 12 # For the plots.
  grid_extent <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])
  grid_area   <- (grid_extent[2] - grid_extent[1]) * (grid_extent[4] - grid_extent[3])
  grid_aspect_ratio <- (grid_extent[4] - grid_extent[3]) / (grid_extent[2] - grid_extent[1])
  # Empirical multiplier to reduce label and line size when the modeled extent is very big.
  # Useful for huge glaciers and multi-glacier (e.g. catchment) simulations.
  extent_size_multiplier <- max(0.1, exp(-(max(0,(grid_area-5e6))^2)/5e17))
  
  # Empirical top margin to keep plots inside page borders
  # when the glacier is tall (aspect ratio > 1.07).
  margin_top <- min(80, max(0, (grid_aspect_ratio - 1.05) * 1200))
  theme_map_swe <- theme_void(base_size = base_size) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1.17, "cm"),
          legend.key.height = unit(0.15, "cm"),
          legend.box.margin = margin(0,0,5,0),
          legend.title = element_text(vjust = 1.1, face = "bold", size = base_size*0.7, margin = margin(0,10,0,0,unit = "pt")),
          legend.text = element_text(face = "bold", size = base_size*0.5),
          plot.margin = margin(margin_top,0,0,0, unit = "pt"))
  
  
  plot_df <- data.frame(crds(data_dhms$elevation[[year_data$dhm_grid_id]], na.rm = FALSE))
  
  plot_width <- 1200 # px
  
  # elevation_df is to plot the contours.
  elevation_df <- data.frame(plot_df, z = values(data_dems$elevation[[year_data$dem_grid_id]])[,1])
  
  max_swe <- round(quantile(year_data$mod_output_annual_cur$vec_swe_all, 0.98) / 400) * 400 * run_params$output_mult/1000
  swe_breaks <- c(0.000, 0.025, 0.050, 0.125, 0.250, 0.375, 0.500, 0.750, 1.000)*max_swe
  swe_labels <- sprintf(run_params$output_fmt2, swe_breaks)
  swe_labels[length(swe_labels)] <- ""
  
  palette_swe <- c("#CDFFCC", "#99F1B3", "#53BCA0", "#3296B3", "#0770AE", "#00358F", "#d30688", "#ff00ff")
  palette_cur <- palette_swe
  
  # Daily loop to produce the plots.
  # Optionally reduced frequency (e.g. weekly).
  for (day_id in 1:(year_data$model_annual_days_n + 1)) {
    
    # Plot only one every few days, to speed up.
    if (!(day_id %% run_params$plot_daily_maps_frequency)) {
      
      cat("\r** Generating daily plots of SWE and surface type...", day_id, "/", year_data$model_annual_days_n+1, "**")
      cells_cur <- (day_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)

      plot_df$swe <- year_data$mod_output_annual_cur$vec_swe_all[cells_cur]
      plot_df$snow <- as.integer(plot_df$swe > 0)
      plot_df$surf <- factor(year_data$mod_output_annual_cur$vec_surftype_all[cells_cur],
                             levels = c("0", "1", "2", "4", "5")) # Needed to show the full legend even when we have no firn/debris.
      date_text <- format(c(year_data$weather_series_annual_cur$timestamp,
                            year_data$weather_series_annual_cur$timestamp[year_data$model_annual_days_n] + 1)[day_id], "%Y/%m/%d")
      
      # Plot of daily SWE.
      ggplot(plot_df) +
        geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult/1000, alpha = as.character(snow))) +
        scale_alpha_manual(values = c("0" = 0, "1" = 1)) +
        geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = 0.2) +
        {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = 0.15)}+
        {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.2, stroke.color = "#FFFFFF", size = 1.6, min.size = 10)} +
        annotate("label", x = Inf, y = Inf, hjust = 1.3, vjust = 1, label = date_text) +
        scale_fill_stepsn(name = paste0("SWE [", run_params$output_unit, " w.e.]"),
                          colors = palette_cur,
                          limits = c(0,max_swe),
                          breaks = swe_breaks,
                          labels = swe_labels,
                          oob = scales::oob_squish,
                          values = swe_breaks/max(swe_breaks)) +
        guides(alpha = "none") +
        theme_map_swe
      suppressWarnings(ggsave(file.path(run_params$out_daily_dirpath, "gridded", year_data$year_cur, "swe_plots", paste0(sprintf("%03d", day_id), ".jpg")),
                              width = plot_width, height = plot_width * grid_aspect_ratio, units = "px"))
      
      
      
      # Plot of daily surface type.
      ggplot(plot_df) +
        geom_raster(aes(x = x, y = y, fill = surf)) +#, alpha = as.character(snow))) +
        geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = 0.2) +
        geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = 0.15) +
        geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.2, stroke.color = "#FFFFFF", size = 1.6, min.size = 10) +
        annotate("label", x = Inf, y = Inf, hjust = 1.3, vjust = 1, label = date_text) +
        scale_fill_manual(name = "Surface type",
                          values = c("0" = "#EEEEEE",
                                     "1" = "#6992D5",
                                     "2" = "#B3D8FF",
                                     "4" = "#777777",
                                     "5" = "#C5A47A"),
                          labels = c("0" = "Ice",
                                     "1" = "Firn",
                                     "2" = "Snow",
                                     "4" = "Rock",
                                     "5" = "Debris"),
                          drop = FALSE) +
        theme_void() +
        theme(plot.background = element_rect(fill = "#FFFFFF", linetype = "blank"))
      suppressWarnings(ggsave(file.path(run_params$out_daily_dirpath, "gridded", year_data$year_cur, "surftype_plots", paste0(sprintf("%03d", day_id), ".jpg")),
                              width = plot_width, height = plot_width * grid_aspect_ratio, units = "px"))
      
    } # End selection on day_id to plot only one day every few.
    
  } # End daily loop to plot SWE and surface type.
  
  cat("\n")
  
  
  # # Plot of daily cumulative SMB.
  # for (day_id in 1:(year_data$model_annual_days_n+1)) {
  #   cat("\r** Generating daily SMB plots...", day_id, "/", year_data$model_annual_days_n+1, "**")
  #   cells_cur <- (day_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)
  #   max_mb <- 3999
  #   plot_df$massbal <- year_data$mod_output_annual_cur$vec_massbal_cumul[cells_cur]
  #   date_text <- format(c(year_data$weather_series_annual_cur$timestamp, year_data$weather_series_annual_cur$timestamp[year_data$model_annual_days_n] + 1)[day_id], "%Y/%m/%d")
  #   ggplot(plot_df) +
  #     surf_base +
  #     geom_raster(aes(x = x, y = y, fill = massbal)) +
  #     geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = 0.2) +
  #     geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = 0.15) +
  #     geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.2, stroke.color = "#FFFFFF", size = 1.6, min.size = 10) +
  #     annotate("label", x = Inf, y = Inf, hjust = 1.3, vjust = 1, label = date_text) +
  #     scale_fill_fermenter(name = "Cumulative\nSMB [mm w.e.]", palette = "RdBu",
  #                          direction = 1, limits = c(-max_mb,max_mb),
  #                          breaks = c(-3000,-1600,-800,-300,0,300,800,1600,3000)) +
  #     theme_void()
  #   suppressWarnings(ggsave(file.path(run_params$out_daily_dirpath, "gridded", year_cur, "massbal_plots", paste0(sprintf("%03d", day_id), ".jpg")),
  #     width = plot_width, height = plot_width * grid_aspect_ratio, units = "px"))
  # }
  
  cat("\n")
  
}
