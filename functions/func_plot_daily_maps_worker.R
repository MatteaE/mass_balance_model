###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code of a low-level worker to produce daily plots of     #
#                 SWE and surface type. It can work with the result of either winter or annual    #
#                 model runs.                                                                     #
###################################################################################################


# period_sel can be either "winter" or "annual",
# it is used to select the appropriate plotting variables.

func_plot_daily_maps_worker <- function(year_data,
                                        run_params,
                                        data_dhms,
                                        data_dems,
                                        data_outlines,
                                        period_sel) {
  
  # Output paths for the plots.
  out_p          <- file.path(run_params$out_daily_dirpath, "gridded", year_data$year_cur)
  out_swe_p      <- file.path(out_p, period_sel, "swe_plots")
  out_surftype_p <- file.path(out_p, period_sel, "surftype_plots")
  
  dir.create(out_swe_p, showWarnings = F, recursive = TRUE)
  dir.create(out_surftype_p, showWarnings = F, recursive = TRUE)
  
  
  
  weather_series_cur <- year_data[[paste0("weather_series_", period_sel, "_cur")]]
  mod_output_cur     <- year_data[[paste0("mod_output_", period_sel, "_cur")]]
  model_days_n       <- year_data[[paste0("model_", period_sel, "_days_n")]]
  
  
  # Common elements for the SWE plots -------------------------------------------------------------
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
  
  # We keep max_swe from the annual simulation even when we are plotting just the winter one,
  # for consistency and comparability of the plots.
  max_swe <- round(quantile(year_data$mod_output_annual_cur$vec_swe_all, 0.98) / 400) * 400 * run_params$output_mult/1000
  swe_breaks <- c(0.000, 0.025, 0.050, 0.125, 0.250, 0.375, 0.500, 0.750, 1.000)*max_swe
  swe_labels <- sprintf(run_params$output_fmt2, swe_breaks)
  swe_labels[length(swe_labels)] <- ""
  
  palette_swe <- c("#CDFFCC", "#99F1B3", "#53BCA0", "#3296B3", "#0770AE", "#00358F", "#d30688", "#ff00ff")
  palette_cur <- palette_swe
  
  
  
  
  # Daily loop to produce the plots ---------------------------------------------------------------
  # Optionally reduced frequency (e.g. weekly).
  for (day_id in 1:(model_days_n + 1)) {
    
    # Plot only one every few days, to speed up.
    if (!(day_id %% run_params[[paste0("plot_daily_maps_", period_sel, "_freq")]])) {
      
      cat("\r** Generating daily plots of SWE and surface type from the", period_sel, "simulation...", day_id, "/", model_days_n+1, "**")
      cells_cur <- (day_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)
      
      plot_df$swe  <- mod_output_cur$vec_swe_all[cells_cur]
      plot_df$snow <- as.integer(plot_df$swe > 0)
      plot_df$surf <- factor(mod_output_cur$vec_surftype_all[cells_cur],
                             levels = c("0", "1", "2", "4", "5")) # Needed to show the full legend even when we have no firn/debris.
      date_cur <- c(weather_series_cur$timestamp,
                    weather_series_cur$timestamp[model_days_n] + 1)[day_id]
      date_cur_str <- format(date_cur, "%Y-%m-%d")
      
      # Plot of daily SWE.
      ggplot(plot_df) +
        geom_raster(aes(x = x, y = y, fill = swe * run_params$output_mult/1000, alpha = as.character(snow))) +
        scale_alpha_manual(values = c("0" = 0, "1" = 1)) +
        geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = 0.2) +
        {if (run_params$show_contours) geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = 0.15)}+
        {if (run_params$show_contour_labels) geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.2, stroke.color = "#FFFFFF", size = 1.6, min.size = 10)} +
        annotate("label", x = Inf, y = Inf, hjust = 1.3, vjust = 1, label = date_cur_str) +
        scale_fill_stepsn(name = paste0("SWE [", run_params$output_unit, " w.e.]"),
                          colors = palette_cur,
                          limits = c(0,max_swe),
                          breaks = swe_breaks,
                          labels = swe_labels,
                          oob = scales::oob_squish,
                          values = swe_breaks/max(swe_breaks)) +
        guides(alpha = "none") +
        theme_map_swe
      suppressWarnings(ggsave(file.path(out_swe_p, paste0(date_cur_str, ".jpg")),
                              width = plot_width, height = plot_width * grid_aspect_ratio, units = "px"))
      
      
      
      # Plot of daily surface type.
      ggplot(plot_df) +
        geom_raster(aes(x = x, y = y, fill = surf)) +#, alpha = as.character(snow))) +
        geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", linewidth = 0.2) +
        geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = 0.15) +
        geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.2, stroke.color = "#FFFFFF", size = 1.6, min.size = 10) +
        annotate("label", x = Inf, y = Inf, hjust = 1.3, vjust = 1, label = date_cur_str) +
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
      suppressWarnings(ggsave(file.path(out_surftype_p, paste0(date_cur_str, ".jpg")),
                              width = plot_width, height = plot_width * grid_aspect_ratio, units = "px"))
      
    } # End selection on day_id to plot only one day every few.
    
  } # End daily loop to plot SWE and surface type.
  
}
