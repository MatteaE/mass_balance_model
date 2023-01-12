###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to produce daily plots of SWE and mass balance.     #
#                 These can be directly turned into a nice animation.                             #
###################################################################################################


func_plot_daily_maps <- function(year_data,
                                 run_params,
                                 data_surftype,
                                 data_dems,
                                 data_outlines) {
  
  
  
  surf_r    <- subst(data_surftype$grids[[year_data$surftype_grid_id]], from = c(0, 1, 4, 5), to = c(100, 170, 0, 70))
  surf_g    <- subst(data_surftype$grids[[year_data$surftype_grid_id]], from = c(0, 1, 4, 5), to = c(150, 213, 0, 20))
  surf_b    <- subst(data_surftype$grids[[year_data$surftype_grid_id]], from = c(0, 1, 4, 5), to = c(200, 255, 0, 20))
  surf_base <- ggRGB(stack(surf_r, surf_g, surf_b), r = 1, g = 2, b = 3, ggLayer = TRUE)
  
  dir.create(file.path(run_params$output_dirname, "daily", year_cur, "massbal"), recursive = TRUE)
  dir.create(file.path(run_params$output_dirname, "daily", year_cur, "swe_surftype"), recursive = TRUE)
  
  plot_df <- data.frame(crds(data_dems$elevation[[year_data$dem_grid_id]], na.rm = FALSE))
  
  # elevation_df is to plot the contours.
  elevation_df <- data.frame(plot_df, z = values(data_dems$elevation[[year_data$dem_grid_id]])[,1])
  
  # Plot of daily SWE evolution.
  for (day_id in 1:(year_data$model_annual_days_n + 1)) {
    cat("\r** Generating daily SWE plots...", day_id, "/", year_data$model_annual_days_n+1, "**")
    cells_cur <- (day_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)
    max_swe <- 3500
    plot_df$swe <- clamp(year_data$mod_output_annual_cur$vec_swe_all[cells_cur], -Inf, max_swe, values = TRUE)
    plot_df$snow <- as.integer(plot_df$swe > 0)
    plot_df$surf <- year_data$mod_output_annual_cur$vec_surftype_all[cells_cur]
    date_text <- format(c(year_data$weather_series_annual_cur$timestamp, year_data$weather_series_annual_cur$timestamp[year_data$model_annual_days_n] + 1)[day_id], "%Y/%m/%d")
    ggplot(plot_df) +
      surf_base +
      geom_raster(aes(x = x, y = y, fill = swe, alpha = as.character(snow))) +
      scale_alpha_manual(values = c("0" = 0, "1" = 1)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", size = 0.2) +
      geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = 0.15) +
      geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.2, stroke.color = "#FFFFFF", size = 1.6, min.size = 10) +
      annotate("label", x = Inf, y = Inf, hjust = 1.3, vjust = 1.5, label = date_text) +
      scale_fill_fermenter(name = "SWE [mm]", palette = "RdPu",
                           direction = 1, limits = c(0,max_swe),
                           breaks = c(100,200,400,800,1400,2000,2800,3600)) +
      guides(alpha = "none") +
      theme_void()
    suppressWarnings(ggsave(file.path(run_params$output_dirname, "daily", year_cur, "swe_surftype", paste0(sprintf("%03d", day_id), ".png")), width = 5, height = 3))
  }
  
  cat("\n")
  
  
  # Plot of daily cumulative SMB.
  for (day_id in 1:(year_data$model_annual_days_n+1)) {
    cat("\r** Generating daily SMB plots...", day_id, "/", year_data$model_annual_days_n+1, "**")
    cells_cur <- (day_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)
    max_mb <- 3999
    plot_df$massbal <- year_data$mod_output_annual_cur$vec_massbal_cumul[cells_cur]
    date_text <- format(c(year_data$weather_series_annual_cur$timestamp, year_data$weather_series_annual_cur$timestamp[year_data$model_annual_days_n] + 1)[day_id], "%Y/%m/%d")
    ggplot(plot_df) +
      surf_base +
      geom_raster(aes(x = x, y = y, fill = massbal)) +
      geom_sf(data = as(data_outlines$outlines[[year_data$outline_id]], "sf"), fill = NA, color = "#202020", size = 0.2) +
      geom_contour(data = elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = 0.15) +
      geom_text_contour(data = elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.2, stroke.color = "#FFFFFF", size = 1.6, min.size = 10) +
      annotate("label", x = Inf, y = Inf, hjust = 1.3, vjust = 1.5, label = date_text) +
      scale_fill_fermenter(name = "Cumulative\nSMB [mm w.e.]", palette = "RdBu",
                           direction = 1, limits = c(-max_mb,max_mb),
                           breaks = c(-3000,-1600,-800,-300,0,300,800,1600,3000)) +
      theme_void()
    suppressWarnings(ggsave(file.path(run_params$output_dirname, "daily", year_cur, "massbal", paste0(sprintf("%03d", day_id), ".png")), width = 5, height = 3))
  }
  
  cat("\n")
  
}
