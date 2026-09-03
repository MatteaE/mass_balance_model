###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which plots common map elements, to reuse them.  #
################################################################################################### 

func_plot_map_common_elements <- function(year_data,
                                          data_dems,
                                          data_dhms,
                                          data_outlines) {
  
  # Elements used in glacier-only (DEM) plots -----------------------------------------------------
  # These are: mass balance maps, Voronoi weights
  dem_contour_linesize       <- 0.40
  dem_contour_label_textsize <- 4
  
  dem_grid_extent            <- ext(data_dems$elevation[[year_data$dem_grid_id]])
  dem_grid_area              <- (dem_grid_extent[2] - dem_grid_extent[1]) * (dem_grid_extent[4] - dem_grid_extent[3])
  # Empirical multiplier to reduce label and line size when the modeled extent is very big.
  # Useful for huge glaciers and multi-glacier (e.g. catchment) simulations.
  dem_extent_size_multiplier <- max(0.1, exp(-(max(0,(dem_grid_area-5e6))^2)/5e17))
  
  dem_plot_df_base           <- data.frame(crds(data_dems$elevation[[year_data$dem_grid_id]], na.rm = FALSE))
  dem_elevation_df           <- data.frame(dem_plot_df_base, z = values(data_dems$elevation[[year_data$dem_grid_id]], mat = F))
  
  dem_ele_contours <- NULL
  if (run_params$show_contours) {
    dem_ele_contours          <- geom_contour(data = dem_elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = dem_contour_linesize)
  }
  
  dem_ele_text_contours <- NULL
  if (run_params$show_contour_labels) {
    dem_ele_text_contours     <- geom_text_contour(data = dem_elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*dem_extent_size_multiplier, stroke.color = "#FFFFFF", size = dem_contour_label_textsize*dem_extent_size_multiplier, min.size = 15, fontface = "bold")
  }
  
  
  # Glacier outline, used in both DHM and DEM plots.
  outl_sf                     <- as(data_outlines$outlines[[year_data$outline_id]], "sf")
  
  
  # Elements used in full-grid (DHM) plots --------------------------------------------------------
  # These are: avalanche effect, snow cover duration, snow distribution multiplier, SWE
  dhm_contour_linesize        <- 0.25
  dhm_contour_label_textsize  <- 4
  
  dhm_grid_extent             <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])
  dhm_grid_area               <- (dhm_grid_extent[2] - dhm_grid_extent[1]) * (dhm_grid_extent[4] - dhm_grid_extent[3])
  dhm_grid_aspect_ratio       <- (dhm_grid_extent[4] - dhm_grid_extent[3]) / (dhm_grid_extent[2] - dhm_grid_extent[1])
  # Empirical multiplier to reduce label and line size when the modeled extent is very big.
  # Useful for huge glaciers and multi-glacier (e.g. catchment) simulations.
  dhm_extent_size_multiplier  <- max(0.1, exp(-(max(0,(dhm_grid_area-5e6))^2)/5e17))
  
  dhm_plot_df_base            <- data.frame(crds(data_dhms$elevation[[year_data$dhm_grid_id]], na.rm = FALSE))
  dhm_elevation_df            <- data.frame(dhm_plot_df_base, z = values(data_dhms$elevation[[year_data$dhm_grid_id]], mat = F))
  
  dhm_ele_contours            <- NULL
  if (run_params$show_contours) {
    dhm_ele_contours          <- geom_contour(data = dhm_elevation_df, aes(x = x, y = y, z = z), color = "#202020", linewidth = dhm_contour_linesize)
  }
  
  dhm_ele_text_contours <- NULL
  if (run_params$show_contour_labels) {
    dhm_ele_text_contours     <- geom_text_contour(data = dhm_elevation_df, aes(x = x, y = y, z = z), check_overlap = TRUE, stroke = 0.1*dhm_extent_size_multiplier, stroke.color = "#FFFFFF", size = dhm_contour_label_textsize*dhm_extent_size_multiplier, min.size = 15, fontface = "bold")
  }
  
  
  
  return(list(dem_plot_df_base           = dem_plot_df_base,
              dem_ele_contours           = dem_ele_contours,
              dem_ele_text_contours      = dem_ele_text_contours,
              dem_extent_size_multiplier = dem_extent_size_multiplier,
              outl_sf                    = outl_sf,
              dhm_plot_df_base           = dhm_plot_df_base,
              dhm_ele_contours           = dhm_ele_contours,
              dhm_ele_text_contours      = dhm_ele_text_contours,
              dhm_extent_size_multiplier = dhm_extent_size_multiplier,
              dhm_grid_aspect_ratio      = dhm_grid_aspect_ratio
              ))
  
}
