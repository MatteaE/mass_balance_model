###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which makes scatterplots of stake bias versus    #
#                 altitude and snow distribution multiplier.                                      #
################################################################################################### 



func_plot_bias_scatterplots <- function(year_data,
                                        data_dhms,
                                        run_params) {
  
  
  base_size <- 16 # For the plots.
  
  theme_bias_scatterplots <- theme_bw(base_size = base_size) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.99,0.99),
          legend.justification.inside = c(1,1),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.title = element_blank())
  
  
  
  plots <- list()

  
  # Plot of bias vs elevation ---------------------------------------------------------------------
  plot_df <- data.frame(ele  = year_data$massbal_annual_meas_cur$z_dem,
                        bias = year_data$mod_output_annual_cur$stakes_bias*run_params$output_mult/1000)
  plot_mod <- lm(formula = bias~ele,
                 data = plot_df)
  mod_r2_lab <- sprintf("%.3f", summary(plot_mod)$r.squared)
  
  plots[[1]] <-
    ggplot(plot_df) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_point(aes(x = ele, y = bias), shape = 3, stroke = 1.5, size = 1) +
    geom_abline(intercept = plot_mod$coefficients[1],
                slope = plot_mod$coefficients[2],
                color = "red") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
    annotation_custom(grobTree(textGrob(bquote(bold("R"^"2"~"="~.(mod_r2_lab))), x = 0.99,  y = 0.99, hjust=1, vjust = 1,
                                        gp=gpar(col="black")))) +
    xlab("Elevation [m asl]") +
    ylab(paste0("Model bias [", run_params$output_unit, " w.e.]")) +
    theme_bias_scatterplots
  
  
  # Plot of bias vs snow distribution multiplier --------------------------------------------------
  snowdist_mult_r <- setValues(data_dhms$elevation[[year_data$dhm_grid_id]],
                               year_data$dist_topographic_values_red * year_data$dist_probes_norm_values_red)
  
  plot_df    <- data.frame(snowdist_mult  = terra::extract(snowdist_mult_r,
                                                           (year_data$massbal_annual_meas_cur[,c("x", "y")]),
                                                           method = "bilinear",
                                                           ID = FALSE)[,1],
                           bias           = year_data$mod_output_annual_cur$stakes_bias*run_params$output_mult/1000)
  plot_mod   <- lm(formula = bias~snowdist_mult,
                   data = plot_df)
  mod_r2_lab <- sprintf("%.3f", summary(plot_mod)$r.squared)
  
  
  plots[[2]] <- ggplot(plot_df) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_point(aes(x = snowdist_mult, y = bias), shape = 3, stroke = 1.5, size = 1) +
    geom_abline(intercept = plot_mod$coefficients[1],
                slope = plot_mod$coefficients[2],
                color = "red") +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
    annotation_custom(grobTree(textGrob(bquote(bold("R"^"2"~"="~.(mod_r2_lab))), x = 0.99,  y = 0.99, hjust=1, vjust = 1,
                                        gp=gpar(col="black")))) +
    xlab("Snow distribution multiplier [-]") +
    ylab(paste0("Model bias [", run_params$output_unit, " w.e.]")) +
    theme_bias_scatterplots
  
  
  return(plots)
  
}
