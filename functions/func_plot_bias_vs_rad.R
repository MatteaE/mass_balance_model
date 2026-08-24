###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which makes a scatterplot of stake bias versus   #
#                 fraction of melt contributed by radiation.                                      #
################################################################################################### 



func_plot_bias_vs_rad <- function(year_data,
                                  run_params) {
  
  
  base_size <- 16 # For the plots.
  
  theme_bias_rad_plot <- theme_bw(base_size = base_size) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.99,0.99),
          legend.justification.inside = c(1,1),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.title = element_blank())
  
  plot_df <- data.frame(rad_pct = 100*terra::extract(year_data$melt_from_radiation_frac,
                                                     year_data$massbal_annual_meas_cur[,c("x", "y")],
                                                     method = "bilinear",
                                                     ID = FALSE)[,1],
                        bias     = year_data$mod_output_annual_cur$stakes_bias*run_params$output_mult/1000)
  # plot_df <- data.frame(rad_pct = 100*terra::extract(zzz,
  #                                                    year_data$massbal_annual_meas_cur[,c("x", "y")],
  #                                                    method = "bilinear",
  #                                                    ID = FALSE)[,1],
  #                       bias     = year_data$mod_output_annual_cur$stakes_bias*run_params$output_mult/1000)
  plot_mod <- lm(formula = bias~rad_pct,
                 data = plot_df)
  mod_r2_lab <- sprintf("%.3f", summary(plot_mod)$r.squared)
  
  
  
  plot_bias_rad <-ggplot(plot_df) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_point(aes(x = rad_pct, y = bias), shape = 3, stroke = 1.5, size = 1) +
    geom_abline(intercept = plot_mod$coefficients[1],
                slope = plot_mod$coefficients[2],
                color = "red") +
    annotation_custom(grobTree(textGrob(bquote(bold("R"^"2"~"="~.(mod_r2_lab))), x = 0.99,  y = 0.99, hjust=1, vjust = 1,
                                        gp=gpar(col="black")))) +
    xlab("Fraction of melt from radiation [%]") +
    ylab(paste0("Model bias [", run_params$output_unit, " w.e.]")) +
    theme_bias_rad_plot
  
  
  return(plot_bias_rad)
  
}
