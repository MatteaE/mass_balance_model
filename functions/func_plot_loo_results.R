###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to plot the results of LOO validation/sensitivity.  #
###################################################################################################


func_plot_loo_results <- function(year_data,
                                  run_params) {
  
  plots_loo_results <- list()
  
  
  base_size <- 16 # For the plot
  theme_loo_plot <- theme_bw(base_size = base_size) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.01,0.01),
          legend.justification.inside = c(0,0),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.title = element_blank(),
          legend.spacing.y = unit(0, "pt"),
          legend.margin=margin(0,0,0,0,"pt"))
  
  
  plot_df                <- year_data$df_runs_smb
  stakes_col_ids         <- grep("^s[0-9]+$", names(plot_df)) # Only the stakes values, which are called like s01, s123, etc.
  plot_df$stakes_average <- rowMeans(plot_df[,stakes_col_ids])
  
  plot_df_melt1           <- melt(plot_df,
                                  id.vars = c("run_id", "corr_fact", "run_type"),
                                  measure.vars = names(plot_df)[stakes_col_ids])
  
  plot_df_melt2          <- melt(plot_df,
                                 id.vars = c("run_id", "corr_fact", "run_type"),
                                 measure.vars = c("stakes_average", "mb_annual_hydro", "mb_annual_measperiod"))
  
  
  # NOTE: on the X axis we plot 1+corr_fact, since it is an additive change
  # (corr_fact = 0 means original parameter value, corr_fact = 1 means double it)
  
  plots_loo_results[[1]] <- ggplot(plot_df_melt1) +
    
    # Add horizontal line with arithmetic mean of stakes (back-calculated as mean model result - global bias: this is an arithmetic mean over potentially inconsistent time periods, used for the LOO plot only).
    geom_hline(aes(linetype = "hline_stakes_measured_avg",
                   yintercept = (run_params$output_mult/1e3)*(plot_df$stakes_average[which(plot_df$run_type == "main_optim_final")] - year_data$mod_output_annual_cur$global_bias))) +
    
    # Draw the individual stakes
    geom_line(aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, group = variable, color = "stakes_single"), alpha = 0.4, linewidth = 0.5) +
    geom_point(aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, group = variable, shape = run_type, color = "stakes_single"), alpha = 0.4, size = 3, stroke = 0.5) +
    
    # Draw the glacier-wide mass balances and the arithmetic mean of the stakes, with partial alpha.
    geom_line(data = plot_df_melt2, aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, color = variable), linewidth = 1, alpha = 0.6) +
    geom_point(data = plot_df_melt2, aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, color = variable, shape = run_type), size = 5, stroke = 0.5, alpha = 0.6) +
    
    # Draw again the modeled stake average, we want it on top and with alpha = 1.
    geom_line(data = plot_df,
              aes(x = 1+corr_fact, y = stakes_average*run_params$output_mult/1e3, color = "stakes_average"), linewidth = 1) +
    geom_point(data = plot_df, aes(x = 1+corr_fact, y = stakes_average*run_params$output_mult/1e3, color = "stakes_average", shape = run_type), size = 5, stroke = 0.5) +
    
    scale_color_manual(values = c("hline_stakes_measured_avg" = "black",
                                  "stakes_average" = "black",
                                  "mb_annual_hydro" = "#FF9000",
                                  "mb_annual_measperiod" = "#FF0000",
                                  "stakes_single" = "#404040"),
                       labels = c("hline_stakes_measured_avg" = "Measured stake average",
                                  "stakes_average" = "Modeled stake average",
                                  "mb_annual_hydro" = "Glacier-wide, hydrological year",
                                  "mb_annual_measperiod" = "Glacier-wide, annual measurement period",
                                  "stakes_single" = "Individual stakes")) +
    scale_linetype_manual(values = c("hline_stakes_measured_avg" ="dashed"),
                          labels = c("hline_stakes_measured_avg" = "Measured stake average")) +
    scale_shape_manual(values = c("main_optim_dummy" = 4,
                                  "main_optim" = 4,
                                  "main_optim_final" = 19)) +
    guides(color = guide_legend(order = 1),
           linetype = guide_legend(order = 2),
           shape = guide_none()) +
    scale_y_continuous(expand = expansion(mult = c(0.2,0.05),0)) +
    xlab("Optimization factor [-]") +
    ylab(paste0("Mass balance [", run_params$output_unit, " w.e.]")) +
    theme_loo_plot
  
  
  # NOTE: if more plots are added to this list then this function should return the list, not its first element
  return(plots_loo_results[[1]])
  
}
