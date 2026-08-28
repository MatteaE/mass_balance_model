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
  
  
  
  
  # Line and point plot of mass balance of individual stakes --------------------------------------
  # (in all model realizations, vs correction factor)
  plot_df                <- year_data$df_runs_smb
  stakes_col_ids         <- grep("^s[0-9]+$", names(plot_df)) # Only the stakes values, which are called like s01, s123, etc.
  # Compute the weighted average of the SMB at the stakes (same weights as for the bias).
  plot_df$stakes_average <- rowMeans(plot_df[,stakes_col_ids] * rep(year_data$massbal_annual_meas_cur$area_weight, each = nrow(plot_df)))
  
  plot_df_melt1          <- melt(plot_df,
                                 id.vars = c("run_id", "corr_fact", "run_type"),
                                 measure.vars = names(plot_df)[stakes_col_ids])
  
  plot_df_melt2          <- melt(plot_df,
                                 id.vars = c("run_id", "corr_fact", "run_type"),
                                 measure.vars = c("stakes_average", "mb_annual_hydro", "mb_annual_measperiod"))
  
  
  # NOTE: on the X axis we plot 1+corr_fact, since it is an additive change
  # (corr_fact = 0 means original parameter value, corr_fact = 1 means double it)
  
  plots_loo_results[[1]] <- ggplot(plot_df_melt1) +
    
    # Add horizontal dashed line with weighted mean of stakes
    # (back-calculated as weighted mean model result minus weighted bias: 
    # this is a mean over potentially inconsistent time periods if
    # stakes have different survey dates, it is used for the LOO plot only).
    geom_hline(aes(linetype = "hline_stakes_measured_avg",
                   yintercept = (run_params$output_mult/1e3)*(plot_df$stakes_average[which(plot_df$run_type == "main_optim_final")] - year_data$mod_output_annual_cur$weighted_bias))) +
    
    # If the stakes are weighted, also add horizontal dotted line
    # with arithmetic (unweighted) mean of stakes (back-calculated as arithmetic
    # mean model result minus weighted (global) bias: this is a mean over
    # potentially inconsistent time periods if stakes have different survey dates, it is used for the LOO plot only).
    {if (year_data$annual_bias_weighted_logi) geom_hline(aes(linetype = "hline_stakes_measured_avg_unweighted",
                                                             yintercept = (run_params$output_mult/1e3)*(rowMeans(plot_df[which(plot_df$run_type == "main_optim_final"),stakes_col_ids]) - year_data$mod_output_annual_cur$global_bias)))} +
    
    # Draw the individual stakes
    geom_line(aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, group = variable, color = "stakes_single"), alpha = 0.4, linewidth = 0.5) +
    geom_point(aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, group = variable, shape = run_type, color = "stakes_single"), alpha = 0.4, size = 2, stroke = 0.3) +
    
    # Draw the glacier-wide mass balances and the arithmetic mean of the stakes, with partial alpha.
    geom_line(data = plot_df_melt2, aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, color = variable), linewidth = 1, alpha = 0.6) +
    geom_point(data = plot_df_melt2, aes(x = 1+corr_fact, y = value*run_params$output_mult/1e3, color = variable, shape = run_type), size = 5, stroke = 0.5, alpha = 0.6) +
    
    # Draw again the modeled stake average, we want it on top and with alpha = 1.
    geom_line(data = plot_df,
              aes(x = 1+corr_fact, y = stakes_average*run_params$output_mult/1e3, color = "stakes_average"), linewidth = 1) +
    geom_point(data = plot_df, aes(x = 1+corr_fact, y = stakes_average*run_params$output_mult/1e3, color = "stakes_average", shape = run_type), size = 5, stroke = 0.5) +
    
    scale_color_manual(values = c("hline_stakes_measured_avg" = "black",
                                  "hline_stakes_measured_avg_unweighted" = "black",
                                  "stakes_average" = "black",
                                  "mb_annual_hydro" = "#FF9000",
                                  "mb_annual_measperiod" = "#FF0000",
                                  "stakes_single" = "#404040"),
                       labels = c("hline_stakes_measured_avg" = "Measured stake average",
                                  "hline_stakes_measured_avg_unweighted" = "Measured stake average (unweighted)",
                                  "stakes_average" = "Modeled stake average",
                                  "mb_annual_hydro" = "Glacier-wide, hydrological year",
                                  "mb_annual_measperiod" = "Glacier-wide, annual measurement period",
                                  "stakes_single" = "Individual stakes")) +
    scale_linetype_manual(values = c("hline_stakes_measured_avg" ="dashed",
                                     "hline_stakes_measured_avg_unweighted" = "dotted"),
                          labels = c("hline_stakes_measured_avg" = "Measured stake average",
                                     "hline_stakes_measured_avg_unweighted" = "Measured stake average (unweighted)")) +
    scale_shape_manual(values = c("main_optim_dummy" = 1,
                                  "main_optim" = 1,
                                  "main_optim_final" = 19,
                                  "loo" = 4)) +
    guides(color = guide_legend(order = 1),
           linetype = guide_legend(order = 2),
           shape = guide_none()) +
    scale_y_continuous(expand = expansion(mult = c(0.25,0.05),0)) +
    xlab("Optimization factor [-]") +
    ylab(paste0("LOO mass balance [", run_params$output_unit, " w.e.]")) +
    theme_loo_plot
  
  
  
  
  # Boxplot of glacier-wide mass balance in all the LOO realizations (sensitivity) ----------------
  mb_sensitivity_df <- data.frame(mb_measperiod = year_data$df_runs_smb$mb_annual_measperiod[year_data$df_loo_out$loo_run_id],
                                  mb_hydro      = year_data$df_runs_smb$mb_annual_hydro[year_data$df_loo_out$loo_run_id],
                                  stake_id      = year_data$df_loo_out$stake_id)
  mb_sensitivity_df_melt <- melt(mb_sensitivity_df,
                                 id.vars = "stake_id")
  plots_loo_results[[2]] <- ggplot(mb_sensitivity_df_melt) +
    geom_boxplot(aes(x = variable, y = value*run_params$output_mult/1e3)) +
    # Also add non-LOO values, as points.
    geom_point(data = data.frame(type = c("mb_measperiod", "mb_hydro"),
                                 mb   = c(year_data$massbal_annual_values$meas_period.mean,
                                          year_data$massbal_annual_values$hydro.mean)),
               aes(x = type, y = mb*run_params$output_mult/1e3, color = type),
               shape = 8) +
    scale_x_discrete(labels = c("mb_measperiod" = "Measurement period (annual)",
                                "mb_hydro"      = "Hydrological year")) +
    scale_color_manual(values = c("mb_measperiod" = "#FF00FF",
                                  "mb_hydro" = "#0000FF"),
                       guide = guide_none()) +
    xlab("") +
    ylab(paste0("LOO mass balance [", run_params$output_unit, " w.e.]")) +
    theme_loo_plot
  
  
  # Align panels.
  plots_out <- plot_grid(plotlist = plots_loo_results, align = "hv", ncol = 1, nrow = 2)
  
  
  return(plots_out)
  
}
