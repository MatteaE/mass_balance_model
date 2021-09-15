###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the mean of the optimized parameters, #
#                 to be optionally used for the years with no data.                               #
###################################################################################################

func_compute_mean_optimized_params <- function(run_params, overview_annual) {
  
  
  run_params$mean_melt_factor   <- mean(overview_annual$summary_df$melt_factor, na.rm = T)
  run_params$mean_rad_fact_ice  <- mean(overview_annual$summary_df$rad_fact_ice, na.rm = T)
  run_params$mean_rad_fact_snow <- mean(overview_annual$summary_df$rad_fact_snow, na.rm = T)
  run_params$mean_prec_corr     <- mean(overview_annual$summary_df$prec_corr, na.rm = T)
  
  return(run_params)
  
}
