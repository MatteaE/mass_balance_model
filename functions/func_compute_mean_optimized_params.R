###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the mean of the optimized parameters, #
#                 to be optionally used for the years with no data.                               #
###################################################################################################

func_compute_mean_optimized_params <- function(run_params, overview_annual) {
  
  # Compute mean only on years with appropriate measurements (i.e. optimization).
  ids_annual_sel                <- which(overview_annual$summary_df$year_has_annual_data)
  run_params$mean_melt_factor   <- mean(overview_annual$summary_df$melt_factor[ids_annual_sel], na.rm = T)
  run_params$mean_rad_fact_ice  <- mean(overview_annual$summary_df$rad_fact_ice[ids_annual_sel], na.rm = T)
  run_params$mean_rad_fact_snow <- mean(overview_annual$summary_df$rad_fact_snow[ids_annual_sel], na.rm = T)
  
  ids_winter_sel                <- which(overview_annual$summary_df$year_process_winter)
  run_params$mean_prec_corr     <- mean(overview_annual$summary_df$prec_corr[ids_winter_sel], na.rm = T)
  
  return(run_params)
  
}
