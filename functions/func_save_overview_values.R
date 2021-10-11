###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to extract and organize the overview values for     #
#                 the year, for the final overview plots.                                         #
###################################################################################################


func_save_overview_values <- function(year_data,
                                      year_cur_params,
                                      run_params,
                                      df_overview) {
  
  # Save overview values for the year.
  # After the loop we show them in the multi-year plots.
  df_overview$mb_annual_hydro[year_data$year_id]      <- year_data$massbal_annual_values[["hydro"]] * run_params$output_mult / 1e3
  # df_overview$mb_annual_hydro_corr[year_data$year_id] <- year_data$massbal_annual_values[["hydro_corr"]] * run_params$output_mult / 1e3
  # df_overview$mb_annual_fixed[year_data$year_id]      <- year_data$massbal_annual_values[["fixed"]] * run_params$output_mult / 1e3
  df_overview$mb_winter_fixed[year_data$year_id]      <- year_data$massbal_winter_values [["fixed"]] * run_params$output_mult / 1e3
  df_overview$ela[year_data$year_id]                  <- year_data$ela_aar[["ela"]]
  df_overview$aar[year_data$year_id]                  <- year_data$ela_aar[["aar"]] * 100
  df_overview$melt_factor[year_data$year_id]          <- year_cur_params$melt_factor + year_data$optim_corr_annual$melt_factor
  df_overview$rad_fact_ice[year_data$year_id]         <- year_cur_params$rad_fact_ice + year_data$optim_corr_annual$rad_fact_ice
  df_overview$rad_fact_snow[year_data$year_id]        <- year_cur_params$rad_fact_snow + year_data$optim_corr_annual$rad_fact_ice * year_cur_params$rad_fact_ratio_snow_ice
  df_overview$prec_corr[year_data$year_id]            <- year_cur_params$prec_corr + year_data$optim_corr_annual$prec_corr

  # Below: additional output values in case the year has measured mass balance data.
  if (year_data$nstakes_annual > 0) {
    
    df_overview$mb_annual_meas_corr[year_data$year_id]  <- year_data$massbal_annual_values[["meas_period_corr"]] * run_params$output_mult / 1e3
    df_overview$mb_annual_meas[year_data$year_id]       <- year_data$massbal_annual_values[["meas_period"]] * run_params$output_mult / 1e3
    if (year_data$process_winter) {
      df_overview$mb_winter_meas[year_data$year_id]     <- year_data$massbal_winter_values[["meas_period"]] * run_params$output_mult / 1e3
    }
    df_overview$rmse[year_data$year_id]                 <- year_data$mod_output_annual_cur$global_rms * run_params$output_mult / 1e3
    df_overview$year_has_data[year_data$year_id]      <- TRUE
    
  }
  
  return(df_overview)
}
