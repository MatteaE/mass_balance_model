###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to extract and organize the overview values for     #
#                 the year, for the final overview plots.                                         #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

# Save overview values for the year.
# After the loop we show them in the multi-year plots.
df_overview$mb_annual_meas_corr[year_id]  <- massbal_annual_values[["meas_period_corr"]] / 1e3
df_overview$mb_annual_meas[year_id]       <- massbal_annual_values[["meas_period"]] / 1e3
df_overview$mb_annual_hydro[year_id]      <- massbal_annual_values[["hydro"]] / 1e3
# df_overview$mb_annual_hydro_corr[year_id] <- massbal_annual_values[["hydro_corr"]] / 1e3
df_overview$mb_annual_fixed[year_id]      <- massbal_annual_values[["fixed"]] / 1e3
if (process_winter) {df_overview$mb_winter_meas[year_id] <- massbal_winter_values[["meas_period"]] / 1e3}
df_overview$mb_winter_fixed[year_id]      <- massbal_winter_values [["fixed"]] / 1e3
df_overview$ela[year_id]                  <- ela_aar[["ela"]]
df_overview$aar[year_id]                  <- ela_aar[["aar"]] * 100
df_overview$rmse[year_id]                 <- mod_output_annual_cur$global_rms / 1e3
df_overview$melt_factor[year_id]          <- year_cur_params$melt_factor + optim_corr_annual$melt_factor
df_overview$rad_fact_ice[year_id]         <- year_cur_params$rad_fact_ice + optim_corr_annual$rad_fact_ice
df_overview$rad_fact_snow[year_id]        <- year_cur_params$rad_fact_snow + optim_corr_annual$rad_fact_ice * year_cur_params$rad_fact_ratio_snow_ice
df_overview$prec_corr[year_id]            <- year_cur_params$prec_corr + optim_corr_annual$prec_corr
