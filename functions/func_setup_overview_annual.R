###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to prepare overview (i.e. 1-value-per-year)         #
#                 variables before the main loop.                                                 #
###################################################################################################

func_setup_overview_annual <- function(run_params) {
  
  # Here we put:
  # - the summary_df with annual summary values
  # - the areaplots_list with the annual area plots to be put in the overview_areaplot.pdf
  # - the daily_data_list with the daily glacier-wide values, used for the last overview plot.
  overview_annual <- list()

  # In this data frame we put all the annual results
  # for the overview document: glacier-wide mass balance
  # (annual and winter, all versions), ELA, AAR, RMSE,
  # optimized parameters, cumulative mass balance.
  # We also put some logical variables (one value per year)
  # such as whether the year has data and whether we already
  # computed the starting SWE (from the previous year's modeling).
  overview_annual$summary_df <- data.frame(year                        = run_params$years,
                                           mb_annual_meas_corr         = NA,
                                           mb_annual_meas              = NA,
                                           mb_annual_hydro             = NA,
                                           # mb_annual_hydro_corr        = NA,
                                           mb_annual_fixed             = NA,
                                           mb_winter_meas              = NA, # This stays NA unless winter measurements are available.
                                           mb_winter_fixed             = NA,
                                           ela                         = NA,
                                           aar                         = NA,
                                           rmse                        = NA,
                                           melt_factor                 = NA,
                                           rad_fact_ice                = NA,
                                           rad_fact_snow               = NA,
                                           prec_corr                   = NA,
                                           mb_cumul                    = NA,
                                           year_has_data               = FALSE,
                                           year_starting_swe_available = FALSE) # This is used to optionally enable re-using of the modeled SWE as starting condition for a modeled year. If swe_prev_available[year_id] is TRUE, then year_id can use as starting condition the model output of (year_id-1).
  
  # Here we will put daily data for the overview plot
  # of cumulative daily glacierwide mass balance.
  overview_annual$daily_data_list <- list()

  return(overview_annual)
}
