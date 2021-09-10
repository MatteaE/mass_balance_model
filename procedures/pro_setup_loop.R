###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to prepare variables before the main loop.          #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.


# Cleanup memory (temporary variables during loading!)
invisible(gc())


# In this data frame we put all the annual results
# for the overview document: glacier-wide mass balance
# (annual and winter, all versions), ELA, AAR, RMSE,
# optimized parameters, cumulative mass balance.
df_overview <- data.frame(year                 = run_params$years,
                          mb_annual_meas_corr  = NA,
                          mb_annual_meas       = NA,
                          mb_annual_hydro      = NA,
                          # mb_annual_hydro_corr = NA,
                          mb_annual_fixed      = NA,
                          mb_winter_meas       = NA, # This stays NA unless winter measurements are available.
                          mb_winter_fixed      = NA,
                          ela                  = NA,
                          aar                  = NA,
                          rmse                 = NA,
                          melt_factor          = NA,
                          rad_fact_ice         = NA,
                          rad_fact_snow        = NA,
                          prec_corr            = NA,
                          mb_cumul             = NA)

# This annual vector is used to optionally enable re-using of the
# modeled SWE as starting condition for a modeled year.
# If swe_prev_available[year_id] is TRUE, then year_id
# can use as starting condition the model output of (year_id-1).
swe_prev_available <- rep(FALSE, run_params$n_years)

# Prepare some variables to store annual values.
# These are filled within pro_write_year_output.R
# and used within pro_plot_write_overview.R.
mb_series_all_raw <- list()
mb_series_all_dates <- list()
mb_series_all_measperiod_dates <- list()

# Here we will put just the final mass balance for each
# year, to produce the overview_areaplot multi-page PDF file.
overview_areaplots  <- list()

# Here we will put daily data for the overview plot
# of cumulative daily glacierwide mass balance.
overview_daily_data <- list()

# Create output directory.
dir.create(file.path(run_params$output_dirname, "annual_results"), recursive = TRUE)
