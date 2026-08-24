###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to compute the derived year parameters:             #
#                 the ratio of snow/ice radiation factors the year modeling periods, and          #
#                 the series of daily "prec_summer_fact".                                         #
#                 This code is called at the end of the year parameters loading.                  #
###################################################################################################

func_compute_derived_year_params <- function(year_data, year_cur_params, run_params) {
  
  # Compute ratio of snow to ice radiation factors.
  # We will keep this ratio constant as we optimize
  # the radiation factors.
  year_cur_params$rad_fact_ratio_snow_ice <- year_cur_params$rad_fact_snow / year_cur_params$rad_fact_ice
  
  
  
  
  # Compute start and end of current hydrological year.
  # In the Northern Hemisphere the hydrological year starts on 01 Oct YYYY-1 at 00:00 and ends on 01 Oct YYYY at 00:00.
  # In the Southern Hemisphere the hydrological year starts on 01 Apr YYYY-1 at 00:00 and ends on 01 Apr YYYY at 00:00.
  # Since we use Date objects which don't include the time of day, we can set the
  # hydro end to October 1 (else we would miss the mass balance
  # between YYYY/09/30 00:00 and YYYY/10/01 00:00).
  if (run_params$north_south == "North") {
    year_cur_params$hydro_start <- as.Date(paste(year_data$year_cur-1, 10, 1), format="%Y %m %d")
    year_cur_params$hydro_end   <- as.Date(paste(year_data$year_cur, 10, 1), format = "%Y %m %d")
    
    year_cur_params$fixed_winter_start <- as.Date(paste(year_data$year_cur-1, run_params$massbal_fixed_winter_start), format = "%Y %m/%d")
    year_cur_params$fixed_winter_end   <- as.Date(paste(year_data$year_cur, run_params$massbal_fixed_winter_end), format = "%Y %m/%d")
  } else {
    year_cur_params$hydro_start <- as.Date(paste(year_data$year_cur-1, 4, 1), format="%Y %m %d")
    year_cur_params$hydro_end   <- as.Date(paste(year_data$year_cur, 4, 1), format = "%Y %m %d")
    
    year_cur_params$fixed_winter_start <- as.Date(paste(year_data$year_cur-1, run_params$massbal_fixed_winter_start), format = "%Y %m/%d")
    year_cur_params$fixed_winter_end   <- as.Date(paste(year_data$year_cur-1, run_params$massbal_fixed_winter_end), format = "%Y %m/%d")
  }
  
  
  # Compute daily precipitation summer factor following the user-selected method.
  # We compute it over the full, actual period (YYYY-1/01/01 to YYYY/12/31), which is sure
  # to include the full simulation period and also takes care of leap years.
  days_seq <- seq.Date(as.Date(paste(year_data$year_cur-1, "01", "01", sep = "/")),
                       as.Date(paste(year_data$year_cur, "12", "31", sep = "/")),
                       by = "1 day")
  year_cur_params$params_daily_df <- data.frame(date             = days_seq,
                                                prec_summer_fact = NA_real_,
                                                prec_elegrad     = NA_real_,
                                                temp_elegrad     = NA_real_)
  
  # . With constant value within each month.
  if (run_params$params_daily_interp == "constant") {
    for (month_id in 1:12) {
      ids_month <- which(as.integer(format(year_cur_params$params_daily_df$date, "%m")) == month_id)
      year_cur_params$params_daily_df$prec_summer_fact[ids_month] <- year_cur_params$prec_summer_fact[month_id]
      year_cur_params$params_daily_df$prec_elegrad[ids_month]     <- year_cur_params$prec_elegrad[month_id]
      year_cur_params$params_daily_df$temp_elegrad[ids_month]     <- year_cur_params$temp_elegrad[month_id]
    }
    
    # . With daily linear interpolation from one month midpoint to the next.
  } else {
    
    # In a single line we compute the DOY of each month mid-point, for two
    # consecutive years (DOYs do not reset, so they go up to ~730).
    month_dmid <- as.integer(format(c(as.Date(paste(year_data$year_cur-1, sprintf("%02d", 1:12), c(15,14,rep(15,10)), sep = "/")),
                                      as.Date(paste(year_data$year_cur, sprintf("%02d", 1:12), c(15,14,rep(15,10)), sep = "/"))), "%j")) +
      c(rep(0, 12), rep(as.integer(format(as.Date(paste0(year_data$year_cur-1, "/12/31")), "%j")), 12))
    
    # Prepare the series to be interpolated (over the two years that we pick).
    prec_summer_fact <- rep(year_cur_params$prec_summer_fact, 2)
    prec_elegrad     <- rep(year_cur_params$prec_elegrad, 2)
    temp_elegrad     <- rep(year_cur_params$temp_elegrad, 2)
    
    
    # Do the linear interpolation. rule = 2 means the values at the ends
    # (Jan 1 to Jan 14 and Dec 16 to Dec 31) get extrapolated with constant values.
    year_cur_params$params_daily_df$prec_summer_fact <- approx(x = month_dmid,
                                                               y = prec_summer_fact,
                                                               method = "linear",
                                                               xout = 1:nrow(year_cur_params$params_daily_df),
                                                               rule = 2)$y
    
    year_cur_params$params_daily_df$prec_elegrad     <- approx(x = month_dmid,
                                                               y = prec_elegrad,
                                                               method = "linear",
                                                               xout = 1:nrow(year_cur_params$params_daily_df),
                                                               rule = 2)$y
    
    year_cur_params$params_daily_df$temp_elegrad     <- approx(x = month_dmid,
                                                               y = temp_elegrad,
                                                               method = "linear",
                                                               xout = 1:nrow(year_cur_params$params_daily_df),
                                                               rule = 2)$y
    
  }
  
  return(year_cur_params)
  
}
