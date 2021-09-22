###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to estimate the appropriate color scale         #
#                 boundaries for the mass balance maps.                                           #
###################################################################################################  

# The color scale must remain the same across years.
# So we don't base it on the modeled mass balance,
# but rather on the measurements.
# Algorithm:
# If any annual mass balance measurements are present,
#   Find the 90th percentile of their absolute value
#   Find closest multiplier of the default color scale breaks which puts the 90th percentile at the end of the scale.
# If no measurements are present,
#   Look at the weather series
#   Aggregate AWS solid precipitation over each year
#   Find the 90th percentile and repeat.

func_compute_massbal_colorscale_multiplier <- function(data_massbalance_annual,
                                                       data_dems,
                                                       data_weather,
                                                       run_params) {
  
  multipliers_possible <- c(0.5, 1, 1.5, 2, 2.5, 3:10)
  
  data_massbalance_annual_sim <- data_massbalance_annual[as.integer(format(data_massbalance_annual$end_date, "%Y")) %in% run_params$years,]
  
  if (nrow(data_massbalance_annual_sim) > 0) {
    
    mb_val <- as.numeric(quantile(abs(data_massbalance_annual$massbal), 0.9)) / 1e3
    
  } else {
    
    data_weather_sim <- data_weather[data_weather$year_hydro %in% run_params$years,]
    ele_max <- cellStats(data_dems$elevation[[1]], "max")
    prec_solid_annual_max <- rep(NA_real_, run_params$n_years)
    for (year_id in 1:run_params$n_years) {
      year_cur <- run_params$years[year_id]
      ids_year <- which(data_weather_sim$year_hydro == year_cur)
      ids_cold <- which(data_weather_sim$t2m_mean < run_params$weather_snowfall_temp)
      prec_solid_annual_max[year_id] <- sum(data_weather_sim$precip[intersect(ids_year, ids_cold)]) * (1 + (ele_max - run_params$weather_aws_elevation) * run_params$default_prec_elegrad / 1e4)
    }
    # Empirical: with avalanches, we assume that maximum mass balance
    # to plot is 5 times the estimated maximum solid precipitation.
    mb_val <- 5 * as.numeric(quantile(prec_solid_annual_max, 0.9)) / 1e3
    
  }
  
  multiplier_id_best <- which.min(abs(mb_val - abs(run_params$mb_colorscale_breaks[1]) * multipliers_possible))
  
  return(multipliers_possible[multiplier_id_best])
  
}
