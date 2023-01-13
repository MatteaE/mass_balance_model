###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to estimate the avalanche deposition limit      #
#                 in case the user leaves it to NA. From experience on Barkrak and Pers,          #
#                 we use a very empirical formula depending on the annual snowfall.               #
###################################################################################################  

func_compute_deposition_lim <- function(run_params,
                                        data_dems,
                                        data_weather) {
  
  data_weather_sim <- data_weather[data_weather$year_hydro %in% run_params$years,]
  ele_max <- global(data_dems$elevation[[1]], fun = "max", na.rm = TRUE)[,1]
  ids_cold <- which(data_weather_sim$t2m_mean < run_params$weather_snowfall_temp)
  prec_solid_annual_max <- rep(NA_real_, run_params$n_years)
  for (year_id in 1:run_params$n_years) {
    year_cur <- run_params$years[year_id]
    ids_year <- which(data_weather_sim$year_hydro == year_cur)
    # If the weather series does not cover the modeling period fully,
    # we will have an NA here. The model will anyway stop with an
    # informative message when we try to simulate mass balance.
    if (length(ids_year) > 0) {
      prec_solid_annual_max[year_id] <- sum(data_weather_sim$precip[intersect(ids_year, ids_cold)] * (1 + (ele_max - run_params$weather_aws_elevation) * (run_params$default_prec_elegrad[data_weather_sim$month[intersect(ids_year, ids_cold)]]) / 1e4))
    } else {
      prec_solid_annual_max[year_id] <- NA_real_
    }
  }
  
  # If the weather data does not cover the modeling period,
  # we use a dummy 1000 value, the model will anyway always
  # stop and complain of missing data with an informative message.
  if (any(!is.na(prec_solid_annual_max))) {
    run_params$deposition_mass_lim <- 8 * as.numeric(quantile(prec_solid_annual_max, 0.9, na.rm = T)) / 1e3
  } else {
    run_params$deposition_mass_lim <- 1000
  }
  
  return(run_params)
} 
