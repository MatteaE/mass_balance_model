###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to determine the modeling periods, both for      #
#                 the winter period and the annual model.                                         #
################################################################################################### 

# The output is a vector of four Date objects: start and end of the annual
# modeling period, and same for the winter modeling period.

# ANNUAL modeling period starts at the beginning of the observation
# period with the earliest start (i.e. the annual stake which was surveyed first
# on the previous year), but no later than Oct 1 (to include the whole hydrological year),
# and no later than the date of fixed mass balance evaluation, set by the user.
# Annual modeling period ends at the end of the observation period with the latest end
# (i.e. the annual stake which was surveyed last on the current year), but no earlier than Sep 30 (hydro year),
# and no earlier than the date of fixed mass balance evaluation, set by the user.
# WINTER modeling period starts at the beginning of the observation period with the earliest start
# (among the winter ones) and ends at the end of the period with the latest end (among the winter ones).
# It is extended to include te dates of fixed mass balance evaluation set by the user.
func_compute_modeling_periods <- function(year_data, run_params, year_cur_params) {
  
  # na.rm because we also support NA as start date, meaning
  # "end of previous ablation season" i.e. mass balance minimum.
  # NOTE: we don't change the modeling period to include stakes
  # which start at NA. So if the mass balance minimum happens
  # before Sep 30 of the previous year and there isn't a measurement
  # before that date, the starting date of those stakes will be
  # set at Sep 30.
  # We use year_cur_params$hydro_end-1 since hydro_end is Oct 1 (00:00, i.e. as.Date(Oct 1)),
  # which means we can stop modeling after the (weather series) time step of Sep 30.
  # annual_start <- min(c(year_cur_params$hydro_start, year_data$massbal_annual_meas_cur$start_date, year_cur_params$fixed_annual_start), na.rm = T)
  # annual_end   <- max(c(year_cur_params$hydro_end-1, year_data$massbal_annual_meas_cur$end_date, year_cur_params$fixed_annual_end))
  annual_start <- min(c(year_cur_params$hydro_start, year_data$massbal_annual_meas_cur$start_date, year_data$massbal_winter_meas_cur$start_date), na.rm = T)
  annual_end   <- max(c(year_cur_params$hydro_end-1, year_data$massbal_annual_meas_cur$end_date))
  
  winter_start <- NA
  winter_end   <- NA
  if (year_data$process_winter) {
    winter_start <- min(year_data$massbal_winter_meas_cur$start_date)
    winter_end   <- max(year_data$massbal_winter_meas_cur$end_date)
  }
  
  year_data$model_time_bounds <- c(annual_start, annual_end, winter_start, winter_end)
  return(year_data)
  
}
