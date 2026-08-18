###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function (called within the main loop) to entirely       #
#                 process one year (either having or missing the mass balabnce measurements).     #
###################################################################################################  

func_process_year <- function(year_data,
                              year_data_prev,
                              run_params,
                              year_cur_params,
                              data_all,
                              grids_snowdist_topographic,
                              overview_annual) {
  
  
  # Calculate and print PDD sum for the hydrological year period.
  pdd_id1 <- which(data_all$data_weather$timestamp == year_cur_params$hydro_start)
  pdd_id2 <- which(data_all$data_weather$timestamp == (year_cur_params$hydro_end-1)) # Hydro end is already Oct 1.
  year_data$pdd_sum_hydro <- sum(pmax(0.0, data_all$data_weather$t2m_mean[pdd_id1:pdd_id2]))
  cat("PDD sum at the AWS over the hydrological year:", round(year_data$pdd_sum_hydro), "\u00B0C d\n")
  
  # Find offsets on the grid of all stakes and user-defined points of daily output.
  year_data <- func_find_mb_points_on_grid(year_data,
                                           data_all$data_dhms,
                                           data_all$data_dems,
                                           run_params)
  
  
  # Setup grids from winter snow probes, if available. Also set flag year_data$process_winter to TRUE/FALSE.
  year_data <- func_setup_winter_probes_dist(year_data,
                                             data_all$data_dhms,
                                             data_all$data_dems,
                                             data_all$data_outlines,
                                             run_params,
                                             year_cur_params)
  
  
  #### . Compute annual and winter modeling periods ####
  year_data <- func_compute_modeling_periods(year_data,
                                             run_params,
                                             year_cur_params)
  # Stop with an error in case we don't have all
  # weather data we need for the simulation period.
  model_time_bounds_range <- range(year_data$model_time_bounds, na.rm = T)
  time_bounds_match <- match(model_time_bounds_range, data_all$data_weather$timestamp)
  if (any(is.na(time_bounds_match))) {
    offending_id1 <- which(is.na(time_bounds_match))[1] # The [1] to handle the case where both simulation start and end don't have meteo data. This index then is either value 1 or 2
    offending_date <- model_time_bounds_range[offending_id1]
    func_customlog("Meteo data for the current year are missing. Please check the meteo file and the first_year/last_year!", level = 2)
    func_customlog("Offending date:", format(offending_date, "%Y/%m/%d"), "(day-of-year:", format(offending_date, "%j)."), "\n", level = 0)
    func_stop()
  }
  
  
  #### .  Setup initial snow cover from previous year or estimation ####
  year_data <- func_setup_initial_snow_cover(year_data,
                                             year_data_prev,
                                             data_all$data_dhms,
                                             data_all$data_dems,
                                             grids_snowdist_topographic,
                                             overview_annual$summary_df$year_starting_swe_available,
                                             run_params)
  
  
  #### .  Simulate winter mass balance (only if measurements available) ####
  year_data <- func_process_winter(year_data,
                                   run_params,
                                   year_cur_params,
                                   data_all$data_dhms,
                                   data_all$data_dems,
                                   data_all$data_surftype,
                                   data_all$data_radiation,
                                   data_all$data_weather)
  
  
  #### .  Simulate annual mass balance ####
  # If we have mass balance data, this runs the optimization.
  # Else just a single simulation.
  year_data <- func_process_annual(year_data,
                                   run_params,
                                   year_cur_params,
                                   data_all$data_dhms,
                                   data_all$data_dems,
                                   data_all$data_surftype,
                                   data_all$data_radiation,
                                   data_all$data_weather)
  
  # After an annual model run we have SWE information
  # suitable for use as starting condition of the next
  # year, if we want to use it.
  if (year_data$year_id < run_params$n_years) {
    overview_annual$summary_df$year_starting_swe_available[year_data$year_id+1] <- TRUE
  }
  
  
  #### . Extract mass balance results ####
  year_data <- func_extract_year_massbalance(year_data,
                                             run_params,
                                             year_cur_params,
                                             data_all$data_dhms,
                                             data_all$data_dems)
  
  
  #### . Post-process mass balance (correction in elevation bands, ELA/AAR, standardized over the measurement period) ####
  year_data <- func_massbal_postprocess(year_data,
                                        run_params,
                                        year_cur_params,
                                        data_all$data_dems)
  
  
  #### . Save to overview_annual$summary_df the overview values for the current year ####
  overview_annual$summary_df <- func_save_overview_values(year_data,
                                                          year_cur_params,
                                                          run_params,
                                                          overview_annual$summary_df)
  
  
  #### . Produce all plots for the year ####
  # This creates a PDF file for the year
  # and also adds a plot to the overview
  # plots, which are saved to PDF at the end.
  year_data$ele_bands_plot_df    <- func_plot_year(year_data,
                                                   year_cur_params,
                                                   run_params,
                                                   data_all$data_dems,
                                                   data_all$data_dhms,
                                                   data_all$data_outlines)
  
  
  #### . Write annual model output to files ####
  overview_annual$daily_data_list <- func_write_year_output(year_data,
                                                            run_params,
                                                            data_all$data_dhms,
                                                            data_all$data_dems,
                                                            overview_annual$daily_data_list)
  
  
  #### . Plot daily maps of SWE and surface type ####
  # The function checks whether it should plot anything
  # from either the annual or winter simulation, and at
  # what frequency.
  func_plot_daily_maps(year_data,
                       run_params,
                       data_all$data_dhms,
                       data_all$data_dems,
                       data_all$data_outlines)
  
  
  #### . Write daily grids of SWE and cumulative mass balance ####
  # The function checks whether it should write anything
  # from either the annual or winter simulation, and at
  # what frequency.
  func_write_daily_grids(year_data,
                         run_params,
                         data_all$data_dems)
  
  
  # Commented code below: if a stake is at the very edge of the
  # glacier, bilinear extraction of the modeled series is impossible
  # (one or more cells of the 4 neighbors are outside the glacierized area).
  # In this case, the series extracted by our vectorized bilinear filtering
  # is different from the one extracted by the raster::extract() function,
  # because that one uses some magic to replace missing values (NOT true
  # bilinear filtering at those edges), while we use a rigorous formula
  # (which would take the mass balance from the outside cell, i.e.
  # from a non-glacierized surface, which should NOT contribute to the stake!).
  # In that case the code below would find and print the discrepancy.
  # It is no longer very useful since we now switch automatically
  # to nearest glacierized neighbor for cells on the edge.
  # if (year_data$nstakes_annual > 0) {
  # stake_errors <- abs((extract(year_data$massbal_annual_maps$meas_period, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear")[,1] - year_data$massbal_annual_meas_cur$massbal_meas_standardized) - (year_data$mod_output_annual_cur$stakes_mb_mod - year_data$mod_output_annual_cur$stakes_mb_meas))
  # max_error <- max(stake_errors)
  # max_error_id <- which.max(stake_errors)
  # if (max_error > 1) {
  # func_customlog("The recomputed stake mass balance biases over the stake period and over the single \"measurement period\" do not match. This is likely an issue with the bilinear extraction of the stakes series. Check if there are stakes coordinates exactly aligned with cell centers or too close to the glacier edges, they are likely the cause.\n", level = 1)
  # cat(paste0("The max error is at stake ", max_error_id, ", with value ", round(max_error, 1), " mm w.e.\n"))
  # cat("Stake data:", paste(year_data$massbal_annual_meas_cur[max_error_id,]), sep = "  |  ", "\n")
  # }
  # }
  
  func_customlog("============  FINISHED processing of year ", year_data$year_cur, "  ============\n", level = 4)
  
  return(list(year_data       = year_data,
              overview_annual = overview_annual))
  
}
