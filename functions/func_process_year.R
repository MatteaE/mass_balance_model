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
  
  # Find stake offsets on the grid.
  if (year_data$nstakes_annual > 0) {
    year_data <- func_find_stake_cells_dx_dy(year_data,
                                             data_all$data_dhms,
                                             data_all$data_dems,
                                             run_params)
  }
  
  # Setup grids from winter snow probes, if available. Also set flag year_data$process_winter to TRUE/FALSE.
  year_data <- func_setup_winter_probes_dist(year_data,
                                             data_all$data_dhms,
                                             data_all$data_dems,
                                             run_params)
  
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
    cat("* FATAL: meteo data for the current year are missing. Please check the meteo file and the first_year/last_year! Offending date:", format(offending_date, "%Y/%m/%d"), "(day-of-year:", format(offending_date, "%j)."), "\n")
    stop()
  }
  
  
  
  #### .  Setup initial snow cover from previous year or estimation ####
  year_data <- func_setup_initial_snow_cover(year_data,
                                             year_data_prev,
                                             data_all$data_dhms,
                                             data_all$data_dems,
                                             grids_snowdist_topographic,
                                             overview_annual$year_starting_swe_available,
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
                                                   run_params,
                                                   data_all$data_dems,
                                                   data_all$data_outlines)
  
  #### . Write annual model output to files ####
  overview_annual$daily_data_list <- func_write_year_output(year_data,
                                                            run_params,
                                                            data_all$data_dems,
                                                            overview_annual$daily_data_list)
  
  #### . Produce daily plots (only if asked to do so) ####
  if (run_params$plot_daily_maps) {
    func_plot_daily_maps(year_data,
                         run_params,
                         data_all$data_surftype,
                         data_all$data_dems,
                         data_all$data_outlines)
  }
  
  # Commented code below: if a stake is at the very edge of the
  # glacier, bilinear extraction of the modeled series is impossible
  # (one or more cells of the 4 neighbors are outside the glaciated area).
  # In this case, the series extracted by our vectorized bilinear filtering
  # is different from the one extracted by the raster::extract() function,
  # because that one uses some magic to replace missing values (NOT true
  # bilinear filtering at those edges), while we use a rigorous formula
  # (which would take the mass balance from the outside cell, i.e.
  # from a non-glaciated surface, which should NOT contribute to the stake!).
  # In that case the code below would find and print the discrepancy.
  # It is no longer very useful since we now switch automatically
  # to nearest glaciated neighbor for cells on the edge.
  # if (year_data$nstakes_annual > 0) {
    # stake_errors <- abs((extract(year_data$massbal_annual_maps$meas_period, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear")[,1] - year_data$massbal_annual_meas_cur$massbal_standardized) - (year_data$mod_output_annual_cur$stakes_mb_mod - year_data$mod_output_annual_cur$stakes_mb_meas))
    # max_error <- max(stake_errors)
    # max_error_id <- which.max(stake_errors)
    # if (max_error > 1) {
      # cat("* SERIOUS WARNING: the recomputed stake mass balance biases over the stake period and over the single \"measurement period\" do not match. This is likely an issue with the bilinear extraction of the stakes series. Check if there are stakes coordinates exactly aligned with cell centers or too close to the glacier edges, they are likely the cause.\n")
      # cat(paste0("The max error is at stake ", max_error_id, ", with value ", round(max_error, 1), " mm w.e.\n"))
      # cat("Stake data:", paste(year_data$massbal_annual_meas_cur[max_error_id,]), sep = "  |  ", "\n")
    # }
  # }
  
  cat("============  FINISHED simulation of year", year_data$year_cur, "  ============\n")
  
  return(list(year_data       = year_data,
              overview_annual = overview_annual))
  
}
