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
                              data_dhms,
                              data_dems,
                              data_surftype,
                              data_radiation,
                              data_outlines,
                              data_weather,
                              grids_snowdist_topographic,
                              overview_annual) {
  
  # Find stake offsets on the grid.
  if (year_data$nstakes_annual > 0) {
    year_data <- func_find_stake_dxdy(year_data,
                                      data_dhms,
                                      run_params)
  }
  
  # Setup grids from winter snow probes, if available. Also set flag year_data$process_winter to TRUE/FALSE.
  year_data <- func_setup_winter_probes_dist(year_data,
                                             data_dhms,
                                             data_dems,
                                             run_params)
  
  #### . Compute annual and winter modeling periods ####
  year_data <- func_compute_modeling_periods(year_data,
                                             run_params,
                                             year_cur_params)
  
  #### .  Setup initial snow cover from previous year or estimation ####
  year_data <- func_setup_initial_snow_cover(year_data,
                                             year_data_prev,
                                             data_dhms,
                                             data_dems,
                                             grids_snowdist_topographic,
                                             overview_annual$year_starting_swe_available,
                                             run_params)
  
  #### .  Simulate winter mass balance (only if measurements available) ####
  year_data <- func_process_winter(year_data,
                                   run_params,
                                   year_cur_params,
                                   data_dhms,
                                   data_dems,
                                   data_surftype,
                                   data_radiation,
                                   data_weather)
  
  
  
  #### .  Simulate annual mass balance ####
  # If we have mass balance data, this runs the optimization.
  # Else just a single simulation.
  year_data <- func_process_annual(year_data,
                                   run_params,
                                   year_cur_params,
                                   data_dhms,
                                   data_dems,
                                   data_surftype,
                                   data_radiation,
                                   data_weather)
  
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
                                             data_dhms,
                                             data_dems)
  
  #### . Post-process mass balance (correction in elevation bands, ELA/AAR, standardized over the measurement period) ####
  year_data <- func_massbal_postprocess(year_data,
                                        run_params,
                                        year_cur_params,
                                        data_dems)
  
  #### . Save to overview_annual$summary_df the overview values for the current year ####
  overview_annual$summary_df <- func_save_overview_values(year_data,
                                                          year_cur_params,
                                                          overview_annual$summary_df)
  
  #### . Produce all plots for the year ####
  # This creates a PDF file for the year
  # and also adds a plot to the overview
  # plots, which are saved to PDF at the end.
  plot_year_result <- func_plot_year(year_data,
                                     run_params,
                                     data_dems,
                                     data_outlines,
                                     overview_annual$areaplots_list)
  
  overview_annual$areaplots_list <- plot_year_result[["areaplots_list"]]
  year_data$ele_bands_plot_df    <- plot_year_result[["ele_bands_plot_df"]]
  
  #### . Write annual model output to files ####
  overview_annual$daily_data_list <- func_write_year_output(year_data,
                                                            run_params,
                                                            data_dems,
                                                            overview_annual$daily_data_list)
  
  #### . Produce daily plots (only if asked to do so) ####
  if (run_params$plot_daily_maps) {
    func_plot_daily_maps(year_data,
                         run_params,
                         data_surftype,
                         data_dems,
                         data_outlines)
  }
  
  if ((year_data$nstakes_annual > 0) && (max(abs((extract(year_data$massbal_annual_maps$meas_period, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear") - year_data$massbal_annual_meas_cur$massbal_standardized) - (year_data$mod_output_annual_cur$stakes_mb_mod - year_data$mod_output_annual_cur$stakes_mb_meas))) > 1)) {
    stop("ERROR: the recomputed stake mass balance biases over the stake period and over the single \"measurement period\" do not match. This is likely an issue with the bilinear filtering of the stakes series. Check if there are stakes coordinates exactly aligned with cell centers or too close to the glacier edges, they are likely the cause.")
  }
  
  return(list(year_data = year_data,
              overview_annual = overview_annual))
  
}
