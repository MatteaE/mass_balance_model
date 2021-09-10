###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), inspired by the IDL version by Matthias Huss.        #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the main loop and instructions.                              #
###################################################################################################

message("\n\n|++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|")
message("|++++++++++++++++                          ++++++++++++++++|")
message("|+++++++++               DMBSim v1.0              +++++++++|")
message("|++++++++++++++++                          ++++++++++++++++|")
message("|++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|\n\n")

# Set English language for dates (in the plots).
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")


#### Load from input data files or reboot file ####
boot_file_write   <- FALSE                # Save .RData file with the input data, for faster reload.
boot_file_read    <- FALSE                 # Load .RData file with the input data, instead of loading input files.
boot_file_name    <- "boot_file_barkrak.RData"    # Name of the .RData input data file.


#### Load function definitions and R modules, set params ####
source(file.path("procedures", "pro_load_libraries.R"))
invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))


#### Setup simulation ####
source(file.path("procedures", "pro_load_data_all.R"))    # Load input data.
# Below: remove cacheDir option to force recompilation of the C++ code (useful after changing computer or editing the source file).
if (run_params$avalanche_routine_cpp == TRUE) {sourceCpp(file.path("functions", "func_avalanche_gruber.cpp"), cacheDir = "functions")}
source(file.path("procedures", "pro_compute_grid_parameters.R")) # Set grid-dependent parameters.
source(file.path("procedures", "pro_compute_all_fixed_grids.R")) # Compute static grids.
source(file.path("procedures", "pro_save_boot_files.R"))         # Save boot files if needed.
source(file.path("procedures", "pro_setup_loop.R"))              # Prepare variables before main loop. Also create output directory.


#### Main loop ####
# Here year_data is a list which is gradually built and
# modified during one iteration of the main loop.
year_data <- list()
for (year_id in 1:run_params$n_years) {

  #### . Select current year, parameters, data ####
  year_cur <- run_params$years[year_id]
  year_cur_params <- func_load_year_params(run_params, year_cur)
  
  cat("\n\n\n\n============  STARTING NEW YEAR:", year_cur, " ============\n")
  
  # Select data from the current year.
  # NOTE: year data contains indices of the
  # data grids, not copies of the grids themselves.
  year_data_prev <- year_data # Save a copy, we need it e.g. to model starting from the previous year's result.
  year_data <- func_select_year_data(data_dhms, data_dems, data_surftype, data_outlines,
                                     grids_avalanche, grids_snowdist_topographic, grids_ice_albedo_fact,
                                     data_massbalance_annual, data_massbalance_winter,
                                     year_id, run_params)

  # Find stake offsets on the grid.
  year_data <- func_find_stake_dxdy(year_data,
                                    data_dhms,
                                    run_params)
  
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
                                             swe_prev_available,
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
  if (year_id < run_params$n_years) {
    swe_prev_available[year_id+1] <- TRUE
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
  
  #### . Save to df_overview the overview values for the current year ####
  df_overview <- func_save_overview_values(year_data,
                                           df_overview)
  
  #### . Produce all plots for the year ####
  # This creates a PDF file for the year
  # and also adds a plot to the overview
  # plots, which are saved to PDF at the end.
  plot_year_result <- func_plot_year(year_data,
                                     run_params,
                                     data_dems,
                                     data_outlines,
                                     overview_areaplots)
  
  overview_areaplots          <- plot_year_result[["overview_areaplots"]]
  year_data$ele_bands_plot_df <- plot_year_result[["ele_bands_plot_df"]]

  #### . Write annual model output to files ####
  overview_daily_data <- func_write_year_output(year_data,
                                                run_params,
                                                data_dems,
                                                overview_daily_data)
  

  #### . Produce daily plots (only if asked to do so) ####
  if (run_params$plot_daily_maps) {
    func_plot_daily_maps(year_data,
                         run_params,
                         data_surftype,
                         data_dems,
                         data_outlines)
  }
  
  if (max(abs((extract(year_data$massbal_annual_maps$meas_period, cbind(year_data$massbal_annual_meas_cur$x, year_data$massbal_annual_meas_cur$y), method = "bilinear") - year_data$massbal_annual_meas_cur$massbal_standardized) - (year_data$mod_output_annual_cur$stakes_mb_mod - year_data$mod_output_annual_cur$stakes_mb_meas))) > 1) {
    stop("ERROR: the recomputed stake mass balance biases over the stake period and over the single \"measurement period\" do not match. This is likely an issue with the bilinear filtering of the stakes series. Check if there are stakes coordinates exactly aligned with cell centers or too close to the glacier edges, they are likely the cause.")
  }
  
}

cat("\n** Main loop has finished. **\n")
#### Plot and write overview ####
func_plot_write_overview(df_overview,
                         overview_daily_data,
                         run_params)


message("\n============  All done! Model has finished succesfully.  ============\n")
