###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), inspired by the IDL version by Matthias Huss.        #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the main loop and instructions.                              #
###################################################################################################

# Set English language for dates (in the plots).
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")


#### Load from input data files or reboot file ####
boot_file_write   <- FALSE                # Save .RData file with the input data, for faster reload.
boot_file_read    <- TRUE                 # Load .RData file with the input data, instead of loading input files.
boot_file_name    <- "boot_file_barkrak2.RData"    # Name of the .RData input data file.


#### Load function definitions and R modules, set params ####
source(file.path("procedures", "pro_load_libraries.R"))
invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))


#### Setup simulation ####
source(file.path("procedures", "pro_load_data_parameters.R"))    # Load input data.
# Below: remove cacheDir option to force recompilation of the C++ code (useful after changing computer or editing the source file).
if (run_params$avalanche_routine_cpp == TRUE) {sourceCpp(file.path("functions", "func_avalanche_gruber.cpp"), cacheDir = "functions")}
source(file.path("procedures", "pro_compute_grid_parameters.R")) # Set grid-dependent parameters.
source(file.path("procedures", "pro_compute_all_fixed_grids.R")) # Compute static grids.
source(file.path("procedures", "pro_save_boot_files.R"))         # Save boot files if needed.
source(file.path("procedures", "pro_setup_loop.R"))              # Prepare variables before main loop. Also create output directory.


#### Main loop ####
for (year_id in 1:run_params$n_years) {

  #### . Select current year, parameters, data ####
  year_cur <- run_params$years[year_id]
  year_cur_params <- func_load_year_params(run_params, year_cur)
  
  cat("\n\n\n\n============  STARTING NEW YEAR:", year_cur, " ============\n")
  
  source(file.path("procedures", "pro_select_year_data.R")) # Select data for the current year.
  
  source(file.path("procedures", "pro_find_stake_dxdy.R")) # Find stake offsets on the grid.
  
  source(file.path("procedures", "pro_setup_winter_probes_dist.R")) # Setup grids from winter snow probes, if available. Also set flag process_winter to TRUE/FALSE.

  #### . Compute annual and winter modeling periods ####
  model_time_bounds   <- func_compute_modeling_periods(run_params, massbal_annual_meas_cur, massbal_winter_meas_cur, year_cur, year_cur_params)
  
  #### .  Initial snow cover ####
  source(file.path("procedures", "pro_compute_initial_snow_cover.R"))
  
  #### .  Simulate winter mass balance (only if measurements available) ####
  source(file.path("procedures", "pro_process_winter.R"))
  
  #### .  Simulate annual mass balance ####
  source(file.path("procedures", "pro_process_annual.R"))
  
  #### . Extract mass balance results ####
  source(file.path("procedures", "pro_extract_massbalance.R"))
  
  #### . Post-process mass balance (correction in elevation bands, ELA/AAR, standardized over the measurement period) ####
  source(file.path("procedures", "pro_massbal_postprocess.R"))
  
  #### . Save overview values for the year ####
  source(file.path("procedures", "pro_save_overview_values.R"))
  
  #### . Produce all plots for the year ####
  source(file.path("procedures", "pro_plot_year.R"))
  
  #### . Write annual model output to files ####
  source(file.path("procedures", "pro_write_year_output.R"))

  #### . Produce daily plots (only if asked to do so) ####
  source(file.path("procedures", "pro_plot_daily_maps.R"))
  
  if (max(abs((extract(massbal_annual_maps$meas_period, cbind(massbal_annual_meas_cur$x, massbal_annual_meas_cur$y), method = "bilinear") - massbal_annual_meas_cur$massbal_standardized) - (mod_output_annual_cur$stakes_mb_mod - mod_output_annual_cur$stakes_mb_meas))) > 1) {
    stop("ERROR: the recomputed stake mass balance biases over the stake period and over the single \"measurement period\" do not match. This is likely an issue with the bilinear filtering of the stakes series. Check if there are stakes coordinates exactly aligned with cell centers or too close to the glacier edges, they are likely the cause.")
  }
  
}

#### Plot and write overview ####
source(file.path("procedures", "pro_plot_write_overview.R"))

cat("\n============  All done  ============\n")
