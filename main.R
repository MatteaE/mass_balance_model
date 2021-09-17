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


#### Set parameters, load function definitions and R modules ####
source("set_params.R")
source(file.path("procedures", "pro_load_libraries.R"))
invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))


#### Setup simulation ####
run_params <- func_process_run_params(run_params) # Process fixed run parameters, computing derived ones.
source(file.path("procedures", "pro_load_data_all.R"))    # Load input data.
# Below: remove cacheDir option to force recompilation of the C++ code (useful after changing computer or editing the source file).
if (run_params$avalanche_routine_cpp == TRUE) {sourceCpp(file.path("functions", "func_avalanche_gruber.cpp"), cacheDir = "functions")}

# Compute global grid parameters (numbers of cells and cell size).
run_params <- func_compute_grid_parameters(run_params, data_dhms)

# Compute fixed grids (avalanches, topographic snow distribution, variable ice albedo).
grids_fixed_list <- func_compute_all_fixed_grids(run_params, data_dhms, data_dems)

source(file.path("procedures", "pro_save_boot_files.R"))         # Save boot files if needed.

# Setup list with annual values and plots (1 per year).
overview_annual <- func_setup_overview_annual(run_params)

# Create output directory.
dir.create(file.path(run_params$output_dirname, "annual_results"), recursive = TRUE, showWarnings = FALSE)

#### Main loop ####
# Here year_data is a list which is gradually built and
# modified during one iteration of the main loop.
year_data <- list()
for (year_id in 1:run_params$n_years) {

  #### . Select current year, parameters, data ####
  # Select data from the current year.
  # NOTE: list year_data contains the indices of the
  # data grids, not copies of the grids themselves.
  year_data_prev <- year_data # Save a copy, we need it e.g. to model based on the previous year's result.
  year_data <- func_select_year_data(data_dhms, data_dems, data_surftype, data_outlines,
                                     grids_fixed_list,
                                     data_massbalance_annual, data_massbalance_winter,
                                     year_id, run_params)
  
  if (year_data$nstakes_annual > 0) {
    
    cat("\n\n\n\n============  STARTING NEW YEAR:", year_data$year_cur, " ============\n")
    
    year_cur_params <- func_set_year_params(year_data, run_params)

    year_results_list <- func_process_year(year_data,
                                           year_data_prev,
                                           run_params,
                                           year_cur_params,
                                           data_dhms,
                                           data_dems,
                                           data_surftype,
                                           data_radiation,
                                           data_outlines,
                                           data_weather,
                                           grids_fixed_list$grids_snowdist_topographic,
                                           overview_annual)
    year_data         <- year_results_list$year_data
    overview_annual   <- year_results_list$overview_annual
    
  } else {
    cat("\n\n============  DEFERRING simulation of year", paste0(year_data$year_cur, ", which has no mass balance measurements... ============\n"))
  }
}

message("\n** Finished simulation of all years with mass balance measurements. **")

# Here: compute mean of optimized parameters, to use on nodata years.
run_params <- func_compute_mean_optimized_params(run_params, overview_annual)

# Check if there are any years without mass balance
# measurements, these are still not simulated.
year_ids_todo <- which(!overview_annual$summary_df$year_has_data)
years_todo_n  <- length(year_ids_todo)
if (length(year_ids_todo) > 0) {
  
  message("\n** Processing ", years_todo_n, " year(s) without mass balance measurements... **")
  
  for (year_id in year_ids_todo) {
    
    #### . Select current year, parameters, data ####
    # Select data from the current year.
    # NOTE: list year_data contains the indices of the
    # data grids, not copies of the grids themselves.
    year_data_prev <- year_data # Save a copy, we need it e.g. to model based on the previous year's result.
    year_data <- func_select_year_data(data_dhms, data_dems, data_surftype, data_outlines,
                                       grids_fixed_list,
                                       data_massbalance_annual, data_massbalance_winter,
                                       year_id, run_params)
    
    cat("\n\n\n\n============  STARTING NEW YEAR:", year_data$year_cur, " ============\n")
    
    year_cur_params <- func_set_year_params(year_data, run_params)
    
    year_results_list <- func_process_year(year_data,
                                           year_data_prev,
                                           run_params,
                                           year_cur_params,
                                           data_dhms,
                                           data_dems,
                                           data_surftype,
                                           data_radiation,
                                           data_outlines,
                                           data_weather,
                                           grids_fixed_list$grids_snowdist_topographic,
                                           overview_annual)
    year_data         <- year_results_list$year_data
    overview_annual   <- year_results_list$overview_annual
  }
}

message("\n** All simulation loops have finished. **")

#### Plot and write overview ####
func_plot_write_overview(overview_annual,
                         run_params)

message("\n============  All done! Model has finished succesfully.  ============\n")
