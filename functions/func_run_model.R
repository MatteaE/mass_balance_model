###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to run the entire model.                        #
################################################################################################### 

func_run_model <- function(run_params) {
  
  # Model version.
  run_params$dmbsim_version <<- "3.0"
  
  # Character vectors with all emitted warnings and errors.
  # They are updated by func_customlog() and used
  # by func_end_dialog() to show relevant information.
  warnings_char <<- NULL
  fatal_char    <<- NULL
  
  
  # Close any leftover sinks and connections from previous runs.
  while (sink.number() > 0) {
    sink()
  }
  while (nrow(showConnections(all = FALSE)) > 0) {
    close(getConnection(rownames(showConnections(all = FALSE))))
  }
  
  
  # Set main output directory, where the output and logs will be stored.
  run_params$output_dirname <- file.path("output", run_params$name_glacier)
  run_params$dir_output_logs <- file.path(run_params$output_dirname, "logs")
  dir.create(run_params$dir_output_logs, showWarnings = FALSE, recursive = TRUE)
  
  
  # Start logger ------------------------------------------------------------------------------------
  logfile <<- file.path(run_params$dir_output_logs, paste0("mb_model_run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  logcon  <<- file(logfile, open = "a") # Assigned globally to be available to func_customlog()
  
  # Setup sink for split logging (console + logfile).
  sink(logcon, split = TRUE)
  options(warn=1) # Configure immediate printing of warnings.
  
  func_customlog("|++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|")
  func_customlog("|++++++++++++++++                          ++++++++++++++++|")
  func_customlog("|+++++++++               DMBSim v", run_params$dmbsim_version, "              +++++++++|")
  func_customlog("|++++++++++++++++                          ++++++++++++++++|")
  func_customlog("|++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|\n\n")
  
  start_t <- Sys.time()
  cat("Run started at", format(start_t), paste0("(", Sys.timezone(), ")"), "\n\n")
  
  cat("System info:")
  print(R.version)
  cat("\n")
  
  
  # Output directory already exists (we are putting the logs in it).
  # If it already has stuff in it beside logs/, stop with
  # error unless run_params$overwrite_output is TRUE,
  # in which case overwrite with a warning.
  output_lf <- list.files(run_params$output_dirname)
  if (length(setdiff(output_lf, "logs")) > 0) {
    if (!is.null(run_params$overwrite_output) && (run_params$overwrite_output == FALSE)) {
      func_customlog("Output destination already exists. Please move, remove or rename it before running the model.", level = 2)
      fsm()
    } else {
      func_customlog("Output destination already exists. Old files will be overwritten.", level = 1)
    }
  }
  
  
  # Setup simulation ------------------------------------------------------------------------------
  
  # Set English language for dates (in the plots).
  if (Sys.info()["sysname"] == "Windows") {
    Sys.setlocale(category = "LC_TIME", locale = "English")
  } else {
    Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
  }
  
  
  # Load required R packages
  packages_loaded <- func_load_packages(run_params)
  if (packages_loaded == FALSE) {
    func_stop()
  }
  
  # Process fixed run parameters, setting values which the user
  # has not supplied and computing derived parameters.
  run_params <- func_process_run_params(run_params)
  
  # Load all input data
  cat("\n")
  func_customlog("Loading all input data...", level = 4)
  data_all   <- func_load_data_all(run_params)
  func_customlog("Finished loading all input data.", level = 4)
  cat("\n")
  
  
  func_customlog("Setting up the model.", level = 4)
  
  # Find out whether we are North or South of the Equator.
  # This is used in multi-annual runs for the firnification routine,
  # which is called on April 1 in the Northern hemisphere and on
  # October 1 in the Southern one.
  ext_cur     <- ext(data_all$data_dems$elevation[[1]])[1:4]
  crds_center <- cbind(mean(ext_cur[1:2]), mean(ext_cur[3:4]))
  lat_center  <- terra::project(crds_center, run_params$grids_crs_epsg, "EPSG:4326")[,2]
  if (lat_center >= 0) {
    run_params$north_south <- "North"
    run_params$firnification_date <- "04/01"
  } else {
    run_params$north_south <- "South"
    run_params$firnification_date <- "10/01"
  }
  
  
  # Below: remove cacheDir option to force recompilation of the C++ code (useful after changing computer or editing the source file).
  if (run_params$avalanche_routine_cpp == TRUE) {
    sourceCpp(file.path("functions", "func_avalanche_gruber.cpp"), cacheDir = "functions")
  }
  
  # Source C++ definition of fourCellsFromXY,
  # which we have taken from the raster package
  # since it is not yet implemented in the terra package.
  sourceCpp(file.path("functions", "func_four_cells_from_xy.cpp"), cacheDir = "functions")
  
  
  # Compute and apply multiplier for color scale of mass balance maps.
  if (is.na(run_params$mb_colorscale_multiplier)) {
    run_params$mb_colorscale_multiplier <- func_compute_massbal_colorscale_multiplier(data_all$data_massbalance_annual,
                                                                                      data_all$data_dems,
                                                                                      data_all$data_weather,
                                                                                      run_params)
  }
  run_params$mb_colorscale_breaks <- run_params$mb_colorscale_breaks * run_params$mb_colorscale_multiplier
  
  # Compute global grid parameters (numbers of cells and cell size).
  run_params <- func_compute_grid_parameters(run_params, data_all$data_dhms)
  
  # Estimate (if missing) three parameters which depend on the DEM:
  # weather_max_precip_ele, elevation_effect_threshold and initial_snowline_elevation.
  run_params <- func_compute_altitude_params(run_params, data_all$data_dems)
  
  # Estimate (if missing) the max avalanche deposition (kg m-2),
  # it depends somewhat on the amounts of accumulation.
  if (is.na(run_params$deposition_mass_lim)) {
    run_params <- func_compute_deposition_lim(run_params, data_all$data_dems, data_all$data_weather)
  }
  
  # Compute static grids (avalanches, topographic snow distribution, variable ice albedo).
  grids_static_list <- func_compute_all_static_grids(run_params, data_all$data_dhms, data_all$data_dems)
  
  # Setup list with annual values and plots (1 per year).
  overview_annual   <- func_setup_overview_annual(run_params)
  
  # Create output directory for annual results.
  dir.create(file.path(run_params$output_dirname, "annual_results"), recursive = TRUE, showWarnings = FALSE)
  
  func_customlog("Finished model setup.", level = 4)
  
  # Main loop -------------------------------------------------------------------------------------
  # Here year_data is a list which is gradually built and
  # modified during one iteration of the main loop.
  year_data <- list()
  
  cat("\n")
  func_customlog("Entering first loop over the years: processing only years with mass balance data", level = 4)
  
  for (year_id in 1:run_params$n_years) {
    
    cat("\n\n")
    func_customlog("Year ", year_id, " out of ", run_params$n_years, ": ", run_params$years[year_id], level = 4)
    
    
    # . Select current year, parameters, data -----------------------------------------------------
    # Select data from the current year.
    # NOTE: list year_data contains the indices of the
    # data grids, not copies of the grids themselves.
    year_data_prev <- year_data # Save a copy, we need it e.g. to model based on the previous year's result.
    year_data <- func_select_year_data(data_all,
                                       grids_static_list,
                                       year_id,
                                       run_params)
    
    cat("Input data of the current year were loaded successfully.\n")
    
    if (year_data$nstakes_annual > 0) {
      
      cat("\n")
      func_customlog("============  STARTING processing of year ", year_data$year_cur, " ============\n", level = 4)
      
      year_cur_params   <- func_set_year_params(year_data, run_params)
      year_results_list <- func_process_year(year_data,
                                             year_data_prev,
                                             run_params,
                                             year_cur_params,
                                             data_all,
                                             grids_static_list$grids_snowdist_topographic,
                                             overview_annual)
      year_data         <- year_results_list$year_data
      overview_annual   <- year_results_list$overview_annual
      
    } else {
      cat("\n")
      func_customlog("============  DEFERRING processing of year ", paste0(year_data$year_cur, ", because it has no mass balance measurements. ============\n"), level = 4)
    }
  }
  
  cat("\n")
  func_customlog("Finished processing of all years with mass balance measurements", level = 4)
  
  # Here: compute mean of optimized parameters, to use on nodata years.
  run_params <- func_compute_mean_optimized_params(run_params, overview_annual)
  
  # Check if there are any years without mass balance
  # measurements, these are still not simulated.
  year_ids_todo <- which(!overview_annual$summary_df$year_has_data)
  years_todo_n  <- length(year_ids_todo)
  if (length(year_ids_todo) > 0) {
    
    cat("\n")
    func_customlog("There are still ", years_todo_n, " year(s) without mass balance measurements. Entering second processing loop\n", level = 4)
    cat("\n")
    
    # Loop over the years without data ------------------------------------------------------------
    for (year_id_id in 1:length(year_ids_todo)) {
      
      year_id <- year_ids_todo[year_id_id]
      cat("\n")
      func_customlog("Year ", year_id_id, " out of ", length(year_ids_todo), ": ", run_params$years[year_id], level = 4)
      
      
      # . Select current year, parameters, data ---------------------------------------------------
      # Select data from the current year.
      # NOTE: list year_data contains the indices of the
      # data grids, not copies of the grids themselves.
      year_data_prev <- year_data # Save a copy, we need it e.g. to model based on the previous year's result.
      year_data <- func_select_year_data(data_all,
                                         grids_static_list,
                                         year_id,
                                         run_params)
      
      cat("Input data of the current year were loaded successfully.\n")
      cat("\n")
      func_customlog("============  STARTING processing of year ", year_data$year_cur, " ============\n", level = 4)
      
      year_cur_params   <- func_set_year_params(year_data, run_params)
      year_results_list <- func_process_year(year_data,
                                             year_data_prev,
                                             run_params,
                                             year_cur_params,
                                             data_all,
                                             grids_static_list$grids_snowdist_topographic,
                                             overview_annual)
      year_data         <- year_results_list$year_data
      overview_annual   <- year_results_list$overview_annual
      
    }
    
    cat("\n")
    func_customlog("Finished processing of all years without mass balance measurements", level = 4)
  }
  
  func_customlog("All processing loops have finished", level = 4)
  
  # Plot and write overview -----------------------------------------------------------------------
  overview_annual$data_weather <- data_all$data_weather
  func_plot_write_overview(overview_annual,
                           run_params)
  
  if (run_params$save_simulation_RData == TRUE) {
    cat("\n** Saving entire simulation output to file model_output.RData... **\n")
    save(list = ls(all.names = TRUE), file = "model_output.RData", envir = environment())
    
  }
  
  cat("\n\n")
  gc_res <- gc(verbose = FALSE)
  end_t <- Sys.time()
  func_customlog("Run finished succesfully at ", format(Sys.time()), paste0(" (", Sys.timezone(), ")"), level = 3)
  cat("\n")
  elapsed_t <- as.numeric(difftime(end_t, start_t, units = "secs"))
  elapsed_str <- sprintf("%02d:%02d:%02d", elapsed_t %/% 3600, elapsed_t %% 3600 %/% 60,  elapsed_t %% 60 %/% 1)
  func_customlog("Elapsed time = ", elapsed_str, level = 4)
  func_customlog("Max memory used = ", round(gc_res[1,6] + gc_res[2,6]), " MB", level = 4)
  cat("\n\n")
  
  
  # Stop logger -------------------------------------------------------------------------------------
  sink()
  close(logcon)
  
  notify("Run finished successfully ✅",
         title = paste0("DMBSim ", run_params$dmbsim_version),
         image = normalizePath("icons/icon64.png"))
  
  
  
  # Show modal dialog -----------------------------------------------------------------------------
  if (rstudioapi::isAvailable()) {
    func_end_dialog(run_params,
                    logfile,
                    exit_state = "success")
  }
  
  return(0)
  
}
