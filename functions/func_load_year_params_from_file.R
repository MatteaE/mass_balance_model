###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the annual parameter loading from file.                      #
###################################################################################################


# Algorithm:
# Check that file exists.
# If not: return untouched year_cur_params.
# If yes:
# - read.delim (;) file.
# - match 3rd column to parameter names
# - for all found matches, assign numeric value to corresponding item of year_cur_params
# - parameters which are not assigned a value remain NA. Also malformed elevation bands directly become NA.
func_load_year_params_from_file <- function(year_data,
                                            year_cur_params,
                                            params_names_all,
                                            run_params) {
  
  # Path to annual parameters file.
  filepath_params <- file.path(run_params$dir_data_params,
                               paste0(run_params$filename_params_prefix,
                                      year_data$year_cur,
                                      run_params$filename_params_suffix))
  
  if (file.exists(filepath_params)) {
    
    cat("Reading year-specific parameters...\n")
    
    # Read parameter file.
    params_raw <- read.delim(filepath_params,
                             header = FALSE,
                             sep = ";",
                             comment.char = "*",
                             stringsAsFactors = FALSE,
                             strip.white = TRUE,
                             col.names = paste0("V", 1:4)) # Specifying col.names enables graceful failure when the params file has no actual contents.
    
    if (any(duplicated(params_raw[,3]))) {
      func_customlog("Year ", year_data$year_cur, ": found duplicated parameter names in the params file. Please fix.", level = 2)
      func_stop()
    }
    
    params_available_ids <- match(params_raw[,3], params_names_all)
    
    
    # Remove param ids if they don't match parameters which can be set.
    # This prevents an unhandled error in case the user supplies some
    # additional parameters which cannot be set (e.g. evaluate_snowdist
    # from the old parameter file format).
    params_available_remove <- which(is.na(params_available_ids))
    if (length(params_available_remove) > 0) {
      func_customlog("Year ", year_data$year_cur, ": dropping ", length(params_available_remove), " file-based parameter(s) with unknown name.", level = 1)
      func_customlog("They are: ", paste0(params_raw[params_available_remove,3], collapse = ", "), level = 0)
      cat("\n")
      params_available_ids <- params_available_ids[-params_available_remove]
      params_raw <- params_raw[-params_available_remove,]
    }
    
    params_available_n   <- length(params_available_ids)
    
    # Is any parameter available after removing unknown ones?
    if (params_available_n > 0) {
      cat("Year ", year_data$year_cur, ": found", params_available_n, "defined year-specific parameter(s):", paste0(params_names_all[params_available_ids], sep = ""), "\n")
      
      # Assemble output, already converting to numeric types.
      for (param_id_raw in 1:params_available_n) {
        param_id_year_cur <- params_available_ids[param_id_raw]
        # Parameters are usually numeric, except for:
        # the temperature and precipitation gradients and the summer precipitation multiplier, which can be either 1 numeric or 12 comma-separated;
        # the elevation bands, which are always comma-separated.
        # So if we are loading the elevation bands we process them as comma-separated.
        # If we are loading the temperature/precipitation gradients, we process them
        # as either comma-separated or simple numeric, and if they are a simple numeric
        # we repeat the value 12 times.
        # For the other parameters, we simply load them as numeric.
        
        
        # Parameter mb_corr_ele_bands -------------------------------------------------------------
        # It must be a character (2 or more comma-separated numbers).
        if (params_names_all[param_id_year_cur] == "mb_corr_ele_bands") {
          
          if (typeof(params_raw[param_id_raw,1]) != "character") {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter mb_corr_ele_bands is malformed. Please fix it.", level = 2)
            func_customlog(paste0("Value(s) provided: ", params_raw[param_id_raw,1]), level = 0)
            func_stop()
          }
          
          val_tmp <- as.numeric(unlist(strsplit(params_raw[param_id_raw,1], ",")))
          if (any(is.na(val_tmp)) || (length(val_tmp) < 2)) {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter mb_corr_ele_bands is malformed. Please fix it.", level = 2)
            func_customlog(paste0("Value(s) provided: ", params_raw[param_id_raw,1]), level = 0)
            func_stop()
          }
          if (any(diff(val_tmp) < 10)) {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter mb_corr_ele_bands has inadequate values. Please fix it (expected: ascending order, bands > 10 m wide).", level = 2)
            func_customlog(paste0("Value(s) provided: ", params_raw[param_id_raw,1]), level = 0)
            func_stop()
          }
          if ((max(val_tmp) < min(year_data$massbal_annual_meas_cur$z_dem)) || (min(val_tmp) > max(year_data$massbal_annual_meas_cur$z_dem))) {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter mb_corr_ele_bands has inadequate values. All correction bands are outside the altitudinal range of the annual mass balance measurements. Please fix.", level = 2)
            func_customlog(paste0("Value(s) provided: ", params_raw[param_id_raw,1]), level = 0)
            func_stop()
          }
          
          # If we have made it to here, we have meaningful correction bands.
          year_cur_params[[param_id_year_cur]] <- val_tmp
          
          
          
          # Parameter prec_summer_fact ------------------------------------------------------------
          # It can be numeric or character (12 comma-separated numbers),
          # if numeric it has a special processing.
        } else if (params_names_all[param_id_year_cur] == "prec_summer_fact") {
          if (typeof(params_raw[param_id_raw,1]) == "character") {
            val_tmp <- as.numeric(unlist(strsplit(params_raw[param_id_raw,1], ",")))
          } else {
            val_tmp <- as.numeric(params_raw[param_id_raw,1])
          }
          if (any(is.na(val_tmp))) {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter prec_summer_fact is malformed. Please fix it.", level = 2)
            func_customlog("Value(s) provided: ", params_raw[param_id_raw,1], level = 0)
            func_stop()
          }
          
          # If a single value is provided, it is applied by default:
          # from May to September (Northern Hemisphere),
          # from November to March (Southern Hemisphere)
          if (length(val_tmp) == 1) {
            if (run_params$north_south == "North") {
              year_cur_params[[param_id_year_cur]] <- c(rep(1.0, 4),
                                                        rep(val_tmp, 5),
                                                        rep(1.0, 3))
            } else {
              year_cur_params[[param_id_year_cur]] <- c(rep(val_tmp, 3),
                                                        rep(1.0, 7),
                                                        rep(val_tmp, 2))
            }
            # Otherwise use all the provided monthly values
          } else if (length(val_tmp) == 12) {
            year_cur_params[[param_id_year_cur]] <- val_tmp
          } else {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter prec_summer_fact must have either 1 annual or 12 comma-separated monthly values.", level = 2)
            func_customlog("Value(s) provided: ", params_raw[param_id_raw,1], level = 0)
            func_stop()
          }
          
          
          
          # Parameters temp_elegrad and prec_elegrad ----------------------------------------------
          # They can be numeric or character (12 comma-separated values).
        } else if (params_names_all[param_id_year_cur] %in% c("temp_elegrad", "prec_elegrad")) {
          if (typeof(params_raw[param_id_raw,1]) == "character") {
            val_tmp <- as.numeric(unlist(strsplit(params_raw[param_id_raw,1], ",")))
          } else {
            val_tmp <- as.numeric(params_raw[param_id_raw,1])
          }
          if (any(is.na(val_tmp))) {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter ", params_names_all[param_id_year_cur], " is malformed. Please fix it.", level = 2)
            func_customlog("Value(s) provided: ", params_raw[param_id_raw,1], level = 0)
            func_stop()
          }
          
          if (length(val_tmp) == 1) {
            year_cur_params[[param_id_year_cur]] <- rep(val_tmp, 12)
          } else if (length(val_tmp) == 12) {
            year_cur_params[[param_id_year_cur]] <- val_tmp
          } else {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter ", params_names_all[param_id_year_cur], " must have either 1 annual or 12 comma-separated monthly values.", level = 2)
            func_customlog("Value(s) provided: ", params_raw[param_id_raw,1], level = 0)
            func_stop()
          }
          
          
          
          # Parameter probes_snowdist_filename ----------------------------------------------------
          # It is a character.
        } else if (params_names_all[param_id_year_cur] == "probes_snowdist_filename") {
          year_cur_params[[param_id_year_cur]] <- as.character(params_raw[param_id_raw,1])
          
          
          
          # All other parameters  -----------------------------------------------------------------
          # Each must be a single numeric.
        } else {
          val_tmp <- as.numeric(params_raw[param_id_raw,1])
          if (is.na(val_tmp)) {
            func_customlog("Year ", year_data$year_cur, ": file-based parameter ", params_names_all[param_id_year_cur], " is malformed, please fix it and run again.", level = 2)
            func_customlog("Value(s) provided: ", params_raw[param_id_raw,1], level = 0)
            func_stop()
          }
          year_cur_params[[param_id_year_cur]] <- val_tmp
        }
      }
      
      # Else: params_available_n is not > 0
    } else {
      
      func_customlog("Year ", year_data$year_cur, ": params file was specified, but no valid parameter was found within it. Will use default values.", level = 1)
      
    }
    
    # Else: no params file found for the current year.
  } else {
    
    cat(paste0("Year ", year_data$year_cur, ": no year-specific parameters defined\n"))
    
  }
  
  return(year_cur_params)
  
}
