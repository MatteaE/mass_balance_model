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
  filepath_params <- file.path(run_params$dir_annual_params,
                               paste0(run_params$filename_params_prefix,
                                      year_data$year_cur,
                                      run_params$filename_params_suffix))
  
  if (file.exists(filepath_params)) {
    
    # Read parameter file.
    params_raw <- read.delim(filepath_params,
                             header = FALSE,
                             sep = ";",
                             comment.char = "*",
                             stringsAsFactors = FALSE,
                             strip.white = TRUE)
    
    params_available_ids <- pmatch(params_raw[,3], params_names_all)
    # Remove param ids if they don't match parameters which can be set.
    # This prevents an unhandled error in case the user supplies some
    # additional parameters which cannot be set (e.g. evaluate_snowdist
    # from the old parameter file format).
    params_available_remove <- which(is.na(params_available_ids))
    if (length(params_available_remove) > 0) {
      params_available_ids <- params_available_ids[-params_available_remove]
      params_raw <- params_raw[-params_available_remove,]
    }
    
    params_available_n   <- length(params_available_ids)
    
    # Assemble output, already converting to numeric types.
    for (param_id_raw in 1:params_available_n) {
      param_id_year_cur <- params_available_ids[param_id_raw]
      # Parameters are usually numeric, except for:
        # the temperature and precipitation gradients, which can be either 1 numeric or 12 comma-separated;
        # the elevation bands, which are always comma-separated.
      # So if we are loading the elevation bands we process them as comma-separated.
      # If we are loading the temperature/precipitation gradients, we process them
      # as either comma-separated or simple numeric, and if they are a simple numeric
      # we repeat the value 12 times.
      # For the other parameters, we simply load them as numeric.
      if (params_names_all[param_id_year_cur] == "mb_corr_ele_bands") {
        year_cur_params[[param_id_year_cur]] <- as.numeric(unlist(strsplit(params_raw[param_id_raw,1], ",")))
        
        
      } else if (params_names_all[param_id_year_cur] == "prec_elegrad") {
        if (typeof(params_raw[param_id_raw,1]) == "character") {
          val_tmp <- as.numeric(unlist(strsplit(params_raw[param_id_raw,1], ",")))
        } else {
          val_tmp <- as.numeric(params_raw[param_id_raw,1])
        }
        if (length(val_tmp) == 1) {
          year_cur_params[[param_id_year_cur]] <- rep(val_tmp, 12)
        } else if (length(val_tmp) == 12) {
          year_cur_params[[param_id_year_cur]] <- val_tmp
        } else {
          stop(paste0("Year ", year_data$year_cur, ": parameter prec_elegrad must have either 1 annual or 12 comma-separated monthly values. Value(s) provided: ", paste0(params_raw[param_id_raw,1], collapse = "")))
        }
        
        
      } else if (params_names_all[param_id_year_cur] == "temp_elegrad") {
        if (typeof(params_raw[param_id_raw,1]) == "character") {
          val_tmp <- as.numeric(unlist(strsplit(params_raw[param_id_raw,1], ",")))
        } else {
          val_tmp <- as.numeric(params_raw[param_id_raw,1])
        }
        if (length(val_tmp) == 1) {
          year_cur_params[[param_id_year_cur]] <- rep(val_tmp, 12)
        } else if (length(val_tmp) == 12) {
          year_cur_params[[param_id_year_cur]] <- val_tmp
        } else {
          stop(paste0("Year ", year_data$year_cur, ": parameter temp_elegrad must have either 1 annual or 12 comma-separated monthly values. Value(s) provided: ", paste0(params_raw[param_id_raw,1], collapse = "")))
        }
        
        
      } else {
        year_cur_params[[param_id_year_cur]] <- as.numeric(params_raw[param_id_raw,1])
      }
    }
  }
  
  return(year_cur_params)
  
}
