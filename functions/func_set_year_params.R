###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to set the current year's parameters.            #
#                 We attempt to load these from file, then we set the remaining ones with         #
#                 either the optimized best parameters or the run_params defaults.                #
###################################################################################################

func_set_year_params <- function(year_data,
                                 run_params) {
  
  # All possible year parameters.
  params_names_all <- c("prec_corr",
                        "prec_elegrad",
                        "prec_summer_fact",
                        "temp_elegrad",
                        "melt_factor",
                        "rad_fact_ice",
                        "rad_fact_snow",
                        "mb_corr_ele_bands")
  
  # Year parameters possibly subject to optimization.
  params_names_optim <- c("prec_corr",
                          "melt_factor",
                          "rad_fact_ice",
                          "rad_fact_snow")
  
  # Empty list which we are going to fill with the parameter values.
  year_cur_params <- setNames(as.list(rep(NA_real_, length(params_names_all))), params_names_all)

  # Try to load parameters from file.
  # This leaves NA for all parameters which we don't manage to set like this.
  year_cur_params <- func_load_year_params_from_file(year_data,
                                                     year_cur_params,
                                                     params_names_all,
                                                     run_params)
  
  # Now find and fill in any remaining NA parameters.
  # Algorithm:
  # - If year has mass balance data, set all missing parameters to run_params defaults.
  # -- If elevation bands are missing, compute them from stakes elevations and glacier highest/lowest points.
  # - Else if year has no mass balance data,
  # -- Take from the defaults (if needed) missing parameters prec_summer_fact, prec_elegrad, temp_elegrad; compute mb_corr_ele_bands from stakes elevations and glacier highest/lowest points.
  # -- For the other 4 parameters (only the missing ones among them) look at run_params$nodata_years_automatic.
  # --- If FALSE, take defaults from run_params. If TRUE, take optimized values from a new 4-member df computed from the overview.
  
  year_cur_params_na_ids   <- as.integer(which(is.na(year_cur_params)))
  year_cur_params_na_names <- names(year_cur_params)[year_cur_params_na_ids]
  year_cur_params_na_n     <- length(year_cur_params_na_ids)
  if (year_cur_params_na_n > 0) {
    
    # Year parameters filling in case the year does have some mass balance measurements.
    if (year_data$nstakes_annual > 0) {
      
      for (param_id in year_cur_params_na_ids) {
        if (params_names_all[param_id] != "mb_corr_ele_bands") {
          year_cur_params[[param_id]] <- run_params[[paste0("default_", params_names_all[param_id])]]
        } else {
          year_cur_params[[param_id]] <- func_compute_ele_bands_from_stakes(year_data$massbal_annual_meas_cur$z_dem)
        }
      }
      
      # Year parameters filling in case the year has no mass balance measurements.
    } else {
      for (param_id in year_cur_params_na_ids) {
        # The four parameters not subject to optimization, take defaults from run_params.
        if (!(params_names_all[param_id] %in% params_names_optim)) {
          year_cur_params[[param_id]] <- run_params[[paste0("default_", params_names_all[param_id])]]
          # Correction bands, computed on the fly.
        } else if (params_names_all[param_id] == "mb_corr_ele_bands") {
          year_cur_params[[param_id]] <- func_compute_ele_bands_from_stakes(year_data$massbal_annual_meas_cur$z_dem)
          # Parameters subject to optimization, look at whether we want them
          # with default values or automatically optimized.
          # Also deal with the possibility that the automatically optimized
          # are missing (NaN, in case there is no mass balance measurement ever).
        } else {
          if ((run_params$nodata_years_automatic == TRUE) && (!is.na(run_params[[paste0("mean_", params_names_all[param_id])]]))) {
            year_cur_params[[param_id]] <- run_params[[paste0("mean_", params_names_all[param_id])]]
          } else {
            year_cur_params[[param_id]] <- run_params[[paste0("default_", params_names_all[param_id])]]
          }
        }
      }
    }
  }
  

  
  # Compute derived parameters.
  year_cur_params <- func_compute_derived_year_params(year_data, year_cur_params)
  
  return(year_cur_params)
  
}
