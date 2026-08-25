###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to write daily grids of SWE and mass balance to     #
#                 geotiff, defining a low-level worker which selects the appropriate winter or    #
#                 annual data.                                                                    #
###################################################################################################


# period_sel is either "winter" or "annual".


func_write_daily_grids_worker <- function(year_data,
                                          run_params,
                                          data_dems,
                                          period_sel) {
  
  
  out_p         <- file.path(run_params$out_daily_dirpath, "gridded", year_data$year_cur, period_sel)
  out_swe_p     <- file.path(out_p, "swe_grids")
  out_massbal_p <- file.path(out_p, "massbal_grids")
  
  dir.create(out_swe_p, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_massbal_p, recursive = TRUE, showWarnings = FALSE)
  
  plot_df <- data.frame(crds(data_dems$elevation[[year_data$dem_grid_id]], na.rm = FALSE))
  
  weather_series_cur <- year_data[[paste0("weather_series_", period_sel, "_cur")]] # For the dates.
  mod_output_cur <- year_data[[paste0("mod_output_", period_sel, "_cur")]]
  model_days_n   <- year_data[[paste0("model_", period_sel, "_days_n")]]
  
  
  # If the user supplied a reference date for the mass balance grids,
  # we have to decide whether it refers to YYYY-1 or to YYYY.
  # See description of daily_massbal_winter_refdate and daily_massbal_annual_refdate in func_process_run_params().
  if (run_params[[paste0("daily_massbal_", period_sel, "_refdate")]] != "") {
    date_ref <- c(as.Date(paste0(year_data$year_cur-1, "/", run_params[[paste0("daily_massbal_", period_sel, "_refdate")]])),
                  as.Date(paste0(year_data$year_cur, "/", run_params[[paste0("daily_massbal_", period_sel, "_refdate")]])))
    if (all(is.na(date_ref))) {
      func_customlog("Wrong value for parameter ", paste0("daily_massbal_", period_sel, "_refdate"), " - please check.", level = 2)
      func_stop()
    } else {
      # In the Southern Hemisphere, reference date is always interpreted as YYYY-1.
      if (run_params$north_south == "South") {
        date_ref <- date_ref[1]
        # In the Northern Hemisphere, it is interpreted as YYYY-1 between 1 Jul and 31 Dec, otherwise YYYY.
      } else {
        if (as.integer(format(date_ref[which(!is.na(date_ref))[1]], "%m")) < 7) {
          date_ref <- date_ref[2]
        } else {
          date_ref <- date_ref[1]
        }
      }
      # This case below can only happen with leap years (if the user has
      # selected 02/29 as reference and the year does not have it).
      if (is.na(date_ref)) {
        func_customlog("Year ", year_data$year_cur, ": wrong value for parameter ", paste0("daily_massbal_", period_sel, "_refdate"), " - please check it.", level = 2)
        func_stop()
      }
    }
    
    dates_all <- c(weather_series_cur$timestamp,
                   weather_series_cur$timestamp[model_days_n] + 1)
    date_ref_id <- match(date_ref, dates_all)
    
    if (is.na(date_ref_id)) {
      func_customlog("Year ", year_data$year_cur, ": reference date ", paste0("daily_massbal_", period_sel, "_refdate"), " does not belong to the simulation period. Please check it.", level = 2)
      func_customlog("The wrong value was: ", format(date_ref, "%Y/%m/%d"), level = 0)
      func_stop()
    }
    
    cells_ref_ids <- (date_ref_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)
    massbal_ref_r <- setValues(data_dems$elevation[[year_data$dem_grid_id]],
                               mod_output_cur$vec_massbal_cumul[cells_ref_ids])
    
    
    # Else: the user did not supply a ref date.
  } else {
    
    massbal_ref_r <- setValues(data_dems$elevation[[year_data$dem_grid_id]],
                               0.0)
    
  }
  
  
  
  # Daily loop to produce the plots ---------------------------------------------------------------
  # Optionally reduced frequency (e.g. weekly).
  for (day_id in 1:(model_days_n + 1)) {
    
    # Plot only one every few days, to speed up.
    if (!(day_id %% run_params[[paste0("write_daily_grids_", period_sel, "_freq")]])) {
      
      cat("\r** Writing daily grids of SWE and cumulative mass balance from the", period_sel, "simulation...", day_id, "/", model_days_n+1, "**")
      
      date_cur <- c(weather_series_cur$timestamp,
                    weather_series_cur$timestamp[model_days_n] + 1)[day_id]
      date_cur_str <- format(date_cur, "%Y-%m-%d")
      
      cells_cur <- (day_id-1) * run_params$grid_ncells + 1:(run_params$grid_ncells)
      swe_cur_r <- setValues(data_dems$elevation[[year_data$dem_grid_id]],
                             mod_output_cur$vec_swe_all[cells_cur])
      writeRaster(swe_cur_r,
                  file.path(out_swe_p, paste0(date_cur_str, ".tif")),
                  overwrite = TRUE)
      
      # massbal_cur_r is relative to massbal_ref_r,
      # which is just 0.0 if the user did not supply a reference date.
      massbal_cur_r <- setValues(data_dems$elevation[[year_data$dem_grid_id]],
                                 mod_output_cur$vec_massbal_cumul[cells_cur]) - massbal_ref_r
      writeRaster(massbal_cur_r,
                  file.path(out_massbal_p, paste0(date_cur_str, ".tif")),
                  overwrite = TRUE)
      
    } # End selection on day_id to plot only one day every few.
    
  } # End daily loop to write SWE and mass balance grids.
  
}