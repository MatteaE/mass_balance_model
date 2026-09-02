###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to write daily grids of SWE and mass balance to     #
#                 geotiff, calling a worker which uses the same code for winter and annual        #
#                 results.                                                                        #
###################################################################################################


func_write_daily_grids <- function(year_data,
                                   run_params,
                                   data_dems) {
  
  
  # Write daily SWE and mass balance grids from the winter simulation.
  if (run_params$write_daily_grids_winter) {
    
    # Is there a winter simulation to write?
    if (year_data$process_winter) {
      
      func_write_daily_grids_worker(year_data,
                                    run_params,
                                    data_dems,
                                    "winter")
      cat("\n")
      
    } else {
      
      func_customlog("Parameter write_daily_grids_winter is TRUE, but the current year has no winter-only processing - there is nothing to write.", level = 1)
      
    }
  } # End else cannot write winter grids.
  
  
  
  # Write daily SWE grids from the annual simulation.
  if (run_params$write_daily_grids_annual) {
    
    func_write_daily_grids_worker(year_data,
                                  run_params,
                                  data_dems,
                                  "annual")
    cat("\n")
  } # End if write annual grids.
  
  cat("\n")  
}
