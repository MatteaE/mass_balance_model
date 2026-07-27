###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to produce daily plots of SWE and surface type.     #
#                 These can be directly turned into a nice animation.                             #
###################################################################################################


func_plot_daily_maps <- function(year_data,
                                 run_params,
                                 data_dhms,
                                 data_dems,
                                 data_outlines) {
  
  
  # Plot daily maps from the winter simulation?
  if (run_params$plot_daily_maps_winter == TRUE) {
    if (year_data$process_winter == TRUE) {
      
      func_plot_daily_maps_worker(year_data,
                                  run_params,
                                  data_dhms,
                                  data_dems,
                                  data_outlines,
                                  "winter")
      
      # User asked for daily plots of the winter simulation, but none exist (no winter stakes).
    } else {
      func_customlog("Parameter plot_daily_maps_winter is TRUE, but there is no winter simulation to be plotted.", level = 1)
    }
  }
  
  
  
  # Plot daily maps from the annual simulation?
  if (run_params$plot_daily_maps_annual == TRUE) {
    
    func_plot_daily_maps_worker(year_data,
                                run_params,
                                data_dhms,
                                data_dems,
                                data_outlines,
                                "annual")
    
  }
  
  cat("\n")
  
}
