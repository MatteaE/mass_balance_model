###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a function to call stop() with a custom message.             #
###################################################################################################



func_stop <- function() {
  # Write end time to log file --------------------------------------------------------------------
  t_end <- Sys.time()
  
  if (tryCatch({
    isOpen(logcon)
  },
  error = function(e) {
    FALSE
  })) {
    flush(logcon)
    sink()
    writeLines(paste0("Run failed at ", format(t_end), " (", Sys.timezone(), ")", "\n"), con = logcon, sep = "")
    flush(logcon)
    close(logcon)
  }
  
  
  # Send notification -----------------------------------------------------------------------------
  notify("Run failed ❌", 
         title = paste0("DMBSim ", run_params$dmbsim_version),
         image = normalizePath("icons/icon64.png"))
  
  
  # Show modal dialog -----------------------------------------------------------------------------
  if (rstudioapi::isAvailable()) {
    func_end_dialog(run_params,
                    logfile,
                    exit_state = "failure")
  }
  
  stop(paste0("\rRun failed at ", format(t_end), " (", Sys.timezone(), ")", "\n"))
  
}
