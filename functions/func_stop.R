###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a function to call stop() with a custom message.             #
###################################################################################################



func_stop <- function() {
  
  # Send notification -----------------------------------------------------------------------------
  # Wrapped in two tryCatch statements:
  # (1) notification might fail and then mask the actual error, we handle this
  # (2) logging might not yet be set up, if the error happens at the start of the processing, we also handle this.
  tryCatch({
    notify("Run failed ❌", 
           title = paste0("DMBSim ", dmbsim_version),
           image = normalizePath("icons/icon64.png"))
  }, error = function(e) {
    tryCatch({
    func_customlog("Could not send desktop notification (", conditionMessage(e), ").", level = 0)
    }, error = function(e2) {
      cat(paste0("Could not send desktop notification (\"", conditionMessage(e), 
          "\"). Logging also failed (\"", conditionMessage(e2), "\").\n"))
    })
  })
  
  
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
  
  
  # Show modal dialog -----------------------------------------------------------------------------
  if (!exists("logfile")) {
    logfile <- NULL
  }
  if (rstudioapi::isAvailable()) {
    func_end_dialog(logfile,
                    exit_state = "failure")
  }
  
  stop(paste0("\rRun failed at ", format(t_end), " (", Sys.timezone(), ")", "\n"))
  
}
