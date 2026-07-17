###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a function to call stop() with a custom message.             #
###################################################################################################



func_stop <- function() {
  
  flush(logcon)
  sink()
  t_end <- Sys.time()
  writeLines(paste0("Run failed at ", format(t_end), " (", Sys.timezone(), ")", "\n"), con = logcon, sep = "")
  flush(logcon)
  close(logcon)
  notify("Run failed ❌", title = "Glacier model DMBSim 3.0", image = normalizePath("icons/icon128.png"))
  stop(paste0("\rRun failed at ", format(t_end), " (", Sys.timezone(), ")", "\n"))
  
}
