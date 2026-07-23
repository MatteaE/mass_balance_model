###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to show a modal dialog on model exit,           #
#                 giving information about the run.                                               #
################################################################################################### 


func_end_dialog <- function(run_params,
                            logfile,
                            exit_state) {
  
  # If the model run managed to finish, write it.
  if (exit_state == "success") {
    
    msg_txt <- "<p><b>✅ Run finished successfully</b></p>"
    
    # If there are between 1 and 10 warnings, write them all.
    # Otherwise, write just the first 10.
    warnings_n <- length(warnings_char)
    if (warnings_n > 0) {
      
      # There are more than 10 warnings - print the first 10 and direct to the log file.
      if (warnings_n > 10) {
        
        msg_txt <- paste0(msg_txt,
                          "<p><b>⚠️ There were ", warnings_n, " warning️s</b></p>",
                          "<p><b>Showing the first 10:</b><p>")
        msg_txt <- paste0(msg_txt,
                          "<p>(1) ", warnings_char[1],
                          paste0(paste0("</p><p>(", 2:10, ") "), warnings_char[2:10], collapse = ""),
                          "</p>")
        msg_txt <- paste0(msg_txt,
                          "<p><b>Check out all warnings in the log file:</b></p>",
                          "<p>", logfile, "</p>")
        
        
        # There are 10 or fewer warnings - print them all.
      } else {
        
        # There are 2-10 warnings.
        if (warnings_n > 1) {
        msg_txt <- paste0(msg_txt,
                          "<p><b>⚠️ There were ", warnings_n, " warnings:</b></p>",
                          "<p>(1) ", warnings_char[1],
                          paste0(paste0("</p><p>(", 2:warnings_n, ") "), warnings_char[2:warnings_n], collapse = ""),
                          "</p>")
        
        # There is exactly 1 warning.
        } else {
          msg_txt <- paste0(msg_txt,
                            "<p><b>⚠️ There was ", warnings_n, " warning:</b></p>",
                            "<p>(1) ", warnings_char, "</p>")
        }
        msg_txt <- paste0(msg_txt,
                          "<p><b>Check out the log file for full information:</b></p>",
                          "<p>", logfile, "</p>")
        
      }
      
      # Else: no warnings generated - best possible case! 
    } else {
      msg_txt <- paste0(msg_txt,
                        "<p><b>✅ No warnings were generated</b></p>",
                        "<p><b>Log file:</b></p>",
                        "<p>", logfile, "</p>")
    }
    
    # The model run did not manage to finish.
  } else {
    
    msg_txt <- "<p><b>❌ Run failed</b></p>"
    # This should be always TRUE, but let's be careful.
    if (length(fatal_char) > 0) {
      msg_txt <- paste0(msg_txt,
                        "<p><b>The raised error was:</b></p>",
                        "<p>", fatal_char[1], "</p>",
                        "<p><b>Check out the log file for full information:</b></p>",
                        "<p>", logfile, "</p>")
    }
    
  }
  
  rstudioapi::showDialog(
    paste0("DMBSim ", run_params$dmbsim_version),
    msg_txt
  )
  
}
