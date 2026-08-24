###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a helper function to write to console with some style.       #
################################################################################################### 

# Select OS-specific function to write to console.
# RStudio on Windows uses an ugly red text for message(),
# so we use cat() and ANSI bold text instead.
if (Sys.info()["sysname"] == "Windows") {
  
  func_consolewrite <- function(prefix, txt) {
    cat(paste0(prefix,
               "\033[1m", txt, "\033[0m", "\n"))
  }
  
  # On other systems use message(): RStudio uses a nice grey background.
} else {
  
  func_consolewrite <- function(prefix, txt) {
    message(paste0(prefix,
                   txt))
  }
}
