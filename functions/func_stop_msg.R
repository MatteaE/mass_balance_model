###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a function to call stop() with a custom message.             #
###################################################################################################



func_stop_msg <- function() {
  
  stop("\rRun stopped early due to a fatal error")
  
}
