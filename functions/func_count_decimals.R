###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a utility function to return the number of decimal places    #
#                 of a numeric value.                                                             #
###################################################################################################  


func_count_decimals <- function(x) {
  
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    
    return(nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]]))
    
  } else {
    
    return(0)
    
  }
  
}
