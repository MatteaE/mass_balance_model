###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the selection of the indices of the mass balance points      #
#                 which will be considered for the given year.                                    #
#                 These are all the points whose period of measurement ENDS between 1 Jan and     #
#                 31 Dec of the year.                                                             #
################################################################################################### 

func_select_year_measurements <- function(data_massbal, year) {
  
  ids_year <- which(as.integer(format(data_massbal$end_date, "%Y")) == year)

  return(ids_year)
  
}