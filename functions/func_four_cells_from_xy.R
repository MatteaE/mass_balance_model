###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), after rhijmans code for raster package               #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the handy function fourCellsFromXY, which is needed          #
#                 to extract stake measurements but (as of 2023.01.12) is not yet implemented     #
#                 in the terra package. So we take it from the raster package.                    #
#                 .doFourCellsFromXY is implemented as an Rcpp function.                          #
###################################################################################################

# Actual R function.
fourCellsFromXY <- function(r, xy, duplicates = FALSE) {
  return(.doFourCellsFromXY(ncol(r), nrow(r), xmin(r), xmax(r), ymin(r), ymax(r), xy))
} 
