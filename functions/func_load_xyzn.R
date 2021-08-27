###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for a single XYZN file, which is         #
#                 loaded as a SpatialPolygons-class object.                                       #
#                 This routine is called by func_load_outlines().                                 #
################################################################################################### 


func_load_xyzn <- function(filepath, refsys) {
  
  dat_raw <- read.table(filepath, header = FALSE)
  
  names(dat_raw) <- c("x", "y", "z", "n")
  
  ids_start <- which(dat_raw$n == 21)
  ids_end <- which(dat_raw$n == 23)
  
  # Polygon -> Polygons -> SpatialPolygons
  pol <- list()
  for (pol_id in 1:length(ids_start)) {
    
    pol_start <- ids_start[pol_id]
    pol_end <- ids_end[pol_id]
    pol[[pol_id]] <- Polygon(dat_raw[pol_start:pol_end, 1:2])
    
  }
  
  pols <- Polygons(pol, 1)
  spols <- SpatialPolygons(list(pols), proj4string = refsys)
  
  return(spols)
  
}
