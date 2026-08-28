###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to compute the areas of Voronoi cells           #
#                 corresponding to the given input points.                                        #
###################################################################################################


func_compute_voronoi_cells <- function(massbal_cur_v,
                                       ext_cur,
                                       outl_v) {

    
  # This does not keep the same ordering as massbal_cur_v,
  # so we need to use match() later.
  voronoi_cur   <- voronoi(massbal_cur_v, bnd = ext_cur)
  
  # Crop to glacierized extent.
  cells_cur     <- terra::intersect(voronoi_cur, outl_v)
  
  cells_ids     <- match(massbal_cur_v$id,
                         cells_cur$id)
  
  return(cells_cur[cells_ids,])
  
}
