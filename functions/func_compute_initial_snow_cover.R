###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the initial snow cover for a year.    #
################################################################################################### 


# Algorithm:
# Start with the topographic distribution grid (elevation and curvature), already reduced in variability if the user chose to do so.
# Combine with the snow line elevation and snowgrad
# Compute avalanche on the resulting grid (with appropriate multiplier for max deposition)
# Multiply the grid with the probes idw if available
# Return result

func_compute_initial_snow_cover <- function(run_params,
                                            data_dhms,
                                            data_dems,
                                            grids_snowdist_topographic,
                                            grids_avalanche_cur,
                                            grid_probes_norm_values,
                                            dhm_grid_id,
                                            dem_grid_id,
                                            data_massbal_winter) {
  
  # We start with the elevation/curvature effect.
  # This is possibly already reduced by user-defined parameter.
  dist_cur <- grids_snowdist_topographic[[dem_grid_id]]
  
  # writeRaster(dist_cur, "1-dist-topo.tif", overwrite = T)
  
  # Distribution from snow line elevation and snow gradient.
  dist_snl <- setValues(dist_cur,
                        pmax(0,
                             values(data_dhms$elevation[[dhm_grid_id]] - run_params$initial_snowline_elevation) * run_params$initial_snow_gradient / 100))
  
  dist_cur <- dist_cur * dist_snl
  
  # writeRaster(dist_cur, "2-dist-topo-snl.tif", overwrite = T)
  

  
  # If we have any winter stakes for the year,
  # use the large-scale variability computed from
  # them to further enhance the initial snow distribution.
  if (nrow(data_massbal_winter) > 0) {
    
    # We put the (normalized, reduced-variability) values from IDW
    # of probes back onto a raster grid for this multiplication.
    dist_cur <- dist_cur * setValues(data_dhms$elevation[[dhm_grid_id]], grid_probes_norm_values)
    
    # writeRaster(dist_cur, "3-dist-topo-snl-probes.tif", overwrite = T)
    
  }
  
  
  # If asked to do so, redistribute mass with an avalanche.
  # For this, use the actual values of maximum deposition,
  # do not alter them with a multiplier.
  if (run_params$initial_snow_avalanche) {
    values(dist_cur) <- func_avalanche(run_params,
                                       grids_avalanche_cur,
                                       as.numeric(values(dist_cur)),
                                       1.0,
                                       TRUE)
    # writeRaster(dist_cur, "4-dist-topo-snl-probes-avalanche.tif", overwrite = T)
  }
  
  
  dist_cur <- subst(dist_cur, NA, 0.0) # Possible residual NA values in the current distribution, along the border.
  
  return(dist_cur)
  
}
