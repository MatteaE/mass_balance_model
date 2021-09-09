###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to compute the initial snow cover for a year.    #
################################################################################################### 


# Algorithm:
  # Start with the topographic distribution grid (elevation and curvature)
  # Combine with the snow line elevation and snowgrad
  # Compute avalanche on the resulting grid (with appropriate multiplier for max deposition)
  # Reduce effect of the post-avalanche grid (custom reduction parameter, in IDL it is 0.5)
  # Multiply the grid with the probes idw if available
  # Return result

func_compute_initial_snow_cover <- function(run_params,
                                            data_dhms,
                                            data_dems,
                                            grids_snowdist_topographic,
                                            grids_avalanche_cur,
                                            grid_probes_norm,
                                            dhm_grid_id,
                                            dem_grid_id,
                                            data_massbal_winter) {
  
  # We start with the elevation/curvature effect.
  dist_cur <- grids_snowdist_topographic[[dem_grid_id]]
  
  # writeRaster(dist_cur, "1-dist-topo.tif", overwrite = T)
  
  # Distribution from snow line elevation and snow gradient.
  dist_snl <- setValues(dist_cur,
                        pmax(0,
                             getValues(data_dhms$elevation[[dhm_grid_id]] - run_params$initial_snowline_elevation) * run_params$initial_snow_gradient / 100))

  dist_cur <- dist_cur * dist_snl

  # writeRaster(dist_cur, "2-dist-topo-snl.tif", overwrite = T)
  
  # We set the mass deposition limit so that avalanches
  # won't carry snow below the marked initial snow line.
  dist_cur <- setValues(dist_cur,
                        func_avalanche(run_params,
                                       grids_avalanche_cur,
                                       getValues(dist_cur),
                                       run_params$deposition_max_ratio_init / mean(dist_cur[data_dems$glacier_cell_ids[[dem_grid_id]]]),
                                       TRUE))

  # writeRaster(dist_cur, "3-dist-topo-snl-aval.tif", overwrite = T)
  
  # Reduce importance of the computed distribution variability.
  # We do this only for cells which have snow! Else we would be
  # adding some snow to all cells, which is not what we want
  # with the reduction factor.
  dist_cur_snow_ids <- which(getValues(dist_cur) > 0)
  snow_mean <- mean(dist_cur[dist_cur_snow_ids])
  dist_cur[dist_cur_snow_ids] <- snow_mean + run_params$initial_snow_dist_red_fac * (dist_cur[dist_cur_snow_ids] - snow_mean)
  
  # writeRaster(dist_cur, "4-dist-topo-snl-aval-red.tif", overwrite = T)
  
  # If we have any winter stakes for the year,
  # use them to correct the final distribution.
  # In fact just one of the two conditions in
  # the if() should be enough.
  if((nrow(data_massbal_winter) > 0) & (!is.null(grid_probes_norm))) {
    
    dist_cur <- dist_cur * grid_probes_norm
    
    # writeRaster(dist_cur, "5-dist-topo-snl-aval-red-probes.tif", overwrite = T)
    
  }
  
  dist_cur[is.na(getValues(dist_cur))] <- 0.0 # Possible residual NA values in the current distribution, along the border.

  return(dist_cur)
  
}
