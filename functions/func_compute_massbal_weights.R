###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the function to compute the weights of the mass balance      #
#                 points to be used in bias calculations.                                         #
###################################################################################################


# This function returns a numeric vector of weights (one element per mass balance point) if compute_loo is FALSE,
# else it returns a matrix with one column per mass balance point and nrow(xy_mat) + 1 rows - first row
# is the set of weights for the full set of mass balance points; the second row is the set of weights for
# the first LOO set (i.e., removing the first mass balance point - its own weight is 0, and the weights of the
# neighboring points are a bit higher since they conquer its Voronoi cell).
# The weights are always normalized to average 1 (i.e. sum = nrow(massbal points)).

func_compute_massbal_weights <- function(run_params,
                                         massbal_type, # Either "winter" or "annual"
                                         year_data,
                                         data_dhms,
                                         data_outlines,
                                         compute_loo) {
  
  
  cat("Computing weights of the", massbal_type, "mass balance points...\n")
  
  # Select winter vs annual mass balance measurements.
  massbal_cur <- year_data[[paste0("massbal_", massbal_type, "_meas_cur")]]
  
  # Convert outline to SpatVector.
  outl_v    <- set.crs(vect(data_outlines$outlines[[year_data$outline_id]]),
                       run_params$grids_crs_epsg)
  
  
  # If there is a single mass balance entry:
  # This function produces a single cell with weight = 1.0.
  if (nrow(massbal_cur) == 1) {
    year_data[[paste0("voronoi_", massbal_type, "_v")]]                    <- outl_v
    year_data[[paste0("massbal_", massbal_type, "_meas_cur")]]$area_weight <- 1.0
    return(year_data)
  }
  
  
  # Otherwise prepare the data for the main Voronoi calculation.
  # It is done and shown in all cases, even when the weights are uniform.
  massbal_v <- vect(massbal_cur,
                    geom = c("x", "y"),
                    crs = run_params$grids_crs_epsg)
  ext_cur   <- ext(data_dhms$elevation[[year_data$dhm_grid_id]])
  
  # These have the same ordering as the input mass balance points.
  voronoi_cells_main_v <- func_compute_voronoi_cells(massbal_v,
                                                     ext_cur,
                                                     outl_v)
  # Compute areas.
  areas_main <- expanse(voronoi_cells_main_v)
  
  # This formula does:
  # - normalization of weights to average 1.0 (i.e., they sum to nstakes-1)
  # - linear interpolation between 1.0 and the computed weight,
  # according to period-appropriate optim_<winter,annual>_areaweight_fact.
  weights_main <- rep(1.0, nrow(massbal_cur)) + ((areas_main * nrow(massbal_cur) / sum(areas_main)) - 1.0) * run_params[[paste0("optim_", massbal_type, "_areaweight_fact")]]
  
  # Store main Voronoi cells and weights.
  year_data[[paste0("voronoi_", massbal_type, "_v")]]                    <- voronoi_cells_main_v
  year_data[[paste0("massbal_", massbal_type, "_meas_cur")]]$area_weight <- weights_main
  
  # If we are doing LOO, also compute LOO weights (but Voronoi cells are not stored).
  if (compute_loo == TRUE) {
    
    cat("Computing LOO weights...\n")
    
    weights_all_m <- matrix(data = NA_real_,
                            nrow = nrow(massbal_cur),
                            ncol = nrow(massbal_cur))
    
    for (point_id in 1:nrow(massbal_cur)) {
      
      # cat(point_id, "\n")
      
      massbal_loo_v <- massbal_v[-point_id,]
      cells_loo_v   <- func_compute_voronoi_cells(massbal_loo_v,
                                                  ext_cur,
                                                  outl_v)
      areas_loo_cur <- expanse(cells_loo_v)
      
      # This formula does the same as for the main one, but for the LOO case.
      # Weights are again normalized to average 1.0 (excluding the one of the
      # currently removed point which is 0.0).
      # I.e., they sum to nstakes-1.
      weights_loo_cur <- rep(1.0, nrow(massbal_cur)-1) + ((areas_loo_cur * (nrow(massbal_cur)-1) / sum(areas_loo_cur)) - 1.0) * run_params[[paste0("optim_", massbal_type, "_areaweight_fact")]]
      
      # Here the 0.0 weight for the currenly excluded point is set.
      weights_all_m[point_id,] <- append(weights_loo_cur, 0.0, after = point_id - 1)
      
    } # End LOO loop on the points
    
    year_data$loo_weights_mat <- weights_all_m
  } # End if we are doing LOO
  
  return(year_data)
}
