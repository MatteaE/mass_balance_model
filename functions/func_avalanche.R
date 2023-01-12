###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file implements the mass transport algorithm by Gruber (2007),             #
#                 to compute avalanche redistribution of snow.                                    #
#                 Input: raster grids of elevation, slope, aspect and initial mass.               #
#                 NOTE: we compute here a (heavily parametrized/simplified) *movable* mass        #
#                 which for each cell is a fraction of the initial mass.                          #
#                 NOTE-2: the 4 directions from a cell are indexed as 1 = up, 2 = left,           #
#                 3 = right, 4 = bottom. We use lists of rasters for the processing,              #
#                 list[[1]] is the raster related to the up direction.                            #
################################################################################################### 


# The deposition_max_multiplier can be used to enable consistent modelling on different input grids
# (e.g., the normalized snow distribution grid, which has cell values close to 1; an actual snow cover grid,
# with cell values of maybe 1000 (kg m-2); and a seasonal sum (if we choose to have a single avalanche over
# a whole year), with cell values of maybe 5000 (kg m-2)).
# This addresses the discussion of Section 4.4 in Gruber (2007).
# The preserve_edges switch makes the function put back the mass_initial value
# at the edges, so that the output of func_avalanche has no NAs.
func_avalanche <- function(run_params, grids_avalanche_cur, mass_initial_values, deposition_max_multiplier = 1.0, preserve_edges = TRUE) {
  
  deposition <- rast(grids_avalanche_cur$elevation_proc, vals = 0.0)
  mass_movable <- mass_initial_values * as.numeric(values(grids_avalanche_cur$movable_frac))
  mass_fixed <- mass_initial_values - mass_movable  # Mass which stays in place no matter what.
  
  # The snow transport loop is implemented in C++
  # for performance (about 5000 times faster than pure R).
  # An R version is in file "func_avalanche_gruber.R",
  # to use it set run_params$avalanche_routine_cpp to FALSE.
  transport_deposit_mass_chosen <- ifelse(run_params$avalanche_routine_cpp == TRUE, transport_deposit_mass, transport_deposit_mass_R)
  values(deposition) <- transport_deposit_mass_chosen(grids_avalanche_cur$elevation_sorted_ids,
                                                      run_params$grid_ncol,
                                                      as.numeric(values(deposition)),
                                                      mass_movable,
                                                      as.numeric(values(grids_avalanche_cur$deposition_max) * deposition_max_multiplier),
                                                      as.numeric(values(grids_avalanche_cur$draining_fraction[[1]])),
                                                      as.numeric(values(grids_avalanche_cur$draining_fraction[[2]])),
                                                      as.numeric(values(grids_avalanche_cur$draining_fraction[[3]])),
                                                      as.numeric(values(grids_avalanche_cur$draining_fraction[[4]])))
  
  
  
  
  mass_final_values <- mass_fixed + as.numeric(values(deposition))
  
  if (preserve_edges) {
    ids_na_logi                    <- is.na(mass_final_values)
    mass_final_values[ids_na_logi] <- mass_initial_values[ids_na_logi]
  }
  
  return(mass_final_values)
  
}
