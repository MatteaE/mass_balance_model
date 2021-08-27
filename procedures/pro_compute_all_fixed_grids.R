###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to call the computation of fixed grids for          #
#                 avalanches, topographic snow distribution and variable ice albedo.              #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

grids_avalanche            <-   func_compute_avalanche_fixed_grids(run_params, data_dhms)
grids_snowdist_topographic <-   func_compute_snowdist_topographic(run_params, data_dhms, data_dems)
grids_ice_albedo_fact      <-   func_compute_variable_ice_albedo(run_params, data_dhms)
