###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to run leave-one-out validation of the point     #
#                 measurements.                                                                   #
################################################################################################### 


# The LOO validation needs in principle many model runs/realizations (>= 1 per mass balance point).
# Our implementation has two major performance optimizations:
# - gradient estimation from the past model realizations - all point biases from
#   previous results are kept and used to converge to each LOO solution as fast as possible
# - sweep pass at each realization - whenever a model realization is computed, this function checks
#   whether the realization can serve as LOO result for any mass balance point - this could opportunistically
#   produce many LOO results with few model realizations (which are expensive).


# Data structures:
# df_runs_smb      - a data.frame, initially received as parameter and then extended row by row,
#                    with one row per model realization (all: main run and all LOO runs);
#                    columns: run_id, applied correction factor, run_type, measurement-period and hydrological-year glacier-wide
#                    mass balance (for sensitivity), and per-point simulated mass balance (useful for LOO plots)
# df_runs_biases   - a data.frame, initially received as parameter and then extended row by row,
#                    with one row per model realization (all: main run and all LOO runs);
#                    columns: run_id, run_type, applied correction factor, and per-point biases
# df_loo_biases    - a data.frame initially computed from the received df_runs_biases and then extended row by row,
#                    with one row per model realization and columns:
#                    run_id, correction factor, and bias_loo_pt<i> for each point, holding the LOO mean bias
#                    (i.e. mean bias of the set which excludes the respective point)
# df_loo_out       - a data.frame, initialized here as NA and gradually filled,
#                    with one row per mass balance point and columns:
#                    stake_id (same as in df_runs_biases), stake_name (the name in the mass balance file), point_loo_bias
#                    (the bias of the point in the model realization for which the previous data frame
#                    reports zero mean bias when excluding the current point),
#                    run_id of the run which we used to get this LOO result, corresponding correction factor,
#                    and iterations_n (0 if the point was fortuitously found as LOO-solved within another model run,
#                    >=1 if the point was under examination with one or more iterations when it was LOO-solved).

# run_type reports whether the run was one of the two first (dummy) main optimization runs,
#          one of the various optimization runs with all stakes, the final optimization run
#          with all stakes, or a LOO run.

# Algorithm:
# Receive the starting df_runs_biases of previous (main) model runs (with point biases) from the main model run (>=2 iterations/rows, by design)
# Initialize the df_loo_out data frame (one row per point, empty/NA except point id)
# Compute the initial df_loo_biases data frame
# Run a first check in df_loo_biases: find any cells where the LOO bias is <= 1 mm w.e. and mark any such lucky early solves (for which any of the main model runs were already LOO solutions; use iterations_n = 0 and appropriate run_id)
# Define iterations_cur_n = 0, used to check the number of iterations needed to solve each point
# Loop over points still missing a LOO result. For the current point j:
#   Increase iterations_cur_n by one
#   Find in the df_loo_biases (in the column of the current point) the two cell values with lowest absolute LOO bias, compute (secant) derivative of LOO bias w.r.t. multiplier and use it to predict the multiplier value for which LOO bias should be 0
#   Perform a model run at that correction factor value
#   Append the new row (with individual point biases) to the df_runs_biases data frame
#   Compute a new row of the df_loo_biases data frame, see if there are any new solved (abs < 1.0 mm) values which weren't previously done; if j is one of them, set its iterations_n to iterations_cur_n and then reset iterations_cur_n to 0
#   Check which one is the next unsolved point (either still j or another one) and go towards it at the next iteration (if the next point is still j then we just keep moving towards it, otherwise we just move towards another point)
#   If there are no more unsolved points, exit (return a list: df_runs_smb, df_runs_biases, df_loo_out)

func_loo_validation <- function(run_params, year_cur_params, year_data,
                                data_dhms, data_dems, data_surftype, data_radiation) {
  
  
  biases_cols_ids   <- grep("^s[0-9]+$", names(year_data$df_runs_biases))
  stake_biases_mat  <- as.matrix(year_data$df_runs_biases[,biases_cols_ids])
  
  # Skeleton of the output data frame with the LOO results.
  df_loo_out <- data.frame(stake_id         = names(year_data$df_runs_biases)[biases_cols_ids],
                           stake_name       = year_data$massbal_annual_meas_cur$id,
                           stake_loo_bias   = NA_real_,
                           loo_run_id       = NA_integer_,
                           loo_corr_fact    = NA_real_,
                           loo_iterations_n = NA_integer_)
  
  # Calculate LOO biases of the main optimization runs.  
  loo_biases_mat <- (rowSums(stake_biases_mat) - stake_biases_mat) / (year_data$nstakes_annual-1)
  
  
  df_loo_out <- func_loo_check_solved(run_params,
                                      year_data$df_runs_biases, loo_biases_mat, df_loo_out,
                                      1:nrow(year_data$df_runs_biases),
                                      target_iterations_n = NULL,
                                      target_stake_id = NULL)
  
  # WIP: now iterate on the not-yet-solved points
  
  
  
}
