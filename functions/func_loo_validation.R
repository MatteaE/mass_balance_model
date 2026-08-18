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
# df_runs_smb        - a data.frame, initially received as parameter and then extended row by row,
#                      with one row per model realization (all: main run and all LOO runs);
#                      columns: run_id, applied correction factor, run_type, measurement-period and hydrological-year glacier-wide
#                      mass balance (for sensitivity), and per-point simulated mass balance (useful for LOO plots)
# df_runs_biases     - a data.frame, initially received as parameter and then extended row by row,
#                      with one row per model realization (all: main run and all LOO runs);
#                      columns: run_id, run_type, applied correction factor, and per-point biases
# stake_biases_mat   - a matrix, resulting from the extraction (from the previous df) of just the stake data
#                      (i.e., one column per stake)
# loo_set_biases_mat - a matrix, one row per model realization and one column per stake,
#                      initially computed from the received df_runs_biases and then extended row by row,
#                      holding the LOO-set bias for each stake (i.e. the mean bias of 
#                      the set which excludes the respective stake)
# df_loo_out         - a data.frame, initialized here as NA and gradually filled,
#                      with one row per mass balance point and columns:
#                      stake_id (same as in df_runs_biases), stake_name (the name in the mass balance file), point_loo_bias
#                      (the bias of the point in the model realization for which the previous data frame
#                      reports zero mean bias when excluding the current point),
#                      run_id of the run which we used to get this LOO result, corresponding correction factor,
#                      and iterations_n (0 if the point was fortuitously found as LOO-solved within another model run,
#                      >=1 if the point was under examination with one or more iterations when it was LOO-solved).

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
                                data_dhms, data_dems, data_surftype, data_radiation,
                                verbose_logi) {
  
  func_customlog("Starting leave-one-out validation.\n", level = 0)
  
  # Store this for later - number of main iterations.
  iter_prev_n <- nrow(year_data$df_runs_biases)
  
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
  loo_set_biases_mat <- (rowSums(stake_biases_mat) - stake_biases_mat) / (year_data$nstakes_annual-1)
  
  
  # Check which stakes can be marked as solved.
  df_loo_out <- func_loo_check_solved(run_params,
                                      year_data$df_runs_biases, stake_biases_mat,
                                      loo_set_biases_mat, df_loo_out,
                                      1:nrow(year_data$df_runs_biases),
                                      target_iterations_n = NULL,
                                      target_stake_id = NULL,
                                      verbose_logi = verbose_logi)
  
  stakes_to_solve_ids <- which(is.na(df_loo_out$stake_loo_bias))
  stakes_to_solve_n   <- length(stakes_to_solve_ids)
  
  cat("After the main runs, there are", stakes_to_solve_n, "stake(s) left for LOO validation.\n")
  
  iter_cur_id <- nrow(loo_set_biases_mat)     # Global index of the current iteration
  iter_tgt_n  <- rep(0,nrow(df_loo_out))      # Number of iterations expended on each target stake
  stakes_to_solve_orig_n <- stakes_to_solve_n # Used to print progress
  while ((stakes_to_solve_n > 0) && (all(iter_tgt_n < run_params$loo_stake_iter_max_n))) {
    
    
    # Select which stake should be targeted next.
    # Rather than blindly picking the first one,
    # we always take the one with the least accurate information
    # (the one whose best model realization so far is still the farthest
    # from 0 in terms of LOO-set bias). This allows to explore the parameter
    # space faster, reducing the number of required iterations.
    stake_tgt_id <- stakes_to_solve_ids[which.max(colMins(abs(loo_set_biases_mat[,stakes_to_solve_ids,drop = FALSE])))]
    
    # Update iteration numbers.
    iter_cur_id <- iter_cur_id + 1
    iter_tgt_n[stake_tgt_id]  <- iter_tgt_n[stake_tgt_id] + 1
    
    
    runs_sel_ids <- order(abs(loo_set_biases_mat[, stake_tgt_id]))[1:2]
    
    if (verbose_logi) {
      cat("\n")
      cat("  Targeting stake", stake_tgt_id, "using runs", paste0(runs_sel_ids, collapse = " and "), "\n")
    }
    
    c1 <- year_data$df_runs_biases$corr_fact[runs_sel_ids[1]]
    c2 <- year_data$df_runs_biases$corr_fact[runs_sel_ids[2]]
    b1 <- loo_set_biases_mat[runs_sel_ids[1], stake_tgt_id]
    b2 <- loo_set_biases_mat[runs_sel_ids[2], stake_tgt_id]
    corr_fact_cur <- c1 - b1 * (c2 - c1) / (b2 - b1)
    
    mod_output_cur <- func_optim_worker("annual", corr_fact_cur, year_data$corr_fact_winter,
                                        run_params, year_cur_params,
                                        year_data$dhm_grid_id, year_data$dem_grid_id, year_data$surftype_grid_id,
                                        data_dhms, data_dems, data_surftype,
                                        year_data$snowdist_init_annual, data_radiation, year_data$weather_series_annual_cur,
                                        year_data$dist_topographic_values_red, year_data$dist_probes_norm_values_red,
                                        year_data$grids_avalanche_cur, year_data$grid_ice_albedo_fact_cur_values,
                                        year_data$points_dxdy[["annual"]][[1]], year_data$points_dxdy[["annual"]][[2]],
                                        year_data$points_dxdy[["annual"]][[3]], year_data$points_dxdy[["annual"]][[4]],
                                        year_data$nstakes_annual, year_data$model_annual_days_n,
                                        year_data$massbal_annual_meas_cur, year_data$annual_stakes_cells, verbose_logi = FALSE)
    
    
    year_data$df_runs_smb    <- rbind(year_data$df_runs_smb,
                                      func_compile_df_runs_smb(year_cur_params, year_data, mod_output_cur, iter_cur_id, corr_fact_cur, "loo"))
    year_data$df_runs_biases <- rbind(year_data$df_runs_biases,
                                      func_compile_df_runs_biases(year_data, mod_output_cur, iter_cur_id, corr_fact_cur, "loo"))
    
    # Extract stake biases, including the new run.
    stake_biases_mat  <- as.matrix(year_data$df_runs_biases[,biases_cols_ids])
    
    # Recalculate LOO biases including the new run.
    loo_set_biases_mat <- (rowSums(stake_biases_mat) - stake_biases_mat) / (year_data$nstakes_annual-1)
    
    stakes_to_solve_prev_n <- length(which(is.na(df_loo_out$stake_loo_bias)))
    
    # Check which stakes can be marked as solved.
    df_loo_out <- func_loo_check_solved(run_params,
                                        year_data$df_runs_biases, stake_biases_mat,
                                        loo_set_biases_mat, df_loo_out,
                                        iter_cur_id,
                                        target_iterations_n = iter_tgt_n,
                                        target_stake_id     = stake_tgt_id,
                                        verbose_logi = verbose_logi)
    
    # Prepare next iteration.
    stakes_to_solve_ids <- which(is.na(df_loo_out$stake_loo_bias))
    stakes_to_solve_n   <- length(stakes_to_solve_ids)
    
    # Print progress if any.
    if (stakes_to_solve_n < stakes_to_solve_prev_n) {
      stakes_to_solve_nums <- (stakes_to_solve_orig_n-stakes_to_solve_prev_n+1):(stakes_to_solve_orig_n-stakes_to_solve_n)
      stakes_solved_str   <- paste0(paste0(stakes_to_solve_nums,collapse = "..."), "...")
      cat(stakes_solved_str)
    }
    
    
  } # End LOO loop.
  
  cat("\n")
  
  # If the iteration limit was hit, give details.
  if (stakes_to_solve_n > 0) {
    stake_problematic_id <- which(iter_tgt_n >= run_params$loo_stake_iter_max_n)[1]
    if (is.na(stake_problematic_id)) {
      func_customlog("  LOO validation exited early for unknown reasons - please debug manually.", level = 2)
      func_stop()
    }
    func_customlog("  LOO validation hit iteration limit for stake ", stake_problematic_id, "(", df_loo_out$stake_name[stake_problematic_id], "). Please check it.", level = 2)
    func_stop()
  }
  
  
  func_customlog("LOO validation finished after ", iter_cur_id-iter_prev_n, " dedicated iterations.", level = 0)
  cat("\n")
  
  year_data$df_loo_out     <- df_loo_out
  
  year_data$global_loo_rms <- sqrt(mean(year_data$df_loo_out$stake_loo_bias^2))
  
  return(year_data)
  
}
