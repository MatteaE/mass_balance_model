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
#      If the two selected runs for secant calculation were previously used, switch to bracketing instead (i.e., take cell values with lowest absolute value and opposite signs, which are guaranteed to exist at this stage)
#      If we have hit the iteration limit for the current target point, it means it is problematic (happens with avalanches - bias can jump, and a jump at a high-weight stake can prevent convergence of the whole LOO set). In that case just take the lowest-absolute-LOO-bias result for that stake and issue a warning.
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
  
  biases_cols_ids   <- grep("^sa[0-9]+$", names(year_data$df_runs_biases))
  stake_biases_mat  <- as.matrix(year_data$df_runs_biases[,biases_cols_ids])
  
  # Skeleton of the output data frame with the LOO results.
  df_loo_out <- data.frame(stake_id         = names(year_data$df_runs_biases)[biases_cols_ids],
                           stake_name       = year_data$massbal_annual_meas_cur$id,
                           stake_loo_bias   = NA_real_,
                           loo_run_id       = NA_integer_,
                           loo_corr_fact    = NA_real_,
                           loo_iterations_n = NA_integer_)
  
  
  
  # Ensure that the weights matrix has correct normalization.
  if (any(abs(rowSums(year_data$loo_weights_mat) - (year_data$nstakes_annual - 1)) > 1e-8)) {
    func_customlog("Problem with the LOO weights normalization - please check manually!", level = 2)
    func_stop()
  }
  
  # Calculate LOO biases of the main optimization runs.
  loo_set_biases_mat <- (stake_biases_mat %*% t(year_data$loo_weights_mat)) / (year_data$nstakes_annual - 1)
  
  
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
  
  # Now start targeting remaining stakes, one by one.
  iter_cur_id            <- nrow(loo_set_biases_mat)   # Global index of the current iteration
  iter_tgt_n             <- rep(0, nrow(df_loo_out))   # Number of iterations expended on each target stake
  stakes_to_solve_orig_n <- stakes_to_solve_n          # Used to print progress
  runs_used_for_target   <- lapply(1:year_data$nstakes_annual, matrix, nrow = 0, ncol = 2) # Used to remember which runs were already used to target which stakes, to avoid getting stuck in case the two used runs get selected repeatedly.
  stake_bracket_logi     <- rep(FALSE, year_data$nstakes_annual) # Used to force bracketing of a stake as soon as it was required once (see comment below on the selection of runs_sel_ids).
  while (stakes_to_solve_n > 0) {
    
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
    
    
    # Here, we select the two runs to be used for the calculation.
    # The naive way would be to always pick the two previous runs having the
    # lowest absolute bias for the current target left-out stake:
    # runs_sel_ids <- order(abs(loo_set_biases_mat[, stake_tgt_id]))[1:2]
    # However, this can fail (never converge) in some nonlinear edge cases:
    # if the LOO-bias of those two selected runs has the same sign
    # and the estimation overshoots on the other side of 0.0
    # (e.g., the two closest runs have 3.1 and 1.1 mm w.e., and the estimate produces
    # -3.2 mm w.e.) - in that case, the same two runs would keep getting selected over
    # and over, always adding copies of a same result and hitting the iteration limit.
    # To fix this: we keep track of the pairs of indices of the realizations that
    # were already used while targeting a given stake (as list() of 2-column integer matrices,
    # one list element per target stake). If the selected pair was already used for the
    # current target, then the selection rule becomes: pick the two runs with opposite sign
    # which have the lowest bias (among those with their sign).
    # Then we are guaranteed to move closer to convergence (we are bracketing 0.0).
    # At this stage we are confident that there are runs on both sides of 0.0 because a same
    # pair can be selected again only if both runs of the pair have LOO bias with the same
    # sign and the pair produces an overshoot of 0.0 with larger absolute value,
    # then this overshoot is exactly the run result on the other side of 0.0.
    # So, first attempt: select the two runs with lowest absolute bias; sort() them
    # in increasing order, so that they are stored consistently in the list().
    runs_sel_ids <- sort(order(abs(loo_set_biases_mat[, stake_tgt_id]))[1:2])
    
    # If the runs pair was already used, switch to bracketing and force it until stake is solved
    # (if we had to switch to bracketing once, it means the stake is risky/nonlinear,
    # so it makes sense to take the more careful approach).
    if ((stake_bracket_logi[stake_tgt_id] == FALSE) &&
        (any(runs_used_for_target[[stake_tgt_id]][, 1] == runs_sel_ids[1] &
             runs_used_for_target[[stake_tgt_id]][, 2] == runs_sel_ids[2]))) {
      cat("  Detected repeated indices in LOO bias targeting. Switching to bracketing for the current target stake...\n")
      stake_bracket_logi[stake_tgt_id] <- TRUE
    }
    
    if (stake_bracket_logi[stake_tgt_id] == TRUE) {
      cat("  Selecting runs as bracket.\n")
      runs_sel_ids[1] <- which.max(1.0/loo_set_biases_mat[, stake_tgt_id]) # This trick selects the smallest positive LOO-bias.
      runs_sel_ids[2] <- which.max(1.0/-loo_set_biases_mat[, stake_tgt_id]) # This trick selects the smallest (absolute) negative LOO-bias.
      runs_sel_ids    <- sort(runs_sel_ids)
    }
    
    # Store selected index pair of runs to be used for targeting the current stake.
    runs_used_for_target[[stake_tgt_id]] <- rbind(runs_used_for_target[[stake_tgt_id]], sort(runs_sel_ids))
    
    
    c1 <- year_data$df_runs_biases$corr_fact[runs_sel_ids[1]]
    c2 <- year_data$df_runs_biases$corr_fact[runs_sel_ids[2]]
    b1 <- loo_set_biases_mat[runs_sel_ids[1], stake_tgt_id]
    b2 <- loo_set_biases_mat[runs_sel_ids[2], stake_tgt_id]
    corr_fact_cur <- c1 - b1 * (c2 - c1) / (b2 - b1)
    
    if (verbose_logi) {
      cat("  Targeting stake", stake_tgt_id, "using runs", paste0(runs_sel_ids, collapse = " and "),
          "- LOO biases:", sprintf("%+.2f", b1), "and", sprintf("%+.2f mm w.e.", b2), "\n")
    }
    
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
                                        year_data$massbal_annual_meas_cur, year_data$annual_stakes_cells,
                                        verbose_logi = verbose_logi)
    
    
    year_data$df_runs_smb    <- rbind(year_data$df_runs_smb,
                                      func_compile_df_runs_smb(year_cur_params, year_data, mod_output_cur, iter_cur_id, corr_fact_cur, "loo"))
    year_data$df_runs_biases <- rbind(year_data$df_runs_biases,
                                      func_compile_df_runs_biases(year_data, mod_output_cur, iter_cur_id, corr_fact_cur, "loo"))
    
    # Extract stake biases, including the new run.
    stake_biases_mat  <- as.matrix(year_data$df_runs_biases[,biases_cols_ids])
    
    # Recalculate LOO biases including the new run.
    loo_set_biases_mat <- (stake_biases_mat %*% t(year_data$loo_weights_mat)) / (year_data$nstakes_annual - 1)
    
    stakes_to_solve_prev_n <- length(which(is.na(df_loo_out$stake_loo_bias)))
    
    # Check which stakes can be marked as solved.
    df_loo_out <- func_loo_check_solved(run_params,
                                        year_data$df_runs_biases, stake_biases_mat,
                                        loo_set_biases_mat, df_loo_out,
                                        iter_cur_id,
                                        target_iterations_n = iter_tgt_n,
                                        target_stake_id     = stake_tgt_id,
                                        verbose_logi        = verbose_logi)
    
    
    # Did we just hit the iteration limit at the current stake and still not converge?
    # If yes, take the lowest-absolute-value LOO bias as result, and issue a warning.
    # This can happen especially in case there is one high-weight stake in the LOO set
    # whose bias is very sensitive (i.e., at the edge of an avalanche) - then it will
    # pollute the whole LOO-set bias, preventing convergence.
    # It was observed that this is less serious than it may appear - we have failed to converge on this LOO set
    # because we are in a region of corr_fact where the bias of the
    # set which EXCLUDES the current stake is very sensitive / badly behaved,
    # but the bias of the current stake (what we are after) will not necessarily also be very sensitive!
    # So usually we will still be within 1 mm w.e. accuracy of the stake_loo_bias of the current stake.
    # Thus, we issue a warning only if the stake_loo_bias of the best bracket has a spread > 1 mm (or whatever
    # optim threshold is set). Otherwise we just used a lot of iterations but the result is still fully accurate.
    if ((iter_tgt_n[stake_tgt_id] == run_params$loo_stake_iter_max_n) &&
        is.na(df_loo_out$stake_loo_bias[stake_tgt_id])) {
      
      run_id_best                               <- which.min(abs(loo_set_biases_mat[, stake_tgt_id]))
      df_loo_out$stake_loo_bias[stake_tgt_id]   <- stake_biases_mat[cbind(run_id_best, stake_tgt_id)]
      df_loo_out$loo_run_id[stake_tgt_id]       <- year_data$df_runs_biases$run_id[run_id_best]
      df_loo_out$loo_corr_fact[stake_tgt_id]    <- year_data$df_runs_biases$corr_fact[run_id_best]
      df_loo_out$loo_iterations_n[stake_tgt_id] <- iter_tgt_n[stake_tgt_id]+1 # Mark as converged at one iteration past the limit.
      
      # Select closest bracket and print values of the LOO bias of the stake, to evaluate the sensitivity of the stake LOO bias itself.
      runs_bracket_ids <- c(which.max(1.0/loo_set_biases_mat[, stake_tgt_id]),
                            which.max(1.0/-loo_set_biases_mat[, stake_tgt_id]))
      stake_biases_bracket <- sort(stake_biases_mat[runs_bracket_ids, stake_tgt_id])
      ndigits              <- max(1,min(6, 1+ceiling(abs(log10(diff(stake_biases_bracket)))))) # Number of digits needed to represent the bias bracket properly.
      
      if (verbose_logi) {
        func_customlog("  LOO validation hit iteration limit (n = ",  run_params$loo_stake_iter_max_n, ") for target stake ",
                       stake_tgt_id, " (", df_loo_out$stake_name[stake_tgt_id], ").", level = 1)
        func_customlog("            Falling back to closest LOO-set result: ", sprintf("%+.2f", loo_set_biases_mat[run_id_best,stake_tgt_id]), " mm w.e.", level = 0)
        
        bracket_fmt <- paste0("[%+.", ndigits,"f, %+.", ndigits, "f]")
        func_customlog("            Stake bias bracket: ", sprintf(bracket_fmt, stake_biases_bracket[1], stake_biases_bracket[2]), " mm w.e. (range: ",
                       sprintf(paste0("%.", ndigits, "f"), diff(stake_biases_bracket)), " mm w.e.)", level = 0)
      }
      
      if (diff(stake_biases_bracket) > run_params$optim_bias_threshold) {
        func_customlog("  LOO validation did not converge for point ", df_loo_out$stake_name[stake_tgt_id], ".", level = 1)
        func_customlog("            The spread of the closest biases is ", sprintf(paste0("%.", ndigits, "f"), diff(stake_biases_bracket)),
                       " mm w.e., which exceeds the convergence threshold (", run_params$optim_bias_threshold, "mm w.e.)", level = 0)
        func_customlog("            Treat LOO validation results with caution.", level = 1)
      }
      
    }
    
    
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
  
  # If the LOO validation did not succeed, stop.
  if (stakes_to_solve_n > 0) {
    func_customlog("  LOO validation failed for unknown reasons - please debug manually.", level = 2)
    func_stop()
  }
  
  
  func_customlog("LOO validation finished after ", iter_cur_id-iter_prev_n, " dedicated iterations.", level = 0)
  cat("\n")
  
  year_data$df_loo_out     <- df_loo_out
  
  # Store LOO RMS for plotting and saving.
  year_data$global_loo_rms   <- sqrt(mean(year_data$df_loo_out$stake_loo_bias^2))
  year_data$weighted_loo_rms <- sqrt(mean(year_data$massbal_annual_meas_cur$area_weight * (year_data$df_loo_out$stake_loo_bias^2)))
  
  return(year_data)
  
}
