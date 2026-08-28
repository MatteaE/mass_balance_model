###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to check which stakes are LOO-solved by the      #
#                 given model realization(s).                                                     #
###################################################################################################


func_loo_check_solved <- function(run_params,
                                  df_runs_biases, stake_biases_mat,
                                  loo_set_biases_mat, df_loo_out,
                                  rows_to_check,
                                  target_iterations_n = NULL,
                                  target_stake_id = NULL,
                                  verbose_logi = FALSE) {
  
  
  loo_set_biases_sel_mat <- loo_set_biases_mat[rows_to_check, , drop = FALSE]
  
  if (verbose_logi && !is.null(target_stake_id)) {
    cat("LOO bias of the target stake:", sprintf("%+.2f", loo_set_biases_sel_mat[1,target_stake_id]), "mm w.e.\n\n")
  }
  
  
  # Check if there are any lucky/fortuitous/opportunistic early solves.
  solved_cur_ids <- which(abs(loo_set_biases_sel_mat) < run_params$optim_bias_threshold, arr.ind = TRUE)
  
  
  # There are no solves in the current set.
  if (nrow(solved_cur_ids) == 0) {
    return(df_loo_out)
  }
  
  # Map back to full matrix, to use indices relative to the full set of realizations.
  solved_cur_ids[, "row"] <- rows_to_check[solved_cur_ids[, "row"]]
  
  # Potentially multiple LOO solutions for a same point (when looking
  # at the main optimization runs together), take the first one.
  solved_cur_first_ids <- solved_cur_ids[!duplicated(solved_cur_ids[, "col"]), , drop = FALSE]
  
  # Skip points already solved
  already_solved_logi <- !is.na(df_loo_out$stake_loo_bias[solved_cur_first_ids[, "col"]])
  
  solved_cur_first_ids <- solved_cur_first_ids[!already_solved_logi, , drop = FALSE]
  if (nrow(solved_cur_first_ids) == 0) {
    return(df_loo_out)
  }
  
  solved_runs_id   <- solved_cur_first_ids[, "row"]
  solved_stakes_id <- solved_cur_first_ids[, "col"]
  
  stakes_loo_bias <- stake_biases_mat[cbind(solved_runs_id, solved_stakes_id)]
  
  
  df_loo_out$stake_loo_bias[solved_stakes_id]   <- stakes_loo_bias
  df_loo_out$loo_run_id[solved_stakes_id]       <- df_runs_biases$run_id[solved_runs_id] # Since the runs are sorted, this is just equal to solved_runs_id, but it is kept to potentially support unsorted runs if ever needed.
  df_loo_out$loo_corr_fact[solved_stakes_id]    <- df_runs_biases$corr_fact[solved_runs_id]
  # We initially assume that all current solves are opportunistic, then we set
  # the proper number of iterations for the point which was our actual target.
  df_loo_out$loo_iterations_n[solved_stakes_id] <- 0
  
  
  solved_n <- length(solved_stakes_id)
  solved_opportunistic_n <- solved_n
  if (!is.null(target_stake_id)) {
    
    # Was the target stake solved at this function call? If yes,
    # mark its number of iterations and decrease opportunistic solves by 1.
    if (target_stake_id %in% solved_stakes_id) {
      df_loo_out$loo_iterations_n[target_stake_id] <- target_iterations_n[target_stake_id]
      solved_opportunistic_n <- solved_opportunistic_n - 1
    }
    
    if (verbose_logi) {
      cat("  LOO validation: solved", solved_n, "stake(s)", paste0("(", paste0(solved_stakes_id, collapse = "|"), ")"), "of which", solved_opportunistic_n, "opportunistic ones.\n")
    }
    
    # Else: target_stake_id is null, so we are doing the LOO check on the main runs.
  } else {
    
    if (verbose_logi) {
      cat("  LOO validation:", solved_n, "stake(s)", paste0("(", paste0(solved_stakes_id, collapse = "|"), ")"), "already solved during the main runs.\n")
    }
  }
  
  return(df_loo_out)
  
}
