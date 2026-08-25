###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to compile one line of the data frame with          #
#                 SMB results from one model run.                                                 #
###################################################################################################


func_compile_df_runs_smb <- function(year_cur_params,
                                     year_data,
                                     mod_output_cur,
                                     run_id,
                                     corr_fact,
                                     run_type) {
  
  # If there are annual stakes, there is an "annual measurement period" - store its mass balance.
  # Else it is NA.
  if (year_data$nstakes_annual > 0) {
    id_measperiod_start  <- min(mod_output_cur$stakes_start_ids_corr)
    id_measperiod_end    <- max(mod_output_cur$stakes_end_ids)
    mb_annual_measperiod <- mod_output_cur$gl_massbal_cumul[id_measperiod_end] - mod_output_cur$gl_massbal_cumul[id_measperiod_start]
  } else {
    mb_annual_measperiod <- NA_real_
  }
  
  df_runs_smb         <- data.frame(run_id               = run_id,
                                    corr_fact            = corr_fact,
                                    run_type             = run_type,
                                    mb_annual_hydro      = mod_output_cur$gl_massbal_cumul[year_data$id_hydro_end] - mod_output_cur$gl_massbal_cumul[year_data$id_hydro_start],
                                    mb_annual_measperiod = mb_annual_measperiod)
  
  # Generate stake names such as s01 etc., with appropriate number of 0s to support the number of stakes.
  stake_id_names      <- paste0("s", sprintf(paste0("%0", nchar(year_data$nstakes_annual), "d"),
                                             1:year_data$nstakes_annual))
  df_runs_smb[stake_id_names] <- mod_output_cur$stakes_mb_mod
  
  return(df_runs_smb)
  
}
