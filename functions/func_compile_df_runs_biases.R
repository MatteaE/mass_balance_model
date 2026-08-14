###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to compile one line of the data frame with          #
#                 stake biases from one model run.                                                #
###################################################################################################


func_compile_df_runs_biases <- function(year_data,
                                        mod_output_cur,
                                        run_id,
                                        corr_fact,
                                        run_type) {
  
  df_runs_biases         <- data.frame(run_id    = run_id,
                                       corr_fact = corr_fact,
                                       run_type  = run_type)
  
  # Generate stake names such as s01 etc., with appropriate number of 0s to support the number of stakes.
  stake_id_names      <- paste0("s", sprintf(paste0("%0", nchar(year_data$nstakes_annual), "d"),
                                             1:year_data$nstakes_annual))
  df_runs_biases[stake_id_names] <- mod_output_cur$stakes_bias
  
  return(df_runs_biases)
  
}
