###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to format the output daily data frames with the     #
#                 given precision and formats                                                     #
################################################################################################### 

func_format_df_daily <- function(df_daily,
                                 run_params) {
  
  df_daily_form <- df_daily
  
  df_daily_form$gl_massbal_cumul_bandcorr <- sprintf(run_params$output_fmt4, df_daily_form$gl_massbal_cumul_bandcorr)
  df_daily_form$gl_massbal_cumul          <- sprintf(run_params$output_fmt4, df_daily_form$gl_massbal_cumul)
  df_daily_form$gl_accum_cumul            <- sprintf(run_params$output_fmt4, df_daily_form$gl_accum_cumul)
  df_daily_form$gl_melt_cumul             <- sprintf(run_params$output_fmt4, df_daily_form$gl_melt_cumul)
  df_daily_form$gl_melt_cumul_bandcorr    <- sprintf(run_params$output_fmt4, df_daily_form$gl_melt_cumul_bandcorr)
  df_daily_form$gl_melt_daily_m3          <- sprintf("%.1f", df_daily_form$gl_melt_daily_m3)
  df_daily_form$gl_melt_daily_m3_bandcorr <- sprintf("%.1f", df_daily_form$gl_melt_daily_m3_bandcorr)
  df_daily_form$gl_rainfall_daily_m3      <- sprintf("%.1f", df_daily_form$gl_rainfall_daily_m3)
  df_daily_form$gl_scaf                   <- sprintf("%.2f", df_daily_form$gl_scaf)
  
  return(df_daily_form)
  
}
