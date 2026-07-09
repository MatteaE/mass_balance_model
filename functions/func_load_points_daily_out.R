###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for the user-defined points where        #
#                 daily model output will be extracted and saved (e.g. for comparison with        #
#                 in situ measurements at high frequency).                                        #
#                 As output we get a data.frame:                                                  #
#                   id x y                                                                        #
################################################################################################### 


func_load_points_daily_out <- function(run_params,
                                       data_dhms) {
  
  # If the user has specified no points of daily output, return empty data frame for them.
  if (nchar(run_params$filename_points_daily_out) == 0) {
    
    data_points_daily_out_dummy <- data.frame(id = character(0),
                                              x = numeric(0),
                                              y = numeric(0))
    return(data_points_daily_out_dummy)
  }
  
  cat("  Loading selected points of daily output...\n")  
  
  points_daily_path <- file.path(run_params$dir_data_massbalance,
                                 run_params$filename_points_daily_out)
  
  if (!file.exists(points_daily_path)) {
    func_customlog("I could not find the file with points of daily output. The specified path is", points_daily_path, "\n", level = 2)
    func_stop_msg()
  }
  
  # Read file, assign column names.
  data_points_daily_out <- read.table(points_daily_path, header = FALSE, stringsAsFactors = FALSE)
  names(data_points_daily_out) <- c("id", "x", "y")
  
  # Check if any point is outside the combined DHM extent.
  # If yes, hard stop since these are user-defined points and
  # we want no surprises.
  ext_limits <- ext(sprc(data_dhms$elevation))
  ids_df_bad <- which((data_points_daily_out$x < xmin(ext_limits)) |
                        (data_points_daily_out$x > xmax(ext_limits)) |
                        (data_points_daily_out$y < ymin(ext_limits)) |
                        (data_points_daily_out$y > ymax(ext_limits)))
  ids_bad_n <- length(ids_df_bad)
  if (ids_bad_n > 0) {
    func_customlog("FATAL: the selected points for daily output include ", ids_bad_n, " entries which fall outside all provided DHMs. Please fix them manually and re-run. The first bad entry is: ", data_points_daily_out$id[ids_df_bad[1]], " ", data_points_daily_out$x[ids_df_bad[1]], " ", data_points_daily_out$y[ids_df_bad[1]], "\n", level = 2)
    func_stop_msg()
  }
  
  return(data_points_daily_out)
  
}
  