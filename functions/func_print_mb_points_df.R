###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to print (fixed-width) a data frame of measured     #
#                 mass balance points; optionally including their modeled avalanche effect.       #
###################################################################################################


func_print_mb_points_df <- function(points_df,
                                    run_params) {
  
  id_nchar_max <- max(nchar(points_df$id))
  field_widths <- c(min(id_nchar_max + 2, 20), 12, 12, 10, 10, 7, 9)
  
  if ("avalanche_net" %in% names(points_df)) {
    field_widths <- c(field_widths, 15)
  }
  
  func_customlog(paste0(str_pad(names(points_df),
                                field_widths, side = "left", pad = " "), collapse = " "),
                 level = 0)
  # Print aligned table with all relevant info on the points with avalanche effects.
  for (i in 1:nrow(points_df)) {
    
    line_cur <- c(substr(points_df$id[i], 1, 20),
                  format(points_df$start_date[i], "%F"),
                  format(points_df$end_date[i], "%F"),
                  as.character(points_df$x[i]),
                  as.character(points_df$y[i]),
                  as.character(round(points_df$z_dem[i])),
                  sprintf(run_params$output_fmt1, points_df$massbal[i]*run_params$output_mult/1000))
    if ("avalanche_net" %in% names(points_df)) {
      line_cur <- c(line_cur,
                    sprintf(run_params$output_fmt3, points_df$avalanche_net[i]*run_params$output_mult/1000))
    }
    
    
    func_customlog(paste0(str_pad(line_cur,
                                  field_widths,
                                  side = "left",
                                  pad = " "),
                          collapse = " "),
                   level = 0)
    
  } # End loop on the points.
  
  cat("\n")  
  
  
}
