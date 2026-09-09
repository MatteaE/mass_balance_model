###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains a helper function to help with the positioning of the map    #
#                 plots.                                                                          #
################################################################################################### 



func_fix_panel_size <- function(pl, w_cm, h_cm) {
  
  gt <- ggplotGrob(pl)
  gt$heights[unique(gt$layout$t[grepl("panel", gt$layout$name)])] <- unit(h_cm, "cm")
  gt$widths[unique(gt$layout$l[grepl("panel", gt$layout$name)])]  <- unit(w_cm, "cm")
  
  return(gt)
  
}