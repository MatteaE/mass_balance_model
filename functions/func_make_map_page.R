###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which assembles map title and content into a     #
#                 single object.                                                                  #
################################################################################################### 



# The default title_h and legend_h are for the glacier-only plots (mass balance, stake weights).
# Values for the full-grid plots: title_h = 3.0, legend_h = 1.2.
func_make_map_page <- function(title_grob, map_plot, aspect_ratio,
                               page_w = 21, page_h = 29.7, page_margin = 1, title_h = 4.0, legend_h = 1.0) {
  
  # Available width and height on the page
  avail_w <- page_w - 2*page_margin
  avail_h <- page_h - 2*page_margin - title_h - legend_h
  
  # Selected width and height for the plot (must fit the smallest available dimension)
  w_cm <- min(avail_w, avail_h / aspect_ratio)
  h_cm <- w_cm * aspect_ratio
  
  # Update the gtable to the selected width and height
  # (otherwise, it is forcefully aligned at the center of
  # the drawing area, i.e., the location of all figure corners
  # depend on the aspect ratio - instead, like this the top margin
  # is fixed).
  plot_gt <- func_fix_panel_size(map_plot, w_cm, h_cm)
  gt_h    <- as.numeric(convertHeight(sum(plot_gt$heights), "cm"))
  gt_w    <- as.numeric(convertWidth(sum(plot_gt$widths), "cm"))
  
  
  return(cowplot::ggdraw() +
           cowplot::draw_grob(
             title_grob,
             x      = page_margin/page_w,
             y      = 1 - page_margin/page_h,
             width  = avail_w/page_w,
             height = title_h/page_h,
             hjust  = 0,
             vjust  = 1
           ) +
           cowplot::draw_grob(
             plot_gt,
             x      = (page_margin + avail_w/2)/page_w,
             y      = 1 - (page_margin + title_h)/page_h,
             width  = gt_w/page_w,
             height = gt_h/page_h,
             hjust  = 0.5,
             vjust  = 1
           ))
}
