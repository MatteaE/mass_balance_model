###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine which creates a multi-line map title from the    #
#                 given character strings.                                                        #
################################################################################################### 

func_make_map_title <- function(lines_title_l,
                                base_size,
                                line_spacing = 5,    # In pt
                                top_padding  = 0) {  # In pt; this is in addition to the page_margin of func_make_map_page.
  
  fontsize <- c(2*base_size, rep(base_size, length(lines_title_l)-1))
  
  grobs <- vector("list", length(lines_title_l))
  
  y_offset <- top_padding
  
  for (i in seq_along(lines_title_l)) {
    
    grobs[[i]] <- grid::textGrob(
      label = lines_title_l[[i]],
      x = grid::unit(0, "npc"),
      y = grid::unit(1, "npc") - grid::unit(y_offset, "pt"),
      just = c("left", "top"),
      gp = grid::gpar(
        fontsize = fontsize[i],
        fontface = "bold"
      )
    )
    
    y_offset <- y_offset +
      fontsize[i] * 1.2 +
      line_spacing
  }
  
  return(do.call(grid::grobTree, grobs))
  
}
