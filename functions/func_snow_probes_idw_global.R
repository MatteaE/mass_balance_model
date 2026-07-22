###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the global IDW interpolation of snow probing data,           #
#                 using gstat and the prescribed exponent.                                        #
###################################################################################################


func_snow_probes_idw_global <- function(snow_probes_df,
                                        ref_grid,
                                        run_params) {
  
  gs <- gstat(formula=swe~1,
              data=snow_probes_df,
              set=list(idp=run_params$probes_snowdist_idw_exp),
              locations = ~x+y)
  snowdist_idw <- terra::interpolate(ref_grid,
                                     gs,
                                     debug.level = 0)$var1.pred
  
  return(snowdist_idw)
}
