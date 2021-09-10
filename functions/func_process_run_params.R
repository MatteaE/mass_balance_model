###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to process the fixed run parameters set by          #
#                 set_params.R, translating from human to R session.                              #
################################################################################################### 


func_process_run_params <- function(run_params) {
  
  # Convert CRS from number to CRS class.
  run_params$grids_crs <- CRS(paste0("EPSG:", run_params$grids_crs))
  
  
  #### DERIVED parameters, automatically computed: DON'T CHANGE anything below this line ####
  run_params$years                       <- run_params$first_year:run_params$last_year
  run_params$n_years                     <- length(run_params$years)
  
  run_params$curvature_dhm_smooth        <- max(1e-9,run_params$curvature_dhm_smooth) # The gaussian smoothing fails if sigma   = 0 (but 1e-9 still corresponds to no smoothing!)
  run_params$dhm_smooth_windowsize       <- max(5, 2 * run_params$curvature_dhm_smooth + 1)
  
  run_params$elevation_equal_threshold   <-   1e-3 # [m]: threshold for considering two elevation values equal when we look for problematic flat patches
  
  run_params$model_avalanche_dates       <- format(as.Date(run_params$model_avalanche_dates, format = "%m/%d"), format = "%m/%d") # Add leading zeroes to single-digit values if needed.
  
  run_params$stakes_unknown_latest_start <- format(as.Date(run_params$stakes_unknown_latest_start, format = "%m/%d"), format = "%m/%d") # Same.
  
  run_params$massbal_fixed_annual_start <- format(as.Date(run_params$massbal_fixed_annual_start, format = "%m/%d"), format = "%m/%d")
  run_params$massbal_fixed_annual_end <- format(as.Date(run_params$massbal_fixed_annual_end, format = "%m/%d"), format = "%m/%d")
  run_params$massbal_fixed_winter_start <- format(as.Date(run_params$massbal_fixed_winter_start, format = "%m/%d"), format = "%m/%d")
  run_params$massbal_fixed_winter_end <- format(as.Date(run_params$massbal_fixed_winter_end, format = "%m/%d"), format = "%m/%d")
  
  run_params$output_dirname <- file.path("output", run_params$name_glacier)
  
  run_params$size_mult <- 1.183267/3 # To get A4 PDF pages.
  
  return(run_params)
}
