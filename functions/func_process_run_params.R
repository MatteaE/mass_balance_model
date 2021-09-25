###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to set additional run parameters beyond those       #
#                 configured in set_params.R. The parameters set here in general should not       #
#                 be modified except by advanced users.                                           #
################################################################################################### 


func_process_run_params <- function(run_params) {
  
  # Should we call save.image() at the end of the model run?
  # Useful for faster debugging.
  run_params$save_simulation_RData <- FALSE
  
  # The base directory for all the data
  run_params$dir_data_base               <-   file.path("input", run_params$name_glacier)
  
  # Set input data paths. We force the user to use these, which are tidy and easy to understand.
  run_params$dir_data_weather            <-   file.path(run_params$dir_data_base, "weather")     # The weather series goes here
  run_params$dir_data_dhm                <-   file.path(run_params$dir_data_base, "dhm")         # Path to the DHM(s) = elevation grids(s) (rectangular, to compute slopes and curvatures)
  run_params$dir_data_surftype           <-   file.path(run_params$dir_data_base, "surftype")    # Path to the grids of surface type (snow/ice/firn/rock/debris) go here
  run_params$dir_data_outline            <-   file.path(run_params$dir_data_base, "outline")     # Path to the outlines
  run_params$dir_data_radiation          <-   file.path(run_params$dir_data_base, "radiation")   # Path to the grids of potential direct radiation (daily sums)
  run_params$dir_data_massbalance        <-   file.path(run_params$dir_data_base, "massbalance") # The mass balance observations go here
  run_params$dir_annual_params           <-   file.path(run_params$dir_data_base, "params")      # The annual model parameter files go here
  
  
  # File names as created by make_input.
  run_params$filename_dhm_prefix         <-   paste0("dhm_", run_params$name_glacier, "_")
  run_params$filename_dhm_suffix         <-   ""                      # DHM name is <prefix><year><suffix>.tif (or .grid or .asc).
  
  run_params$filename_surftype_prefix    <-   paste0("surface_type_", run_params$name_glacier, "_")
  run_params$filename_surftype_suffix    <-   ""                      # Surface type filename is <prefix><year><suffix>.tif (or .grid or .asc).
  
  run_params$filename_radiation_prefix   <-   "dir"
  run_params$filename_radiation_suffix   <-   "24"                    # Radiation files are called <prefix><doy><suffix> where <doy> is the day of year, zero-padded to length 3 (e.g. 001).
  
  run_params$years_input_allowed         <-   1500:2500                     # Years over which we should search for input data.
  run_params$years_input_allowed_n       <-   diff(range(run_params$years_input_allowed)) + 1
  
  #### MODEL OPTIMIZATION parameters ####
  run_params$optim_max_corr_fact         <-   1           # [-]: maximum allowable positive correction to the melt factor and the radiation factor during optimization, in units of the factors themselves (i.e. by how many times these can be increased). Only positive values make sense. A larger value is safer if a reasonable value for the melt factors is not known, but the optimization will be a bit slower. There is no parameter for the negative correction: it is automatically set to maximum 0.
  run_params$optim_bias_threshold        <-   1           # [mm w.e.]: if abs(bias) is below this threshold then we stop the optimization. This saves us a couple iterations since the optim() function will stop when the value *change* is less than a threshold, not the value itself.
  run_params$optim_max_iter              <-   20          # [-]: force mass balance optimization to stop after this number of iterations, even if bias is not within threshold. This is useful in case the optimization is not converging due to avalanches barely reaching a stake, thus a small change in the snow amounts changes a stake's simulated mass balance by a lot, thus bias keeps jumping around 0. In normal conditions, the model converges much faster than 20 iterations.
  
  #### OUTPUT DEM parameters ####
  run_params$output_grid_ext             <-   ".tif"      # extension of the output mass balance grids. Use ?writeFormats to check what is available. Common choices are ".tif" for GeoTiff, and ".asc" for ASCII grid.
  run_params$dem_write                   <-   TRUE        # [TRUE/FALSE]: should we write the annual used DEM to the output directory?
  run_params$filename_dem_prefix         <-   paste0("dem_", run_params$name_glacier, "_") # output DEM name is <prefix><year><output_grid_ext>
  
  #### STAKES parameters ####
  run_params$stakes_unknown_latest_start <-   "2/28"      # [month/day]: in the automatic search of the start date for snow pits and depth probings without a measured start date, we search no later than this day of year. The starting date will be set to the day of the minimum cumulative mass balance between the start of the simulation and the date set here. Something like end of February should be safe for all stakes. 
  run_params$stake_cluster_distance      <-   50          # [m]: threshold distance for clustering stakes together. This is used to ensure a more uniform distribution of the stakes: if measurements are very dense in one place they can induce a bias in the optimization, so we average stakes in clusters. This can reduce the total number of stakes. Only stakes measured on the same days can be clustered. A value of 0 corresponds to no clustering.
  run_params$snow_probes_idw_exp         <-   0.75        # [-]: exponent for the IDW interpolation of winter snow measurements
  
  #### MASS BALANCE PROCESSING parameters ####
  run_params$ele_bands_ela_size          <-   10          # [m]: to compute the equilibrium line altitude, divide the glacier grid into elevation bands with this vertical extent.
  
  #### PLOT parameters ####
  run_params$mb_colorscale_breaks        <-   c(-2,-1.5,-1,-0.5,-0.2,0,0.2,0.5,1,1.5,2) # [m w.e.]: use these breaks in the color scale for mass balance maps. NOTE: these have to be exactly 11 at the moment.
  run_params$ele_bands_plot_size         <-   50          # [m]: plot the annual mass balance profile as function of elevation, using elevation bands with this vertical extent.
  run_params$plot_daily_maps             <-   FALSE       # [TRUE/FALSE]: produce daily plots of mass balance and SWE (slow!).
  
  #### FIXED MASS BALANCE PERIODS choice ####
  # run_params$massbal_fixed_annual_start   <-   "10/31"    # [month/day]: start of the user-defined fixed period for annual mass balance evaluation. This is referred to (<year_cur> - 1).
  # run_params$massbal_fixed_annual_end     <-   "8/31"     # [month/day]: end of the user-defined fixed period for annual mass balance evaluation. This is referred to <year_cur>.
  run_params$massbal_fixed_winter_start  <-   "10/1"      # [month/day]: start of the user-defined fixed period for winter mass balance evaluation. This is referred to (<year_cur> - 1).
  run_params$massbal_fixed_winter_end    <-   "4/30"      # [month/day]: end of the user-defined fixed period for winter mass balance evaluation. This is referred to <year_cur>.
  
  
  #### DERIVED parameters, automatically computed ####
  # The model might not work if you change anything below this line.
  
  # Convert CRS from number to CRS class.
  run_params$grids_crs <- CRS(paste0("EPSG:", run_params$grids_crs))
  
  run_params$years                       <- run_params$first_year:run_params$last_year
  run_params$n_years                     <- length(run_params$years)
  
  run_params$curvature_dhm_smooth        <- max(1e-9,run_params$curvature_dhm_smooth) # The gaussian smoothing fails if sigma   = 0 (but 1e-9 still corresponds to no smoothing!)
  run_params$dhm_smooth_windowsize       <- max(5, 2 * run_params$curvature_dhm_smooth + 1)
  
  run_params$elevation_equal_threshold   <-   1e-3 # [m]: threshold for considering two elevation values equal when we look for problematic flat patches
  
  run_params$ele_bands_auto_min_extent   <- 50 # When automatically computing elevation bands for contour line correction, merge bands which are smaller than this vertical extent in meters.
  
  run_params$model_avalanche_dates       <- format(as.Date(run_params$model_avalanche_dates, format = "%m/%d"), format = "%m/%d") # Add leading zeroes to single-digit values if needed.
  
  run_params$stakes_unknown_latest_start <- format(as.Date(run_params$stakes_unknown_latest_start, format = "%m/%d"), format = "%m/%d") # Same.
  
  # run_params$massbal_fixed_annual_start <- format(as.Date(run_params$massbal_fixed_annual_start, format = "%m/%d"), format = "%m/%d")
  # run_params$massbal_fixed_annual_end <- format(as.Date(run_params$massbal_fixed_annual_end, format = "%m/%d"), format = "%m/%d")
  run_params$massbal_fixed_winter_start <- format(as.Date(run_params$massbal_fixed_winter_start, format = "%m/%d"), format = "%m/%d")
  run_params$massbal_fixed_winter_end <- format(as.Date(run_params$massbal_fixed_winter_end, format = "%m/%d"), format = "%m/%d")
  
  run_params$output_dirname <- file.path("output", run_params$name_glacier)
  
  run_params$size_mult <- 1.183267/3 # To get A4 PDF pages.
  
  return(run_params)
}
