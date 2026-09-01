###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to check the defined parameters (from               #
#                 set_params.R) and set default values for optional params, as well as            #
#                 computing derived parameter values.                                             #
#                 This file should in general not be modified except by advanced users.           #
################################################################################################### 


func_process_run_params <- function(run_params) {
  
  
  # Write supplied parameters ---------------------------------------------------------------------
  func_customlog("Started with ", length(run_params), " run_params:", level = 4)
  run_params_str   <- sapply(run_params, paste0, collapse = ",")
  run_params_table <- paste0(names(run_params_str), str_pad(run_params_str, width = 5 + max(nchar(paste0(names(run_params_str), run_params_str))) - nchar(names(run_params_str)), side = "left"))
  cat(paste0(run_params_table, collapse = "\n"))
  cat("\n")
  
  
  # Check REQUIRED parameters ---------------------------------------------------------------------
  # These must exist from set_params, stop with error if not.
  run_params_required <- c("name_glacier",             # [name as string]: this name is used for the input folder, the output folder, and a lot of files
                           "filename_weather",         # [filename as string]: name of the file with the daily meteorological series
                           "file_weather_nskip",       # [-]: number of header lines to skip in the meteo file. The first non-skipped line should already have the first data entry (no header)
                           "grids_crs",                # [-]: EPSG code as integer - this is the reference system of the grids, used in slope/aspect computations. Overrides any CRS info reported from the grid files.
                           "weather_aws_elevation",    # [m asl]: reference elevation for the meteorological data
                           "first_year",               # [-]: year as integer - the first year to be simulated. In the Northern Hemisphere a year usually goes from September of the previous year (YYYY-1) to September of the specified year (YYYY). In the Southern Hemisphere, from March YYYY-1 to March YYYY.
                           "last_year")                # [-]: year as integer - the last year to be simulated. If same as first_year, a single year is simulated. Else more than 1.
  
  # Check availability.
  run_params_required_missing <- setdiff(run_params_required, names(run_params))
  missing_n                   <- length(run_params_required_missing)
  if (missing_n > 0) {
    func_customlog("There are ", missing_n, " required parameters which are missing. Please define them in set_params.R. They are: ", paste0(run_params_required_missing, collapse = ", "), level = 2)
    func_stop()
  }
  
  
  # Check type.
  run_params_required_classes <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric")
  run_params_required_wrongtype <- run_params_required[which(sapply(run_params[run_params_required], class) != run_params_required_classes)]
  if (length(run_params_required_wrongtype) > 0) {
    func_customlog("There are ", length(run_params_required_wrongtype), " required parameters which have wrong type. Please check them in set_params.R. They are: ", paste0(run_params_required_wrongtype, collapse = ", "), level = 2)
    func_stop()
  }

    
  # Check finite values.
  params_required_numeric_ids <- 3:length(run_params_required)
  ids_val_bad <- which(!is.finite(unlist(run_params[run_params_required[params_required_numeric_ids]])))
  if (length(ids_val_bad) > 0) {
    func_customlog("There are ", length(ids_val_bad), " required parameters which have a wrong value. Please check them in set_params.R. They are: ", paste0(run_params_required[params_required_numeric_ids][ids_val_bad], collapse = ", "), level = 2)
    func_stop()
  }
  
  # Check that first year is not greater than last year.
  if (run_params$first_year > run_params$last_year) {
    func_customlog("Parameter first_year (value: ", run_params$first_year,
                   ") must not be greater than parameter last_year (value: ", run_params$last_year, ")", level = 2)
    func_stop()
  }
  
  
  # Check and set OPTIONAL parameters -------------------------------------------------------------
  # These can be specified in set_params.R or not - the ones which are missing
  # are set from the following defaults, the others are left untouched.
  run_params_optional_defaults <- list(
    
    
    # . (1) Input files parameters ----------------------------
    dir_data_recursive                 = FALSE,       # [TRUE/FALSE]: should the model look recursively for input files in the specified directories?
    dhm_interpolate                    = FALSE,       # [TRUE/FALSE]: should the model do linear interpolation to compute each year's elevation model? Otherwise, the grid closest in time is used.
    
    filename_massbalance_annual        = "",          # [filename as string]: file (under massbal/) with the annual mass balance observations. Can be "" if there are no such data.
    filename_massbalance_winter        = "",          # [filename as string]: file (under massbal/) with the winter mass balance observations. Can be "" if there are no such data.
    filename_points_daily_out          = "",          # [filename as string]: file (under massbal/) with additional point locations where the model should compute and save daily results. Can be "" if there are no such points.
    filename_params_prefix             = "param_",    # [string]: file name of the annual parameters will be <prefix><year><suffix>
    filename_params_suffix             = ".dat",      # [string]: file name of the annual parameters will be <prefix><year><suffix>
    
    
    # . (2) Weather series parameters -------------------------
    weather_snowfall_temp              = 1.5,         # [°C]: at this temperature, precipitation is 50 % rain and 50 % snow. One degree above it is all rain, one degree below it is all snow (snow fraction is linearly interpolated).
    weather_max_precip_ele             = NA,          # [m asl]: above this altitude, precipitation does not increase any more but becomes constant (cutoff). If NA, it is estimated automatically as the 80th percentile of the values in the first DEM grid.
    
    
    # . (3) Mass balance input parameters ---------------------
    stake_cluster_distance             = 20,          # [m]: threshold distance for clustering stakes together. This is used to ensure a more uniform distribution of the stakes: if measurements are very dense in one place they can induce a bias in the optimization, so we average stakes in clusters. This can reduce the total number of stakes. Only stakes measured on the same days can be clustered. A value of 0 corresponds to no clustering.
    stakes_unknown_latest_start        = NA,          # [month/day]: in the automatic search of the start date for snow pits and depth probings without a measured start date, we search no later than this day of year. The starting date will be set to the day of the minimum cumulative mass balance between the start of the simulation and the date set here. If NA, it is set to 28 February in the Northern Hemisphere and 31 August in the Southern Hemisphere.
    
    # These are used in the selection of mass balance measurements of the current year (func_select_year_mb_measurements).
    # They are customizable so that survey dates can be flexibly included/excluded (e.g., monthly stakes).
    # They should be set differently between the Northern and Southern Hemispheres (see func_check_north_south()).
    stake_end_earliest                 = NA,          # [month/day]: a mass balance measurement is included in the set of the current year if the end date of its observation period is at or later than stake_end_earliest. In the Northern Hemisphere, stake_end_earliest with month in [10,12] is interpreted as YYYY-1, else YYYY; in the Southern Hemisphere it is [4,12] YYYY-1. Default value: "12/01" in the North, "06/01" in the South (i.e., a winter stake could be measured already 2 months into the hydrological year).
    stake_end_latest                   = NA,          # [month/day]: a mass balance measurement is included in the set of the current year if the end date of its observation period is at or earlier than stake_end_latest. In the Northern Hemisphere, stake_end_latest is always interpreted as YYYY; in the Southern Hemisphere, stake_end_latest with month in [7,12] is interpreted as YYYY-1, else YYYY. Default value: "11/30" in the North, "05/31" in the South (i.e., an annual stake could be measured up to 2 months after the end of the hydrological year).
    
    
    # . (4) Topographic snow distribution parameters ----------
    curvature_dhm_smooth               = 1.0,         # [cells]: amount of Gaussian smoothing applied before computing curvature (which is very sensitive to DEM noise, unlike slope). Can be non-integer. 1.0 is good for a normal 20 m DEM.
    curvature_cutoff_fact              = 1.2,         # [-]: multiplier for the curvature cutoff threshold at which the snow distribution is not further changed. The threshold is given by the smaller of the two curvature extremes (positive and negative) divided by this factor. Only values >= 1 make sense.
    curvature_effect_limit             = 0.5,         # [-]: maximum effect of curvature, i.e. the curvature multiplier will be within [1 ± curvature_effect_limit]. Only values between 0 and 1 make sense.
    
    elevation_effect_threshold         = NA,          # [m asl]: elevation above which snow accumulation decreases (wind effect). If NA, it is estimated automatically as 95th percentile of the values in the first DEM grid.
    elevation_effect_fact              = 1.0,         # [-]: strength of snow accumulation decrease at very high altitude. Only values between 0 and 1 make sense. At 0 accumulation does not decrease, at 1 accumulation decreases to 0 at the highest point in the DEM.
    
    topographic_snowdist_fact          = 1.0,         # [-]: importance (multiplier) of topographic snow distribution
    
    
    # . (5) Winter probes snow distribution parameters --------
    probes_snowdist_idw_type           = "adaptive",  # ["global"/"adaptive"] type of IDW interpolation: "global" (standard IDW) or "adaptive" (as in IDL)
    probes_snowdist_search_radius_init = 300,         # [m]: for the "adaptive" interpolation: initial search radius
    probes_snowdist_dist_min           = 25,          # [m]: for the "adaptive" interpolation: minimum distance to avoid excessive weighting
    probes_snowdist_search_npoints_min = 4,           # [-]: for the "adaptive" interpolation: minimum number of points to use (otherwise, grow search radius)
    probes_snowdist_idw_exp            = 0.75,        # [-]: exponent for the IDW interpolation of winter snow measurements
    probes_snowdist_smooth_dist        = 25,          # [m]: smoothing radius (in m) for the result of the IDW interpolation
    
    probes_snowdist_fact               = 1.0,         # [-]: importance (multiplier) of probes snow distribution
    
    
    # . (6) Avalanche model parameters ------------------------
    avalanche_routine_cpp              = TRUE,        # [TRUE/FALSE]: should the model use the C++ (TRUE) or R (FALSE) version of the avalanche routine? C++ is much faster but it requires a code compiler
    deposition_slope_lim               = 40,          # [°]: at or above this slope value, snow will not be deposited during an avalanche. A lower value makes avalanches travel farther. Called beta_lim in Gruber (2007).
    deposition_mass_lim                = 2000,        # [mm w.e.]: maximum snow deposition in a (flat) cell during an avalanche. A lower value makes avalanches travel farther. Called D_lim in Gruber (2007).
    movable_slope_lim_lower            = 30,          # [°]: above this slope value, there is a linearly increasing movable fraction in the initial mass distribution, for avalanches. A lower value makes avalanches start also on more gentle slopes.
    movable_slope_lim_upper            = 70,          # [°]: above this slope value, all input snow is movable in the avalanche routine.
    model_avalanche_dates              = "",          # ["mm/dd"]: one or more dates for avalanches during the simulation
    
    
    # . (7) Initial snow cover parameters ---------------------
    initial_snowline_elevation         = NA,          # [m asl]: altitude of the snow line at the start of the simulation
    initial_snow_gradient              = 200,         # [mm w.e. (100 m)-1]: initial SWE gradient above the snowline elevation
    initial_snow_avalanche             = TRUE,        # [TRUE/FALSE]: shall we process the map of initial snow distribution via avalanche, to unload the slopes? This is done at the end of the calculations (i.e., on the map which already includes small-scale and large-scale variability).
    initial_snow_dist_from_model       = FALSE,       # [TRUE/FALSE]: use the result from the previous year's model as starting condition for the current year? This will be respected only if the required model output is available from the previous year (i.e., sequential simulation - not respected if there are annual gaps in the measured mass balances).
    
    
    # . (8) Mass balance model main parameters ----------------
    default_prec_corr                  = 100,         # [%]: default precipitation correction in case no winter data or annual parameters are available.
    default_prec_summer_fact           = 1,           # [-]: default multiplicative reduction of precipitation correction in summer. It can be either 1 number (applied from 1 May to 30 September), or 12 numbers (applied month_wise).
    default_prec_elegrad               = 0,           # [% (100 m)-1]: default altitudinal gradient of precipitation. It can be either 1 number (applied every day of the year), or 12 numbers (applied month-wise). 
    default_temp_elegrad               = -0.65,       # [°C (100 m)-1]: default altitudinal gradient of air temperature. It can be either 1 number (applied every day of the year), or 12 numbers (applied month-wise).
    default_melt_factor                = 4.0,         # [mm w.e. C-1 d-1]: default melt factor for DDF model.
    default_rad_fact_ice               = 0.8,         # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: default radiation factor for ice within DDF model.
    default_rad_fact_snow              = 0.5,         # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: default radiation factor for snow within DDF model.
    nodata_years_automatic             = TRUE,        # [TRUE/FALSE]: if TRUE, the prec_corr/melt_factor/rad_fact_ice/rad_fact_snow parameters for years with no mass balance will be taken as the mean of the parameters optimized over the years with measured mass balance data (only IF there is no overriding value in an annual parameter file AND there is at least one year with measured mass balances). If FALSE, the parameters are taken from the defaults under run_params (only IF there is no overriding value in an annual parameter file).
    params_daily_interp                = "linear",    # ["constant"/"linear"]: how to interpolate the prec_summer_fact, prec_elegrad and temp_elegrad from 12 monthly values (if provided) to the daily series. "constant" is kept constant within each month, "linear" interpolates through the month midpoints.
    
    
    # . (9) Additional melt model parameters ------------------
    debris_red_fac                     = 0.6,         # [-]: reduction factor of melt over debris-covered ice.
    albedo_ice_decrease_elev           = 0.,          # [m asl]: below this altitude, the ice albedo decreases linearly with altitude (darker ice).
    albedo_ice_decrease_fact           = 0.014,       # [m-1]: rate of increase above 1 (with decreasing altitude) of the ice albedo factor (multiplying ice melt).
    
    
    # . (10) Optimization parameters ---------------------------
    optim_bias_threshold               = 1,           # [mm w.e.]: optimization stops when abs(global bias) is below this threshold
    optim_max_iter                     = 20,          # [-]: force mass balance optimization to stop after this number of iterations, even if bias is not within threshold. This is useful in case the optimization is not converging due to avalanches barely reaching a stake, thus a small change in the snow amounts changes a stake's simulated mass balance by a lot, thus bias keeps jumping around 0. In normal conditions, the model converges much faster than 20 iterations.
    optim_winter_areaweight_fact       = 0.0,         # [-]: area weighting of each mass balance point in the winter optimization. 0 (default): uniform weight; 1: weight is the area of the Voronoi cell of each point. Between 0 and 1: linear interpolation.
    optim_annual_areaweight_fact       = 0.0,         # [-]: area weighting of each mass balance point in the annual optimization. 0 (default): uniform weight; 1: weight is the area of the Voronoi cell of each point. Between 0 and 1: linear interpolation.
    
    
    # . (11) Mass balance postprocessing parameters -------
    ele_bands_auto_min_extent          = 50,          # [m]: when automatically computing elevation bands for local correction, merge bands which are smaller than this vertical extent.
    ele_bands_ela_size                 = 10,          # [m]: to compute the equilibrium line altitude, divide the glacier grid into elevation bands with this vertical extent.
    massbal_fixed_winter_start         = NA,          # [month/day or NA]: start of the fixed "winter" period for mass balance evaluation. If NA, this will be 1 October (YYYY-1) in the Northern Hemisphere and 1 April (YYYY-1) in the Southern Hemisphere. Note: this MM/DD date is always assigned to YYYY-1.
    massbal_fixed_winter_end           = NA,          # [month/day or NA]: end of the fixed "winter" period for mass balance evaluation. If NA, this will be 30 April (YYYY) in the Northern Hemisphere and 31 October (YYYY-1) in the Southern Hemisphere. Note: this MM/DD date is assigned to YYYY in the Northern Hemisphere and to YYYY-1 in the Southern Hemisphere.
    
    
    # . (12) Leave-one-out validation parameters ---------------
    run_loo_validation                 = FALSE,       # [TRUE/FALSE]: run leave-one-out validation of the stakes.
    loo_stake_iter_max_n               = 10,          # [-]: maximum number of iterations to achieve the LOO validation of one stake.
    
    
    # . (13) Plot parameters -----------------------------------
    mb_colorscale_multiplier           = 1,           # [-] Use NA for automatic colorscale limits, or give a number to multiply the colorscale limits. 1 = colorscale between -2 and +2 m w.e.
    output_unit                        = "m",         # ["m"/"mm"]: "m" for meters water-equivalent, or "mm" for millimeters water-equivalent. Applies to all output files (results and plots).
    show_contours                      = TRUE,        # [TRUE/FALSE]: in the mass balance maps, show the contour lines (TRUE) or not (FALSE).
    show_contour_labels                = TRUE,        # [TRUE/FALSE]: in the mass balance maps, show the labels of contour lines (TRUE) or not (FALSE).
    show_stake_labels                  = TRUE,        # [TRUE/FALSE]: in the mass balance maps, show the labels of stake measurements (TRUE) or not (FALSE).
    show_month_lines                   = TRUE,        # [TRUE/FALSE]: in the time series plots (mass balance and meteo series), show vertical lines to divide the months (TRUE) or not (FALSE).
    outlines_linesize_mult             = 1.0,         # [-]: multiplier for the thickness of the glacier outlines in maps.
    
    
    # . (14) General output parameters -------------------------
    save_simulation_RData              = FALSE,       # [TRUE/FALSE]: call save.image() at the end of the model run? Can be useful for debugging.
    overwrite_output                   = TRUE,        # [TRUE/FALSE]: what to do if the output directory is already there? TRUE: overwrite with warning. FALSE: stop with error
    output_grid_ext                    = ".tif",      # [".tif"/".asc"/...]: extension of the output grid files. Use ?writeFormats to check what is available. Common choices are ".tif" for GeoTiff, and ".asc" for ASCII grid.
    dem_write                          = TRUE,        # [TRUE/FALSE]: should the model write the annual used DEM to the output directory?
    
    
    # . (15) Daily output parameters ---------------------------
    plot_daily_maps_winter             = FALSE,      # [TRUE/FALSE]: at the end of each year, produce plots of daily surface type and SWE from the winter simulation (slow, but useful for debug or visualization).
    plot_daily_maps_winter_freq        = 1,          # [days]: produce "daily" plots of surface type and SWE from winter simulation only at a given interval, to speed up their generation.
    write_daily_grids_winter           = FALSE,      # [TRUE/FALSE]: at the end of each year, write daily geotiff grids of SWE from winter simulation (useful for debugging)
    write_daily_grids_winter_freq      = 1,          # [days]: write "daily" grids of SWE from winter simulation only at a given interval, to speed up processing
    daily_massbal_winter_refdate       = "11/01",    # [month/day] or "": use this day as reference for the winter cumulative mass balance grids. The grid of that day will be subtracted from all others. If set to "", no subtraction takes place (i.e., it will be the first grid of cumulative mass balance to have 0.0 everywhere). In the Northern Hemisphere, dates from 1 July are interpreted as YYYY-1, before that as YYYY. In the Southern Hemisphere, always as YYYY-1.
    
    plot_daily_maps_annual             = FALSE,      # [TRUE/FALSE]: at the end of each year, produce plots of daily surface type and SWE from the annual simulation (slow, but useful for debug or visualization).
    plot_daily_maps_annual_freq        = 1,          # [days]: produce "daily" plots of surface type and SWE from annual simulation only at a given interval, to speed up their generation.
    write_daily_grids_annual           = FALSE,      # [TRUE/FALSE]: at the end of each year, write daily geotiff grids of SWE from annual simulation (useful for debugging)
    write_daily_grids_annual_freq      = 1,          # [days]: write "daily" grids of SWE from annual simulation only at a given interval, to speed up processing
    daily_massbal_annual_refdate       = "11/01"     # [month/day] or "": use this day as reference for the summer cumulative mass balance grids. The grid of that day will be subtracted from all others. If set to "", no subtraction takes place (i.e., it will be the first grid of cumulative mass balance to have 0.0 everywhere). In the Northern Hemisphere, dates from 1 July are interpreted as YYYY-1, before that as YYYY. In the Southern Hemisphere, always as YYYY-1.
    
  )
  
  
  # Set default values for missing parameters.
  run_params_missing             <- setdiff(names(run_params_optional_defaults), names(run_params))
  run_params[run_params_missing] <- run_params_optional_defaults[run_params_missing]
  
  missing_n <- length(run_params_missing)
  if (missing_n > 0) {
    func_customlog("Using default value for ", missing_n, " optional parameters which were not defined.", level = 0)
  } else {
    func_customlog("All optional parameters were defined.", level = 0)
  }
  cat("\n")
  
  
  # Define fixed INPUT FILES parameters -----------------------------------------------------------
  # In general these should not be changed.
  
  # The base directory for all the data.
  run_params$dir_data_base               <-   file.path("input", run_params$name_glacier)
  
  # Set input data paths. We force this folder structure which provides good data management.
  run_params$dir_data_dhm                <-   file.path(run_params$dir_data_base, "dhm")         # Path to the DHM(s) = elevation grids(s) (rectangular, to compute slopes and curvatures)
  run_params$dir_data_massbalance        <-   file.path(run_params$dir_data_base, "massbalance") # The mass balance observations go here
  run_params$dir_data_outline            <-   file.path(run_params$dir_data_base, "outline")     # Path to the outlines
  run_params$dir_data_params             <-   file.path(run_params$dir_data_base, "params")      # The annual model parameter files go here
  run_params$dir_data_radiation          <-   file.path(run_params$dir_data_base, "radiation")   # Path to the grids of potential direct radiation (daily sums)
  run_params$dir_data_snowdist           <-   file.path(run_params$dir_data_base, "snowdist")    # Path to the external grids of snow distribution
  run_params$dir_data_surftype           <-   file.path(run_params$dir_data_base, "surftype")    # Path to the grids of surface type (snow/ice/firn/rock/debris) go here
  run_params$dir_data_weather            <-   file.path(run_params$dir_data_base, "weather")     # The weather series goes here
  
  
  # File names as created by make_input.
  run_params$filename_dhm_prefix         <-   paste0("dhm_", run_params$name_glacier, "_")
  run_params$filename_dhm_suffix         <-   ""                      # DHM name is <prefix><year><suffix>.tif (or .grid or .asc).
  
  run_params$filename_surftype_prefix    <-   paste0("surface_type_", run_params$name_glacier, "_")
  run_params$filename_surftype_suffix    <-   ""                      # Surface type filename is <prefix><year><suffix>.tif (or .grid or .asc).
  
  run_params$filename_radiation_prefix   <-   "dir"
  run_params$filename_radiation_suffix   <-   "24"                    # Radiation files are called <prefix><doy><suffix> where <doy> is the day of year, zero-padded to length 3 (e.g. 001).
  
  run_params$filename_outline_prefix     <-   paste0("outline_", run_params$name_glacier, "_")
  run_params$filename_outline_suffix     <-   ""                      # Outline name is <prefix><year><suffix>.shp (or .xyzn).
  
  run_params$years_input_allowed         <-   1500:2500                     # Years over which we should search for input data.
  run_params$years_input_allowed_n       <-   diff(range(run_params$years_input_allowed)) + 1
  
  
  
  
  # Process OUTPUT parameters ---------------------------------------------------------------------
  # If output unit is invalid, use meters water equivalent.
  if (!(run_params$output_unit %in% c("mm", "m"))) {
    func_customlog("Output unit specification is malformed, defaulting to meters water equivalent", level = 1)
    run_params$output_unit <- "m"
  }
  
  # Adapt formats to chosen m w.e. or mm w.e.
  if (run_params$output_unit == "m") {
    run_params$output_mult <- 1
    run_params$output_fmt1 <- "%.3f" # For sprintf()
    run_params$output_fmt2 <- "%.2f"
    run_params$output_fmt3 <- "%+.3f"
    run_params$output_fmt4 <- "%.4f"
  } else {
    run_params$output_mult <- 1000
    run_params$output_fmt1 <- "%.0f"
    run_params$output_fmt2 <- "%.0f"
    run_params$output_fmt3 <- "%+.0f"
    run_params$output_fmt4 <- "%.1f"
  }
  
  run_params$filename_dem_prefix <- paste0("dem_", run_params$name_glacier, "_") # output DEM name is <prefix><year><output_grid_ext>
  
  
  
  # Define fixed PLOT parameters ------------------------------------------------------------------
  run_params$mb_colorscale_breaks        <- c(-2,-1.5,-1,-0.5,-0.2,0,0.2,0.5,1,1.5,2) # [m w.e.]: use these breaks in the color scale for mass balance maps. NOTE: these have to be exactly 11.
  run_params$mb_colorscale_breaks        <- run_params$mb_colorscale_breaks * run_params$output_mult
  run_params$ele_bands_plot_size         <- 50          # [m]: plot the annual mass balance profile as function of elevation, using elevation bands with this vertical extent.
  run_params$size_mult                   <- 1.183267/3 # To get A4 PDF pages.
  
  
  
  # Set additional DERIVED parameters, automatically computed -------------------------------------
  # The model might not work if anything is changed below this line.
  
  # Convert CRS from number to string.
  run_params$grids_crs_epsg              <- paste0("EPSG:", run_params$grids_crs)
  
  run_params$years                       <- run_params$first_year:run_params$last_year
  run_params$n_years                     <- length(run_params$years)
  
  run_params$curvature_dhm_smooth        <- max(1e-9,run_params$curvature_dhm_smooth) # The gaussian smoothing fails if sigma   = 0 (but 1e-9 still corresponds to no smoothing!)
  run_params$dhm_smooth_windowsize       <- max(5, 2 * run_params$curvature_dhm_smooth + 1)
  
  run_params$elevation_equal_threshold   <- 1e-3 # [m]: threshold for considering two elevation values equal when we look for problematic flat patches
  run_params$avalanche_effect_threshold  <- 1e-9 # [mm w.e.]: threshold for considering nonzero avalanche effect
  
  run_params$model_avalanche_dates       <- format(as.Date(paste0("2000/", run_params$model_avalanche_dates), format = "%Y/%m/%d"), format = "%m/%d") # Add leading zeroes to single-digit values if needed. Use 2000 as dummy year for that (but it is not recommended to set avalanches on 29 February!).
  if (any(is.na(run_params$model_avalanche_dates))) {
    func_customlog("Invalid value(s) for parameter model_avalanche_dates in set_params.R. Please check it.", level = 2)
    func_stop()
  }
  
  
  run_params$stakes_unknown_latest_start <- format(as.Date(run_params$stakes_unknown_latest_start, format = "%m/%d"), format = "%m/%d") # Same.
  
  run_params$massbal_fixed_winter_start  <- format(as.Date(run_params$massbal_fixed_winter_start, format = "%m/%d"), format = "%m/%d")
  run_params$massbal_fixed_winter_end    <- format(as.Date(run_params$massbal_fixed_winter_end, format = "%m/%d"), format = "%m/%d")
  
  
  # Directory with daily outputs
  run_params$out_daily_dirpath <- file.path(run_params$output_dirname, "daily_results")
  dir.create(run_params$out_daily_dirpath, showWarnings = F, recursive = T)
  
  # Directory with gridded annual outputs
  run_params$out_annual_gridded_dirpath <- file.path(run_params$output_dirname, "annual_results", "gridded")
  dir.create(run_params$out_annual_gridded_dirpath, showWarnings = F, recursive = T)
  
  
  
  #### . Process parameters which can be monthly or annual ----------------------------------------
  # If we have just 1 value for the summer precipitation coefficient,
  # apply it for May to September. Otherwise, use the 12 supplied values.
  if (!(length(run_params$default_prec_summer_fact) %in% c(1,12))) {
    func_customlog("Parameter default_prec_summer_fact in set_params.R must be either one single (annual) value, or 12 comma-separated monthly values. Value(s) provided: ", paste0(run_params$default_prec_summer_fact, collapse = " "), "\n", level = 2)
    func_stop()
  } else {
    if (length(run_params$default_prec_summer_fact) == 1) {
      run_params$default_prec_summer_fact <- c(rep(1.0, 4),
                                               rep(run_params$default_prec_summer_fact, 5),
                                               rep(1.0, 3))
    }
  }
  
  
  if (!(run_params$params_daily_interp %in% c("constant", "linear"))) {
    func_customlog("Parameter params_daily_interp in set_params.R must be either \"constant\" or \"linear\" (value provided: ", run_params$params_daily_interp, ")", level = 2)
    func_stop()
  }
  
  
  # If we have just 1 value for the default prec_elegrad or temp_elegrad,
  # repeat 12 times (to support month-wise lapse rates while also allowing
  # user input of just 1 annual value - easier and backwards-compatible).
  if (!(length(run_params$default_prec_elegrad) %in% c(1,12))) {
    func_customlog(paste0("Parameter default_prec_elegrad in set_params.R must be either one single (annual) value, or 12 comma-separated monthly values. Value(s) provided: ", paste0(run_params$default_prec_elegrad, collapse = " "), "\n"), level = 2)
    func_stop()
  } else {
    if (length(run_params$default_prec_elegrad) == 1) {
      run_params$default_prec_elegrad <- rep(run_params$default_prec_elegrad, 12)
    }
  }
  if (!(length(run_params$default_temp_elegrad) %in% c(1,12))) {
    func_customlog(paste0("Parameter default_temp_elegrad in set_params.R must be either one single (annual) value, or 12 comma-separated monthly values. Value(s) provided: ", paste0(run_params$default_temp_elegrad, collapse = " "), "\n"), level = 2)
    func_stop()
  } else {
    if (length(run_params$default_temp_elegrad) == 1) {
      run_params$default_temp_elegrad <- rep(run_params$default_temp_elegrad, 12)
    }
  }
  
  
  return(run_params)
}
