###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the fixed parameter definitions for the model run.           #
#                 It is an example file with all the fixed parameters which can be possibly set.  #
#                 The provided values are examples only, to understand the parameter type.        #
#                 Additional year-specific parameters can be set via the annual param files.      #
###################################################################################################

run_params <- list(
  
  # . (0) Required parameters -------------------------------
  name_glacier                      = "glacier_name",      # [name in "quotes"]: glacier name, which is used for the input folder, the output folder, and a lot of files
  filename_weather                  = "weather_data.dat",  # [filename in "quotes"]: name of the file (under weather/) with the daily meteorological series
  file_weather_nskip                = 4,                   # [-]: number of header lines to skip in the meteo file. The first non-skipped line should already have the first data entry (no header)
  weather_aws_elevation             = 3500,                # [m asl]: reference elevation for the meteorological data
  grids_crs                         = 32642,               # [-]: EPSG code as integer - this is the reference system of the grids, used in slope/aspect computations. Overrides any CRS info reported from the grid files.
  first_year                        = 2018,                # [-]: the first year to be processed. In the Northern Hemisphere a year usually goes from September of the previous year (YYYY-1) to September of the specified year (YYYY). In the Southern Hemisphere, from March YYYY-1 to March YYYY.
  last_year                         = 2020,                # [-]: the last year to be processed. If same as first_year, a single year is simulated. Else the model covers multiple years.
  
  
  # . (1) Input files parameters ----------------------------
  dir_data_recursive                 = FALSE,              # [TRUE/FALSE]: should the model look recursively for input files in the specified directories?
  dhm_interpolate                    = FALSE,              # [TRUE/FALSE]: should the model do linear interpolation to compute each year's elevation model? Otherwise, the grid closest in time is used.
  
  filename_massbalance_annual        = "mb_annual.dat",    # [filename in "quotes"]: file (under massbal/) with the annual mass balance observations. Can be "" if there are no such data.
  filename_massbalance_winter        = "mb_winter.dat",    # [filename in "quotes"]: file (under massbal/) with the winter mass balance observations. Can be "" if there are no such data.
  filename_points_daily_out          = "mb_pts_out.dat",   # [filename in "quotes"]: file (under massbal/) with additional point locations where the model should compute and save daily results. Can be "" if there are no such points.
  filename_params_prefix             = "param_",           # [string]: file name of the annual parameters will be <prefix><year><suffix>
  filename_params_suffix             = ".dat",             # [string]: file name of the annual parameters will be <prefix><year><suffix>
  
  
  # . (2) Weather series parameters -------------------------
  weather_snowfall_temp              = 1.5,                # [°C]: at this temperature, precipitation is 50 % rain and 50 % snow. One degree above it is all rain, one degree below it is all snow (snow fraction is linearly interpolated).
  weather_max_precip_ele             = 4000,               # [m asl]: above this altitude, precipitation does not increase any more but becomes constant (cutoff). If NA, it is estimated automatically as the 80th percentile of the values in the first DEM grid.
  
  
  # . (3) Mass balance input parameters ---------------------
  stake_cluster_distance             = 20,                 # [m]: threshold distance for clustering stakes together. This is used to ensure a more uniform distribution of the stakes: if measurements are very dense in one place they can induce a bias in the optimization, so we average stakes in clusters. This can reduce the total number of stakes. Only stakes measured on the same days can be clustered. A value of 0 corresponds to no clustering.
  stakes_unknown_latest_start        = "2/28",             # [month/day in "quotes"]: in the automatic search of the start date for snow pits and depth probings without a measured start date, we search no later than this date. The starting date will be set to the day of the minimum cumulative mass balance between the start of the simulation and the date set here. If not set or NA, it is set to 28 February in the Northern Hemisphere and 31 August in the Southern Hemisphere.
  stake_duration_min_n               = 30,                 # [days]: stop with error if any stakes (annual or winter) have an observation period shorter than this duration.
  
  # These two parameters are used in the selection of mass balance measurements of the current year (func_select_year_mb_measurements()).
  # They are customizable so that various survey dates can be flexibly included/excluded (e.g., monthly stakes).
  # They should be set differently between the Northern and Southern Hemispheres (see func_check_north_south()).
  stake_end_earliest                 = "12/01",            # [month/day in "quotes"]: a mass balance measurement is included in the set of the current year if the end date of its observation period is at or later than stake_end_earliest. In the Northern Hemisphere, stake_end_earliest with month in [10,12] is interpreted as YYYY-1, else YYYY; in the Southern Hemisphere it is [4,12] YYYY-1. Default value if not set or NA: "12/01" in the North, "06/01" in the South (i.e., a winter stake could be measured already 2 months into the hydrological year).
  stake_end_latest                   = "11/30",            # [month/day in "quotes"]: a mass balance measurement is included in the set of the current year if the end date of its observation period is at or earlier than stake_end_latest. In the Northern Hemisphere, stake_end_latest is always interpreted as YYYY; in the Southern Hemisphere, stake_end_latest with month in [7,12] is interpreted as YYYY-1, else YYYY. Default value if not set or NA: "11/30" in the North, "05/31" in the South (i.e., an annual stake could be measured up to 2 months after the end of the hydrological year).
  
  
  # . (4) Topographic snow distribution parameters ----------
  curvature_dhm_smooth               = 1.0,                # [cells]: amount of Gaussian smoothing applied before computing curvature (which is very sensitive to DEM noise, unlike slope). Can be non-integer. 1.0 is good for a normal 20 m DEM.
  curvature_cutoff_fact              = 1.2,                # [-]: multiplier for the curvature cutoff threshold at which the snow distribution is not further changed. The threshold is given by the smaller of the two curvature extremes (positive and negative) divided by this factor. Only values >= 1 make sense.
  curvature_effect_limit             = 0.5,                # [-]: maximum effect of curvature, i.e. the curvature multiplier will be within [1 ± curvature_effect_limit]. Only values between 0 and 1 make sense.
  
  elevation_effect_threshold         = 4500,               # [m asl]: elevation above which snow accumulation decreases (wind effect). If NA, it is estimated automatically as 95th percentile of the values in the first DEM grid.
  elevation_effect_fact              = 1.0,                # [-]: strength of snow accumulation decrease at very high altitude. Only values between 0 and 1 make sense. At 0 accumulation does not decrease, at 1 accumulation decreases to 0 at the highest point in the DEM.
  
  topographic_snowdist_fact          = 1.0,                # [-]: importance (multiplier) of topographic snow distribution. 0 = no topographic control on the relative snow distribution. 1 = full-strength topographic control on the relative snow distribution.
  
  
  # . (5) Winter probes snow distribution parameters --------
  probes_snowdist_idw_type           = "adaptive",         # ["global"/"adaptive"] type of IDW interpolation: "global" (standard IDW) or "adaptive" (as in IDL)
  probes_snowdist_search_radius_init = 300,                # [m]: for the "adaptive" interpolation: initial search radius
  probes_snowdist_dist_min           = 25,                 # [m]: for the "adaptive" interpolation: minimum distance to avoid excessive weighting
  probes_snowdist_search_npoints_min = 4,                  # [-]: for the "adaptive" interpolation: minimum number of points to use (otherwise, grow search radius)
  probes_snowdist_idw_exp            = 0.75,               # [-]: exponent for the IDW interpolation of winter snow measurements
  probes_snowdist_smooth_dist        = 25,                 # [m]: smoothing radius (in m) for the result of the IDW interpolation
  
  probes_snowdist_fact               = 1.0,                # [-]: importance (multiplier) of probes snow distribution. 0 = no effect of winter measurements on the relative snow distribution. 1 = full-strength control of winter measurements on the relative snow distribution.
  
  
  # . (6) Avalanche model parameters ------------------------
  avalanche_routine_cpp              = TRUE,               # [TRUE/FALSE]: should the model use the C++ (TRUE) or R (FALSE) version of the avalanche routine? C++ is much faster but it requires a code compiler
  deposition_slope_lim               = 40,                 # [°]: at or above this slope value, snow will not be deposited during an avalanche. A lower value makes avalanches travel farther. Called beta_lim in Gruber (2007).
  deposition_mass_lim                = 2000,               # [mm w.e.]: maximum snow deposition in a (flat) cell during an avalanche. A lower value makes avalanches travel farther. Called D_lim in Gruber (2007).
  movable_slope_lim_lower            = 30,                 # [°]: above this slope value, there is a linearly increasing movable fraction in the initial mass distribution, for avalanches. A lower value makes avalanches start also on more gentle slopes.
  movable_slope_lim_upper            = 70,                 # [°]: above this slope value, all input snow is movable in the avalanche routine.
  model_avalanche_dates              = c("3/31", "5/31"),  # [month/day in "quotes"]: one or more dates for avalanches during the simulation. To skip avalanches, use NA or do not provide this parameter.
  
  
  # . (7) Initial snow cover parameters ---------------------
  initial_snowline_elevation         = NA,                 # [m asl]: altitude of the snow line at the start of the simulation. If NA or missing, the 70th percentile of glacier altitude is used.
  initial_snow_gradient              = 200,                # [mm w.e. (100 m)-1]: initial SWE gradient above the snowline elevation
  initial_snow_avalanche             = TRUE,               # [TRUE/FALSE]: shall we process the map of initial snow distribution via avalanche, to unload the slopes? This is done at the end of the calculations (i.e., on the map which already includes small-scale and large-scale variability).
  initial_snow_dist_from_model       = TRUE,               # [TRUE/FALSE]: use the result from the previous year's model as starting condition for the current year? This will be respected only if the required model output is available from the previous year (i.e., sequential simulation - not respected for the years which are not simulated sequentially (i.e. which have gaps in the measured mass balances - those are processed at the end).
  
  
  # . (8) Mass balance model main parameters ----------------
  default_prec_corr                  = 100,                # [%]: default correction of the measured precipitation. 100 % = no correction, 200 % = double the precipitation. This parameter can be overridden by annual parameter files or by optimization against winter measurements.
  default_prec_summer_fact           = 1,                  # [-]: default multiplicative reduction of precipitation correction in summer. It can be either 1 number (applied from 1 May to 30 September), or 12 numbers (applied month-wise or with daily linear interpolation). This parameter can be overridden by annual parameter files.
  default_prec_elegrad               = 0,                  # [% (100 m)-1]: default altitudinal gradient of precipitation. It can be either 1 number (applied every day of the year), or 12 numbers (applied month-wise or with daily linear interpolation). This parameter can be overridden by annual parameter files.
  default_temp_elegrad               = -0.65,              # [°C (100 m)-1]: default altitudinal gradient of air temperature. It can be either 1 number (applied every day of the year), or 12 numbers (applied month-wise or with daily linear interpolation). This parameter can be overridden by annual parameter files.
  default_melt_factor                = 4.0,                # [mm w.e. C-1 d-1]: default melt factor for DDF model. This parameter can be overridden by annual parameter files or by optimization against annual measurements.
  default_rad_fact_ice               = 0.8,                # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: default radiation factor for ice within DDF model. This parameter can be overridden by annual parameter files or by optimization against annual measurements.
  default_rad_fact_snow              = 0.5,                # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: default radiation factor for snow within DDF model. This parameter can be overridden by annual parameter files or by optimization against annual measurements.
  nodata_years_automatic             = TRUE,               # [TRUE/FALSE]: if TRUE, the prec_corr/melt_factor/rad_fact_ice/rad_fact_snow parameters for years with no mass balance will be taken as the mean of the parameters optimized over the years with measured mass balance data (only IF there is no overriding value in an annual parameter file AND there is at least one year with measured mass balances). If FALSE, the parameters are taken from the defaults under run_params (only IF there is no overriding value in an annual parameter file).
  params_daily_interp                = "linear",           # ["constant"/"linear"]: how to interpolate the prec_summer_fact, prec_elegrad and temp_elegrad from 12 monthly values (if provided) to the daily series. "constant" is kept constant within each month, "linear" interpolates through the month midpoints.
  
  
  # . (9) Additional melt model parameters ------------------
  debris_red_fac                     = 0.6,                # [-]: reduction factor of melt over debris-covered ice.
  albedo_ice_decrease_elev           = 0,                  # [m asl]: below this altitude, the ice albedo decreases linearly with altitude (darker ice).
  albedo_ice_decrease_fact           = 0.014,              # [m-1]: rate of increase above 1 (with decreasing altitude) of the ice albedo factor (multiplying ice melt).
  
  
  # . (10) Optimization parameters ---------------------------
  optim_bias_threshold               = 1,                  # [mm w.e.]: optimization stops when abs(mean bias) is below this threshold
  optim_max_iter                     = 20,                 # [-]: force mass balance optimization to stop after this number of iterations, even if bias is not within threshold. This is useful in case the optimization is not converging due to avalanches barely reaching a stake, thus a small change in the snow amounts changes a stake's simulated mass balance by a lot, thus bias keeps jumping around 0. In normal conditions, the model converges much faster than 20 iterations.
  optim_winter_areaweight_fact       = 0.0,                # [-]: area weighting of each mass balance point in the winter optimization. 0 (default): uniform weight; 1: weight is the area of the Voronoi cell of each point. Between 0 and 1: linear interpolation.
  optim_annual_areaweight_fact       = 0.0,                # [-]: area weighting of each mass balance point in the annual optimization. 0 (default): uniform weight; 1: weight is the area of the Voronoi cell of each point. Between 0 and 1: linear interpolation.
  
  
  # . (11) Mass balance postprocessing parameters -------
  ele_bands_auto_min_extent          = 50,                 # [m]: when automatically computing elevation bands for local correction, merge bands which are smaller than this vertical extent.
  ele_bands_ela_size                 = 10,                 # [m]: to compute the equilibrium line altitude, divide the glacier grid into elevation bands with this vertical extent.
  massbal_fixed_winter_start         = "10/1",             # [month/day in "quotes"]: start of the fixed "winter" period for mass balance evaluation. If NA, this will be 1 October (YYYY-1) in the Northern Hemisphere and 1 April (YYYY-1) in the Southern Hemisphere. Note: this MM/DD date is always assigned to YYYY-1.
  massbal_fixed_winter_end           = "4/30",             # [month/day in "quotes"]: end of the fixed "winter" period for mass balance evaluation. If NA, this will be 30 April (YYYY) in the Northern Hemisphere and 31 October (YYYY-1) in the Southern Hemisphere. Note: this MM/DD date is assigned to YYYY in the Northern Hemisphere and to YYYY-1 in the Southern Hemisphere.
  
  
  # . (12) Leave-one-out validation parameters ---------------
  run_loo_validation                 = FALSE,              # [TRUE/FALSE]: run leave-one-out validation of the stakes.
  loo_stake_iter_max_n               = 10,                 # [-]: maximum number of iterations to achieve the LOO validation of one stake.
  
  
  # . (13) Plot parameters -----------------------------------
  mb_colorscale_multiplier           = 1,                  # [-]: a number to multiply the colorscale limits. 1 = colorscale between -2 and +2 m w.e.; use NA for automatic colorscale limits
  output_unit                        = "m",                # ["m"/"mm"]: "m" for meters water-equivalent, or "mm" for millimeters water-equivalent. Applies to all output files (results and plots).
  show_contours                      = TRUE,               # [TRUE/FALSE]: in the mass balance maps, show the contour lines (TRUE) or not (FALSE).
  show_contour_labels                = TRUE,               # [TRUE/FALSE]: in the mass balance maps, show the labels of contour lines (TRUE) or not (FALSE).
  show_stake_labels                  = TRUE,               # [TRUE/FALSE]: in the mass balance maps, show the labels of stake measurements (TRUE) or not (FALSE).
  show_month_lines                   = TRUE,               # [TRUE/FALSE]: in the time series plots (mass balance and meteo series), show vertical lines to divide the months (TRUE) or not (FALSE).
  outlines_linesize_mult             = 1.0,                # [-]: multiplier for the thickness of the glacier outlines in map plots.
  
  
  # . (14) General output parameters -------------------------
  save_simulation_RData              = FALSE,              # [TRUE/FALSE]: call save.image() at the end of the model run? Can be useful for debugging.
  overwrite_output                   = TRUE,               # [TRUE/FALSE]: what to do if the output directory is already there? TRUE: overwrite with warning. FALSE: stop with error
  output_grid_ext                    = ".tif",             # [".tif"/".asc"/...]: extension of the output grid files. Use ?writeFormats to check what is available. Common choices are ".tif" for GeoTiff, and ".asc" for ASCII grid.
  dem_write                          = TRUE,               # [TRUE/FALSE]: should the model write the annual used DEM to the output directory?
  
  
  # . (15) Daily output parameters ---------------------------
  plot_daily_maps_winter             = FALSE,              # [TRUE/FALSE]: at the end of each year, produce plots of daily surface type and SWE from the winter simulation (slow, but useful for debug or visualization).
  plot_daily_maps_winter_freq        = 1,                  # [days]: produce "daily" plots of surface type and SWE from winter simulation only at a given interval, to speed up their generation. 1 = daily, 2 = skip half of the days, 3 = plot one day out of 3, and so on.
  write_daily_grids_winter           = FALSE,              # [TRUE/FALSE]: at the end of each year, write daily geotiff grids of SWE from winter simulation (useful for debugging)
  write_daily_grids_winter_freq      = 1,                  # [days]: write "daily" grids of SWE from winter simulation only at a given interval, to speed up processing. 1 = daily, 2 = skip half of the days, 3 = write one day out of 3, and so on.
  daily_massbal_winter_refdate       = "11/01",            # [month/day] or "": use this day as reference for the winter cumulative mass balance grids. The grid of that day will be subtracted from all others. If set to "", no subtraction takes place (i.e., it will be the first grid of cumulative mass balance to have 0.0 everywhere). In the Northern Hemisphere, dates from 1 July are interpreted as YYYY-1, before that as YYYY. In the Southern Hemisphere, always as YYYY-1.
  
  plot_daily_maps_annual             = FALSE,              # [TRUE/FALSE]: at the end of each year, produce plots of daily surface type and SWE from the annual simulation (slow, but useful for debug or visualization).
  plot_daily_maps_annual_freq        = 1,                  # [days]: produce "daily" plots of surface type and SWE from annual simulation only at a given interval, to speed up their generation. 1 = daily, 2 = skip half of the days, 3 = plot one day out of 3, and so on.
  write_daily_grids_annual           = FALSE,              # [TRUE/FALSE]: at the end of each year, write daily geotiff grids of SWE from annual simulation (useful for debugging)
  write_daily_grids_annual_freq      = 1,                  # [days]: write "daily" grids of SWE from annual simulation only at a given interval, to speed up processing. 1 = daily, 2 = skip half of the days, 3 = write one day out of 3, and so on.
  daily_massbal_annual_refdate       = "11/01"             # [month/day] or "": use this day as reference for the summer cumulative mass balance grids. The grid of that day will be subtracted from all others. If set to "", no subtraction takes place (i.e., it will be the first grid of cumulative mass balance to have 0.0 everywhere). In the Northern Hemisphere, dates from 1 July are interpreted as YYYY-1, before that as YYYY. In the Southern Hemisphere, always as YYYY-1.
  
)
