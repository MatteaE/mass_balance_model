###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the fixed parameter definitions for the model run.           #
###################################################################################################

dir_data_base                <-   file.path("input", "pers")        # The base directory for all the data

run_params <- list(
  
  name_glacier                =    "pers",                    # Glacier name, used for output directory naming.
  
  #### INPUT-related parameters ####
  # Set input data paths.
  dir_data_weather             =   file.path(dir_data_base, "weather"),      # The weather series goes here
  dir_data_dhm                 =   file.path(dir_data_base, "dhm"),          # Path to the DHM(s) = elevation grids(s) (rectangular, to compute slopes and curvatures)
  dir_data_surftype            =   file.path(dir_data_base, "surface_type"),     # Path to the grids of surface type (snow/ice/firn/rock/debris) go here
  dir_data_outline             =   file.path(dir_data_base, "outline"),      # Path to the outlines
  dir_data_radiation           =   file.path(dir_data_base, "radiation"),    # Path to the grids of potential direct radiation (daily sums)
  dir_data_massbalance         =   file.path(dir_data_base, "massbalance"),  # The mass balance observations go here
  dir_annual_params            =   file.path("input", "pers", "params"),     # The annual model parameter files go here
  
  # Set filenames and input file properties.
  filename_weather             =   "pers_sils_d_proc.dat",      # File name of the weather series
  file_weather_nskip           =   2,                            # [-]: number of lines to skip in the weather file
  
  grids_crs                    =   21781,            # Reference system of the grids, used in slope/aspect computations. Overrides any CRS info reported from the grid files.
  
  filename_dhm_prefix          =   "dhm_pers",
  filename_dhm_suffix          =   ".grid",                       # DHM name is <prefix><year><suffix>
  dhm_years                    =   c(2008,2015,2019),                      # Years for which a DHM is available. These should be sorted in ascending order.
  dhm_interpolate              =   FALSE,                        # [TRUE/FALSE]: should we use linear interpolation to compute each year's DHM?
  
  filename_surftype_prefix     =   "firn_pers",
  filename_surftype_suffix     =   ".grid",                      # Surface type filename is <prefix><year><suffix>
  surftype_years               =   c(2008,2015,2019),                      # Years for which a surface type file is available. These whould be sorted in ascending order.
  
  filename_outline_prefix      =   "pers",
  filename_outline_suffix      =   "_gltot.xyzn",                # Outline name is <prefix><year><suffix>
  outline_years                =   c(2008,2015,2019),                      # Years for which an outline is available.
  
  filename_radiation_prefix    =   "dir",
  filename_radiation_suffix    =   "24.grid",                    # Radiation files are called <prefix><doy><suffix> where <doy> is the day of year, zero-padded to length 3 (e.g. 001).
  
  filename_massbalance_annual  =   "peg_pers.dat",       # File name of the annual mass balance observations
  filename_massbalance_winter  =   "",                           # File name of the winter mass balance observations
  
  filename_params_prefix       =   "param_",
  filename_params_suffix       =   ".dat",                       # Annual parameters filename is <prefix><year><suffix>
  
  
  #### WEATHER INPUT parameters ####
  weather_aws_elevation        =   1798,                         # [m a.s.l.]: AWS elevation
  weather_snowfall_temp        =   1.5,                          # [°C]: at this temperature precipitation is half rain, half snow. One degree above it is all rain, one degree below it is all snow (snow fraction is linearly interpolated).
  weather_max_precip_ele       =   3800,                         # [m a.s.l.]: above this altitude, precipitation does not increase any more but becomes constant (cutoff).
  
  
  #### TOPOGRAPHICAL SNOW DISTRIBUTION-related parameters ####
  curvature_dhm_smooth         =   1.0,                          # [cells]: amount of gaussian smoothing applied before computing curvature (which is very sensitive to DEM noise, unlike slope). Can be non-integer. 1.0 is good for a normal 20 m DEM.
  curvature_cutoff_fact        =   1.2,                          # [-]: multiplier for the curvature cutoff threshold at which the snow distribution is not further changed. The threshold is given by the smaller of the two curvature extremes (positive and negative) divided by this factor. Only values >  = 1 make sense.
  curvature_effect_limit       =   0.5,                          # [-]: maximum effect of curvature, i.e. the curvature multiplier will be within [1 ± curvature_effect_limit]. Only values between 0 and 1 make sense.
  
  elevation_effect_threshold   =   3850,                         # [m]: elevation above which snow accumulation decreases (wind effect)
  elevation_effect_fact        =   1.0,                          # [-]: strength of snow accumulation decrease at very high altitude. Only values between 0 and 1 make sense. At 0 accumulation does not decrease, at 1 accumulation decreases to 0 at the highest point in the DHM.
  
  
  #### AVALANCHE model parameters ####
  avalanche_routine_cpp        =   TRUE,                         # [TRUE/FALSE]: should we use the C++ (TRUE) or R (FALSE) version of the avalanche routine? C++ is much faster but it requires a code compiler.
  deposition_slope_lim         =   40,                           # [°]: at or above this slope value snow will not be deposited during an avalanche. A lower value makes avalanches travel farther. Called beta_lim in Gruber (2007).
  deposition_mass_lim          =   8000,                         # [kg m-2]: maximum deposition during an avalanche. A lower value makes avalanches travel farther. Called D_lim in Gruber (2007).
  movable_slope_lim_lower      =   30,                           # [°]: above this slope value, there is a linearly increasing movable fraction in the initial mass distribution, for avalanches. A lower value makes avalanches start also on more gentle slopes.
  movable_slope_lim_upper      =   70,                           # [°]: above this slope value, all input snow is movable in the avalanche routine.
  deposition_max_ratio_init    =   12,                           # [-]: ONLY for the initial snow distribution grid, how much accumulation can locally result from an avalanche relative to the mean snow distribution before the avalanche? This controls how far avalanches travel, it should be set to a value low enough that avalanches don't bring snow below the marked snow line elevation, and high enough that avalanche deposits look plausible. An exploratory value of 10 can make sense.
  model_avalanche_dates        =   c("3/31", "6/30", "7/31", "8/31"),  # [month/day]: dates at which an avalanche is simulated. Usually one at the end of winter (but before winter stakes are measured), and one or more in summer to avoid overloading the slopes with summer snowfall.
  
  
  #### INITIAL SNOW COVER parameters ####
  initial_snowline_elevation   =   3650,                         # [m]: initial snow line elevation, at the beginning of each simulated year.
  initial_snow_gradient        =   200,                           # [mm w.e. (100 m)-1]: increase of the initial snow amount for every 100 m elevation above the snow line.
  initial_snow_dist_red_fac    =   0.5,                          # [-]: reduction factor to decrease the importance of the snow distribution variability (all components except winter snow probes), for the computed initial snow cover (of each year). 0 means uniform snow distribution, 1 means no reduction.
  initial_snow_dist_from_model =   FALSE,                         # [TRUE/FALSE]: if TRUE, use the simulated SWE of the previous year as starting condition for the simulation. If FALSE, compute initial SWE from topography and given parameters. The first simulated year always uses a computed initial SWE since there is no previous modeled year.
  
  
  #### ACCUMULATION and MELT MODEL fixed parameters ####
  debris_red_fac               =   0.6,                          # [-]: reduction factor of melt over debris-covered ice.
  accum_probes_red_fac         =   0.5,                          # [-]: reduction factor to decrease the importance of the snow probes distribution when distributing snowfall over the grid, in case those are measured also over avalanche deposits (else we would be accounting twice for avalanche redistribution, since we run a process-based avalanche model). 0 means uniform distribution, 1 means no redution in variability.
  accum_snow_dist_red_fac      =   0.5,                          # [-]: reduction factor to decrease the importance of the topographic snow distribution variability (curvature and elevation cutoff) when distributing snowfall over the grid. 0 means uniform snow distribution, 1 means no reduction.
  albedo_ice_decrease_elev     =   0.,                           # [m]: below this altitude, the ice albedo decreases linearly with altitude (darker ice).
  albedo_ice_decrease_fact     =   0.014,                        # [fraction m-1]: rate of increase above 1 (with decreasing altitude) of the ice albedo factor (multiplying ice melt).

    
  #### ACCUMULATION and MELT MODEL default year parameters ####
  default_prec_corr            =   200,                          # [%]: precipitation correction.
  default_prec_summer_fact     =   0.6,                          # [-]: multiplicative reduction of precipitation correction in summer.
  default_prec_elegrad         =   10,                           # [% / 100 m]: altitudinal gradient of precipitation.
  default_temp_elegrad         =   -0.6,                         # [°C / 100 m]: altitudinal gradient of air temperature.
  default_melt_factor          =   1.0,                          # [mm w.e. C-1 d-1]: melt factor for DDF model.
  default_rad_fact_ice         =   0.2,                          # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: radiation factor for ice within DDF model.
  default_rad_fact_snow        =   0.13,                          # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: radiation factor for snow within DDF model.
  nodata_years_automatic       =   TRUE,                         # [TRUE/FALSE]: if TRUE, the prec_corr/melt_factor/rad_fact_ice/rad_fact_snow parameters for years with no mass balance will be taken as the mean of the parameters optimized over the years with measured mass balance data (only IF there is no overriding value in an annual parameter file AND there is at least one year with measured mass balances). If FALSE, the parameters are taken from the defaults under run_params (only IF there is no overriding value in an annual parameter file).
  
  
  #### STAKES parameters ####
  stakes_unknown_latest_start  =   "2/28",                       # [month/day]: in the automatic search of the start date for snow pits and depth probings without a measured start date, we search no later than this day of year. The starting date will be set to the day of the minimum cumulative mass balance between the start of the simulation and the date set here. Something like end of February should be safe for all stakes. 
  stake_cluster_distance       =   50,                          # [m]: threshold distance for clustering stakes together. This is used to ensure a more uniform distribution of the stakes: if measurements are very dense in one place they can induce a bias in the optimization, so we average stakes in clusters. This can reduce the total number of stakes. Only stakes measured on the same days can be clustered. A value of 0 corresponds to no clustering.
  snow_probes_idw_exp          =   0.75,                         # [-]: exponent for the IDW interpolation of winter snow measurements
  
  
  #### MODEL OPTIMIZATION parameters ####
  optim_max_corr_fact          =   1,                            # [-]: maximum allowable positive correction to the melt factor and the radiation factor during optimization, in units of the factors themselves (i.e. by how many times these can be increased). Only positive values make sense. A larger value is safer if a reasonable value for the melt factors is not known, but the optimization will be a bit slower. There is no parameter for the negative correction: it is automatically set to maximum 0.
  optim_bias_threshold         =   1,                            # [mm w.e.]: if abs(bias) is below this threshold then we stop the optimization. This saves us a couple iterations since the optim() function will stop when the value *change* is less than a threshold, not the value itself.
  optim_max_iter               =   20,                           # [-]: forcefully stop mass balance optimization after this number of iterations, even if bias is not within threshold. This is useful in case the optimization is not converging due to avalanches barely reaching a stake, thus a small change in the snow amounts changes a stake's simulated mass balance by a lot, thus bias keeps jumping around 0. In normal conditions, the model converges much faster than 20 iterations.

  
  #### FIXED MASS BALANCE PERIODS choice ####
  massbal_fixed_annual_start   =   "10/31",                       # [month/day]: start of the user-defined fixed period for annual mass balance evaluation. This is referred to (<year_cur> - 1).
  massbal_fixed_annual_end     =   "8/31",                       # [month/day]: end of the user-defined fixed period for annual mass balance evaluation. This is referred to <year_cur>.
  massbal_fixed_winter_start   =   "10/1",                       # [month/day]: start of the user-defined fixed period for winter mass balance evaluation. This is referred to (<year_cur> - 1).
  massbal_fixed_winter_end     =   "4/30",                       # [month/day]: end of the user-defined fixed period for winter mass balance evaluation. This is referred to <year_cur>.
  
  
  #### MASS BALANCE PROCESSING parameters ####
  ele_bands_ela_size           =   10,                           # [m]: to compute the equilibrium line altitude, divide the glacier grid into elevation bands with this vertical extent.
  
  
  #### PLOT parameters ####
  mb_colorscale_breaks         =   c(-2,-1.5,-1,-0.5,-0.2,0,0.2,0.5,1,1.5,2), # [m w.e.]: use these breaks in the color scale for mass balance maps. NOTE: these have to be exactly 11 at the moment.
  ele_bands_plot_size          =   50,                           # [m]: plot the annual mass balance profile as function of elevation, using elevation bands with this vertical extent.
  plot_daily_maps              =   FALSE,                         # [TRUE/FALSE]: produce daily plots of mass balance and SWE (slow!).
  
  
  #### OUTPUT parameters ####
  output_grid_ext           =      ".tif",                       # extension of the output mass balance grids. Use ?writeFormats to check what is available. Common choices are ".tif" for GeoTiff, and ".asc" for ASCII grid.
  dem_write                    =   TRUE,                        # [TRUE/FALSE]: should we write the annual used DEM to the output directory?
  filename_dem_prefix          =   "dem_pers_",               # output DEM name is <prefix><year><output_grid_exts>
  
  
  #### MODELED YEARS choice ####
  first_year                   =   2011,                         # First modeled year (usually from October of the previous year to September of this year)
  last_year                    =   2018                          # Last modeled year (same as previous comment)
  
)
