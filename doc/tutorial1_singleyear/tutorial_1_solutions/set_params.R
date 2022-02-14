###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the fixed parameter definitions for the model run.           #
###################################################################################################

run_params <- list(
  
  name_glacier                =    "yakarcha",                    # Glacier name, used for output directory naming.
  
  #### INPUT-related parameters ####
  dir_data_recursive           =   TRUE,                         # [TRUE/FALSE]: should we look recursively for the input files in the specified directories?
  
  # Set FILE NAMES and input file properties.
  filename_weather             =   "weather_yakarcha.dat",       # File name of the weather series
  file_weather_nskip           =   4,                            # [-]: number of lines to skip in the weather file
  
  grids_crs                    =   32642,                        # Reference system of the grids, used in slope/aspect computations. Overrides any CRS info reported from the grid files.
  
  dhm_interpolate              =   FALSE,                        # [TRUE/FALSE]: should we use linear interpolation to compute each year's DHM?
  
  filename_massbalance_annual  =   "mb_yakarcha.dat",       # File name of the annual mass balance observations
  filename_massbalance_winter  =   "",                           # File name of the winter mass balance observations
  
  filename_params_prefix       =   "param_",
  filename_params_suffix       =   ".dat",                       # Annual parameters filename is <prefix><year><suffix>
  
  
  #### WEATHER INPUT parameters ####
  weather_aws_elevation        =   4000,                         # [m a.s.l.]: AWS elevation
  weather_snowfall_temp        =   1.5,                          # [°C]: at this temperature precipitation is half rain, half snow. One degree above it is all rain, one degree below it is all snow (snow fraction is linearly interpolated).
  weather_max_precip_ele       =   NA,                         # [m a.s.l.]: above this altitude, precipitation does not increase any more but becomes constant (cutoff). If NA, it is estimated automatically from the first DEM grid.
  
  
  #### TOPOGRAPHICAL SNOW DISTRIBUTION-related parameters ####
  curvature_dhm_smooth         =   1.0,                          # [cells]: amount of gaussian smoothing applied before computing curvature (which is very sensitive to DEM noise, unlike slope). Can be non-integer. 1.0 is good for a normal 20 m DEM.
  curvature_cutoff_fact        =   1.2,                          # [-]: multiplier for the curvature cutoff threshold at which the snow distribution is not further changed. The threshold is given by the smaller of the two curvature extremes (positive and negative) divided by this factor. Only values >  = 1 make sense.
  curvature_effect_limit       =   0.5,                          # [-]: maximum effect of curvature, i.e. the curvature multiplier will be within [1 ± curvature_effect_limit]. Only values between 0 and 1 make sense.
  
  elevation_effect_threshold   =   NA,                         # [m]: elevation above which snow accumulation decreases (wind effect). If NA, it is estimated automatically from the first DEM grid.
  elevation_effect_fact        =   1.0,                          # [-]: strength of snow accumulation decrease at very high altitude. Only values between 0 and 1 make sense. At 0 accumulation does not decrease, at 1 accumulation decreases to 0 at the highest point in the DHM.
  
  
  #### AVALANCHE model parameters ####
  avalanche_routine_cpp        =   TRUE,                         # [TRUE/FALSE]: should we use the C++ (TRUE) or R (FALSE) version of the avalanche routine? C++ is much faster but it requires a code compiler.
  deposition_slope_lim         =   40,                           # [°]: at or above this slope value snow will not be deposited during an avalanche. A lower value makes avalanches travel farther. Called beta_lim in Gruber (2007).
  deposition_mass_lim          =   NA,                           # [kg m-2]: maximum deposition during an avalanche. A lower value makes avalanches travel farther. Called D_lim in Gruber (2007). If left at NA, it is automatically estimated from the maximum annual solid precipitation.
  movable_slope_lim_lower      =   30,                           # [°]: above this slope value, there is a linearly increasing movable fraction in the initial mass distribution, for avalanches. A lower value makes avalanches start also on more gentle slopes.
  movable_slope_lim_upper      =   70,                           # [°]: above this slope value, all input snow is movable in the avalanche routine.
  deposition_max_ratio_init    =   12,                           # [-]: ONLY for the initial snow distribution grid, how much accumulation can locally result from an avalanche relative to the mean snow distribution before the avalanche? This controls how far avalanches travel, it should be set to a value low enough that avalanches don't bring snow below the marked snow line elevation, and high enough that avalanche deposits look plausible. An exploratory value of 10 can make sense.
  model_avalanche_dates        =   c("3/31", "6/30", "7/31", "8/31"),  # [month/day]: dates at which an avalanche is simulated. Usually one at the end of winter (but before winter stakes are measured), and one or more in summer to avoid overloading the slopes with summer snowfall.
  
  
  #### INITIAL SNOW COVER parameters ####
  initial_snowline_elevation   =   NA,                         # [m]: initial snow line elevation, at the beginning of each simulated year. If NA, it is estimated automatically from the first DEM grid.
  initial_snow_gradient        =   200,                           # [mm w.e. (100 m)-1]: increase of the initial snow amount for every 100 m elevation above the snow line.
  initial_snow_dist_red_fac    =   0.5,                          # [-]: reduction factor to decrease the importance of the snow distribution variability (all components except winter snow probes), for the computed initial snow cover (of each year). 0 means uniform snow distribution, 1 means no reduction.
  initial_snow_dist_from_model =   FALSE,                         # [TRUE/FALSE]: if TRUE, use the simulated SWE of the previous year as starting condition for the simulation. If FALSE, compute initial SWE from topography and given parameters. The first simulated year always uses a computed initial SWE since there is no previous modeled year.
  
  
  #### ACCUMULATION and MELT MODEL fixed parameters ####
  debris_red_fac               =   0.6,                          # [-]: reduction factor of melt over debris-covered ice.
  accum_probes_red_fac         =   0.5,                          # [-]: reduction factor to decrease the importance of the snow probes distribution when distributing snowfall over the grid, in case those are measured also over avalanche deposits (else we would be accounting twice for avalanche redistribution, since we run a process-based avalanche model). 0 means uniform distribution, 1 means no redution in variability.
  accum_snow_dist_red_fac      =   0.5,                          # [-]: reduction factor to decrease the importance of the topographic snow distribution variability (curvature and elevation cutoff) when distributing snowfall over the grid. 0 means uniform snow distribution, 1 means no reduction.
  albedo_ice_decrease_elev     =   0.,                           # [m]: below this altitude, the ice albedo decreases linearly with altitude (darker ice).
  albedo_ice_decrease_fact     =   0.014,                        # [m-1]: rate of increase above 1 (with decreasing altitude) of the ice albedo factor (multiplying ice melt).

    
  #### ACCUMULATION and MELT MODEL default year parameters ####
  default_prec_corr            =   150,                          # [%]: default precipitation correction in case no data are available.
  default_prec_summer_fact     =   0.8,                          # [-]: default multiplicative reduction of precipitation correction in summer.
  default_prec_elegrad         =   10,                           # [% / 100 m]: default altitudinal gradient of precipitation.
  default_temp_elegrad         =   -0.6,                         # [°C / 100 m]: default altitudinal gradient of air temperature.
  default_melt_factor          =   4.0,                          # [mm w.e. C-1 d-1]: default melt factor for DDF model.
  default_rad_fact_ice         =   0.8,                          # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: default radiation factor for ice within DDF model.
  default_rad_fact_snow        =   0.5,                          # [10^-3 mm w.e. C-1 h-1 (W m-2)-1]: default radiation factor for snow within DDF model.
  nodata_years_automatic       =   TRUE,                         # [TRUE/FALSE]: if TRUE, the prec_corr/melt_factor/rad_fact_ice/rad_fact_snow parameters for years with no mass balance will be taken as the mean of the parameters optimized over the years with measured mass balance data (only IF there is no overriding value in an annual parameter file AND there is at least one year with measured mass balances). If FALSE, the parameters are taken from the defaults under run_params (only IF there is no overriding value in an annual parameter file).

  
  #### PLOTTING parameters ####
  mb_colorscale_multiplier     =   1,                            # NA for automatic colorscale limits, or a number to multiply the colorscale limits. 1 = colorscale between -2 and +2 m w.e.
  output_unit                  =   "m",                          # Either "m" for meters water-equivalent, or "mm" for millimeters water-equivalent.
  show_contour_labels          =   TRUE,                         # In the mass balance maps, show the labels of contour lines (TRUE) or not (FALSE).
  show_stake_labels            =   TRUE,                         # In the mass balance maps, show the labels of stake measurements (TRUE) or not (FALSE).
  show_month_lines             =   TRUE,                         # In the time series plots (mass balance and meteo series), show vertical lines to divide the months (TRUE) or not (FALSE).
  
  
  #### MODELED YEARS choice ####
  first_year                   =   2020,                         # First modeled year (usually from October of the previous year to September of this year)
  last_year                    =   2020                          # Last modeled year (same as previous comment)
  
)
