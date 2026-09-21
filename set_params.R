###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the fixed parameter definitions for the model run.           #
###################################################################################################

run_params <- list(
  
  # . (0) Required parameters -----------------------------
  name_glacier                      = "example_minimal",          # [name in "quotes"]: glacier name, which is used for the input folder, the output folder, and a lot of files
  filename_weather                  = "weather_minimal.dat",      # [filename in "quotes"]: name of the file (under weather/) with the daily meteorological series
  file_weather_nskip                = 4,                          # [-]: number of header lines to skip in the meteo file. The first non-skipped line should already have the first data entry (no header)
  weather_aws_elevation             = 3837,                       # [m asl]: reference elevation for the meteorological data
  grids_crs                         = 32642,                      # [-]: EPSG code as integer - this is the reference system of the grids, used in slope/aspect computations. Overrides any CRS info reported from the grid files.
  first_year                        = 1974,                       # [-]: the first year to be processed. In the Northern Hemisphere a year usually goes from September of the previous year (YYYY-1) to September of the specified year (YYYY). In the Southern Hemisphere, from March YYYY-1 to March YYYY.
  last_year                         = 1974,                       # [-]: the last year to be processed. If same as first_year, a single year is simulated. Else the model covers multiple years.
  

  # . (1) Input files parameters --------------------------
  filename_massbalance_annual        = "mb_minimal_annual.dat",   # [filename in "quotes"]: file (under massbal/) with the annual mass balance observations. Can be "" if there are no such data.
  filename_massbalance_winter        = "mb_minimal_winter.dat",   # [filename in "quotes"]: file (under massbal/) with the winter mass balance observations. Can be "" if there are no such data.
  
  
  # . (6) Avalanche model parameters ----------------------
  model_avalanche_dates        =   c("1/31", "3/31", "6/30")      # [month/day]: dates at which an avalanche is simulated.
  
)
