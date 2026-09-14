###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the fixed parameter definitions for the model run.           #
#                 It is an example file with the minimal set of fixed parameters which are        #
#                 required to run DMBSim.                                                         #
#                 The provided values are examples only, to understand the parameter type.        #
###################################################################################################

run_params <- list(
  
  # . (0) Required parameters -------------------------------
  name_glacier                      = "glacier_name",      # [name in "quotes"]: glacier name, which is used for the input folder, the output folder, and a lot of files
  filename_weather                  = "weather_data.dat",  # [filename in "quotes"]: name of the file (under weather/) with the daily meteorological series
  file_weather_nskip                = 4,                   # [-]: number of header lines to skip in the meteo file. The first non-skipped line should already have the first data entry (no header)
  weather_aws_elevation             = 3500,                # [m asl]: reference elevation for the meteorological data
  grids_crs                         = 32642,               # [-]: EPSG code as integer - this is the reference system of the grids, used in slope/aspect computations. Overrides any CRS info reported from the grid files.
  first_year                        = 2018,                # [-]: the first year to be processed. In the Northern Hemisphere a year usually goes from September of the previous year (YYYY-1) to September of the specified year (YYYY). In the Southern Hemisphere, from March YYYY-1 to March YYYY.
  last_year                         = 2020                 # [-]: the last year to be processed. If same as first_year, a single year is simulated. Else the model covers multiple years.
  
)
