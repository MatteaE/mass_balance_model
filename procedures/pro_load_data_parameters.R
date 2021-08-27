###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to load input data and parameters.                  #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.


#### Load input data from sources or reboot file ####
if (boot_file_read) {
  load(boot_file_name)
} else {
  source(file.path("procedures", "pro_load_data_all.R")) # This calls all the data loading routines.
}
