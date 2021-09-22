###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), inspired by the IDL version by Matthias Huss.        #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the main loop and instructions.                              #
###################################################################################################

#### Set parameters and load function definitions ####
# Set English language for dates (in the plots).
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")

# This creates list run_params with the model parameters.
source("set_params.R")

invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))

#### Run model ####
mod_result <- func_run_model(run_params)
