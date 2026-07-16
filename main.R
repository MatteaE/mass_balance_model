###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), inspired by the IDL version by Matthias Huss.        #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file calls the main function to run the whole thing.                       #
###################################################################################################


# If in utils folder, move one up -----------------------------------------------------------------
if (basename(getwd()) == "utils") { setwd("..") }


# Set parameters ----------------------------------------------------------------------------------
source("set_params.R")


# Load all the model functions --------------------------------------------------------------------
invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))


# Run model ---------------------------------------------------------------------------------------
mod_result <- func_run_model(run_params)
