###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), inspired by the IDL version by Matthias Huss.        #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file calls the main function to run the whole thing.                       #
###################################################################################################


# If in utils folder, move one up -----------------------------------------------------------------
if (basename(getwd()) == "utils") { setwd("..") }


# Load all the model functions --------------------------------------------------------------------
invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))
dmbsim_version <- "3.0"


# Set parameters ----------------------------------------------------------------------------------
if (file.exists("set_params.R")) {
  source("set_params.R")
} else {
  txt <- "Configuration file set_params.R not found. Please provide one in the same folder as the main program."
  func_consolewrite("\033[1;48;5;196;38;5;231m FATAL \033[0m ", txt)
  fatal_char <- txt
  func_stop()
}


# Run model ---------------------------------------------------------------------------------------
mod_result <- func_run_model(run_params)
