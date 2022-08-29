###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), inspired by the IDL version by Matthias Huss.        #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the main loop and instructions.                              #
###################################################################################################

# If in utils folder, move one up.
if (basename(getwd()) == "utils") {
  setwd("..")
}

# Start logging console output.
dir.create("logs", showWarnings = FALSE)
sink(file = file.path("logs", paste0("mb_model_run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")), split = TRUE)

#### Set parameters and load function definitions ####
# Set English language for dates (in the plots).
if (Sys.info()["sysname"] == "Windows") {
  Sys.setlocale(category = "LC_TIME", locale = "English")
} else {
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
}

# This creates list run_params with the model parameters.
source("set_params.R")

invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))

#### Run model ####
mod_result <- func_run_model(run_params)

# Stop logging console output.
sink()
