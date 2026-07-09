###################################################################################################
# Author:         Enrico Mattea (@unifr.ch), inspired by the IDL version by Matthias Huss.        #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the main loop and instructions.                              #
###################################################################################################


# Prepare environment -----------------------------------------------------------------------------
# If in utils folder, move one up.
if (basename(getwd()) == "utils") {
  setwd("..")
}

# Close any leftover sinks and connections from previous runs.
while (sink.number() > 0) { sink() }
while (nrow(showConnections(all = FALSE)) > 0) { close(getConnection(rownames(showConnections(all = FALSE)))) }


# Start logger ------------------------------------------------------------------------------------
dir.create("logs", showWarnings = FALSE)
logfile <- file.path("logs", paste0("mb_model_run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
logcon  <- file(logfile, open = "a")

# Setup sink for split logging (console + logfile).
sink(logcon, split = TRUE)
options(warn=1) # Configure immediate printing of warnings.


# Set parameters and load function definitions ----------------------------------------------------
# Set English language for dates (in the plots).
if (Sys.info()["sysname"] == "Windows") {
  Sys.setlocale(category = "LC_TIME", locale = "English")
} else {
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
}

# This creates list run_params with the model parameters.
source("set_params.R")

invisible(sapply(file.path("functions", list.files("functions", pattern = "\\.R$")), source))


# Run model ---------------------------------------------------------------------------------------
mod_result <- func_run_model(run_params)


# Stop logger -------------------------------------------------------------------------------------
sink()
close(logcon)
