###################################################################################################
# This program installs all the R packages required to run the glacier mass balance model.        #
# Author: Enrico Mattea (University of Fribourg)                                                  #
###################################################################################################

# This function is called after reloading R, needed (on windows) to find Rtools on the path.
func_install_all <- function() {
   
   if (Sys.info()["sysname"] == "Windows") {
      old_path <- Sys.getenv("PATH")
      Sys.setenv(PATH = paste0(old_path, ";C:\\rtools40\\usr\\bin"))
   }
   
   if (nchar(as.character(Sys.which("make"))) == 0) {
      if (Sys.info()["sysname"] == "Windows") {
         cat("FATAL: there was a problem with the installation of RTools.\n")
         cat("       Try to close and reopen RStudio and run again this program.\n")
         cat("       If it doesn't work, try to INSTALL RTools MANUALLY from https://cran.r-project.org/bin/windows/Rtools/\n")
         cat("       Choose the correct version of RTools! You have", R.version[["version.string"]])
         stop()
      } else {
         stop("FATAL: no code compiler found. Please get a code compiler before proceeding.")
      }
   }
   
   packages_cran <- c("cowplot",
                      "ggpattern",
                      "ggplot2",
                      "ggpubr",
                      "gstat",
                      "ggtext",
                      "lwgeom",
                      "metR",
                      "qpdf",
                      "raster",
                      "Rcpp",
                      "readxl",
                      "reshape2",
                      "Rfast",
                      "scales",
                      "shadowtext",
                      "shiny",
                      "shinyFiles",
                      "shinyjs",
                      "stringr",
                      "sf",
                      "sp",
                      "spatialEco",
                      "terra",
                      "topmodel",
                      "timeSeries")
   
   install.packages(setdiff(packages_cran, rownames(installed.packages())))  
   
   packages_archived <- c("insol" = "https://cran.r-project.org/src/contrib/Archive/insol/insol_1.2.2.tar.gz")
   install.packages(packages_archived[setdiff("insol", rownames(installed.packages()))], repos = NULL)
   
   # Install terra package. We take a very recent
   # version since it is still under development.
   #if (!("terra" %in% rownames(installed.packages()))) {
   # install.packages('terra', repos='https://rspatial.r-universe.dev')
   #} else if (compareVersion(as.character(packageVersion("terra")), "1.6.51") == -1) {
   #  install.packages('terra', repos='https://rspatial.r-universe.dev')
   #}
   
   
   message("\n\n====                                     ====\n==== All packages installed succesfully! ====\n====                                     ====")
   message("\nYou should now **CLOSE** and **RESTART** RStudio **BEFORE** running the model.\n")
   
}

# NOTE: this only works on R >= 4.2.0, after installing RTools manually.
# For older versions, consider using the commented code below.
func_install_all()

# if (Sys.info()["sysname"] == "Windows") {
#    if (!("installr" %in% rownames(installed.packages()))) {
#       install.packages("installr")
#    }
#    if (nchar(as.character(Sys.which("make"))) == 0) {
#       # installr::install.Rtools()
#       # writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron") # This hack is for RTools 4.0, not needed on R >= 4.2.
#       Sys.sleep(1)
#       .rs.restartR("func_install_all()")
#    } else {
#       func_install_all()
#    }
# # Install stuff in case we don't have Windows i.e. we don't need to restart R.
# } else {
#    func_install_all()
# }


