###################################################################################################
# This program installs all the R packages required to run the glacier mass balance model.        #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2021/9/20                                                                        #
###################################################################################################

# This function is called after reloading R, needed (on windows) to find Rtools on the path.
func_install_all <- function() {
   
   if (Sys.info()["sysname"] == "Windows") {
      old_path <- Sys.getenv("PATH")
      Sys.setenv(PATH = paste0(old_path, ";C:\\rtools40\\usr\\bin"))
   }
   
   if (nchar(as.character(Sys.which("make"))) == 0) {
      if (Sys.info()["sysname"] == "Windows") {
         stop("FATAL: there was a problem with the installation of RTools. Try to close and reopen RStudio.")
      } else {
         stop("FATAL: no code compiler found. Please get a code compiler before proceeding.")
      }
   }
   
   packages_cran <- c("cowplot",
                      "ggplot2",
                      "ggpubr",
                      "gstat",
                      "insol",
                      "metR",
                      "qpdf",
                      "raster",
                      "Rcpp",
                      "remotes",
                      "reshape2",
                      "Rfast",
                      "RStoolbox",
                      "scales",
                      "shadowtext",
                      "shiny",
                      "shinyFiles",
                      "shinyjs",
                      "stringr",
                      "sf",
                      "sp",
                      "spatialEco",
                      "topmodel",
                      "timeSeries")
   
   install.packages(setdiff(packages_cran, rownames(installed.packages())))  
   
   packages_github_repos <- c("coolbutuseless")
   packages_github_names <- c("ggpattern")
   packages_github_full <- paste(packages_github_repos, packages_github_names, sep="/")
   packages_github_missing_ids <- which(!(packages_github_names %in% rownames(installed.packages())))
   
   remotes::install_github(packages_github_full[packages_github_missing_ids])
   
   message("\n\n====                                     ====\n==== All packages installed succesfully! ====\n====                                     ====")
   message("\nYou should now **CLOSE** and **RESTART** RStudio **BEFORE** running the model.\n")
   
}

if (Sys.info()["sysname"] == "Windows") {
   if (!("installr" %in% rownames(installed.packages()))) {
      install.packages("installr")
   }
   if (nchar(as.character(Sys.which("make"))) == 0) {
      installr::install.Rtools()
      writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
      Sys.sleep(1)
      .rs.restartR("func_install_all()")
   }
# Install stuff in case we don't have Windows i.e. we don't need to restart R.
} else {
   func_install_all()
}


