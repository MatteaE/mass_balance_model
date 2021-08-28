###################################################################################################
# This program installs all the R packages required to run the glacier mass balance model.        #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2021/8/27                                                                        #
###################################################################################################

packages_cran <- c("cowplot",
                   "ggplot2",
                   "ggpubr",
                   "gstat",
                   "metR",
                   "raster",
                   "Rcpp",
                   "remotes",
                   "reshape2",
                   "Rfast",
                   "RStoolbox",
                   "scales",
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
