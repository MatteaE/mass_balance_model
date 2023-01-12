###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to load all required R packages.                    #
################################################################################################### 


func_load_packages <- function(run_params) {
  
  cat("Loading required R packages...\n")
  
  options("rgdal_show_exportToProj4_warnings"="none") # Suppress "Discarded datum" errors - see https://cran.r-project.org/web/packages/rgdal/vignettes/PROJ6_GDAL3.html
  
  package_list <- c("terra",
                    "sp",           # SpatialPolygons(), for the outline.
                    "spatialEco",   # curvature()
                    "scales",       # rescale()
                    "topmodel",     # sinkfill()
                    "gstat",        # IDW of snow probing data
                    "Rfast",        # rowSort() of the stake cells indices
                    "timeSeries",   # interpNA() of the band biases.
                    "stats",        # uniroot()
                    "sf",           # st_read(), to load shapefile outlines.
                    "metR",         # geom_text_contour()
                    "ggplot2",      # Base plotting library
                    "ggtext",       # Additional plotting functions (element_markdown(), for bold superscripts)
                    "ggpubr",       # Additional plotting functions (multi-page PDF)
                    "grid",         # Additional plotting functions (text annotations)
                    "cowplot",      # Additional plotting functions (align plots)
                    "ggpattern",    # Additional plotting functions (pattern as histogram fill)
                    "shadowtext",   # Additional plotting functions (text with white outline)
                    "reshape2",     # melt() data frame
                    "stringr",      # str_split() of the outline filename suffix, to get the extension
                    "RStoolbox",    # For the surface type basemap under the daily SWE plots (currently disabled)
                    "qpdf")         # To extract the annual hydrological mass balance maps and put them into the overview PDF.
  
  # As of 2023/01/12, we need Rcpp for fourCellsFromXY, which is not yet implemented in the terra package.
  # if (run_params$avalanche_routine_cpp == TRUE) {
    package_list <- c(package_list, "Rcpp")
  # }
  
  exit_status <- suppressPackageStartupMessages(sapply(package_list, require, character.only = TRUE))
  exit_status_all <- all(exit_status)
  
  if (!exit_status_all) {
    pkg_missing_ids <- which(!exit_status)
    cat(paste0("* WARNING: some required packages are missing. They are: ", paste(package_list[pkg_missing_ids], collapse = ", ")), "\n")
  }
  
  return(exit_status_all)
}
