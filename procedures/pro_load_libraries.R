###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to load all required modules.                       #
###################################################################################################

# NOTE: this code is source()'d as part of main.R.
# We put code here just to make it more organized.

#### Load packages ####
library(raster)
library(sp)           # SpatialPolygons(), for the outline.
library(spatialEco)   # curvature()
library(scales)       # rescale()
library(topmodel)     # sinkfill()
library(gstat)        # IDW of snow probing data
library(Rfast)        # rowSort() of the stake cells indices
library(timeSeries)   # interpNA() of the band biases.
library(stats)        # uniroot()
library(sf)           # st_read(), to load shapefile outlines.
library(metR)         # geom_text_contour()
library(ggplot2)      # Base plotting library
library(ggpubr)       # Additional plotting functions (multi-page PDF)
library(grid)         # Additional plotting functions (text annotations)
library(cowplot)      # Additional plotting functions (align plots)
library(ggpattern)    # Additional plotting functions (pattern as histogram fill)
library(shadowtext)   # Additional plotting functions (text with white outline)
library(reshape2)     # melt() data frame.
library(stringr)      # str_split() of the outline filename suffix, to get the extension.
library(RStoolbox)    # For the surface type basemap under the daily SWE plots (currently disabled).

#### Set fixed run parameters ####
source("set_params.R")

# Load C++ avalanche routine, only if asked to do so.
if (run_params$avalanche_routine_cpp == TRUE) {library(Rcpp)}         # avalanche function implemented in C++ for performance
