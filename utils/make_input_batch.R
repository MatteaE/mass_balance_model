###################################################################################################
# This tools prepares topographic data for input in the glacier mass balance model DMBSim.        #
# It is a batch, GUI-less version of the make_input tool.                                         #
# It can be used to prepare multi-annual data for one glacier in a single call.                   #
# It takes as input a folder with DEMs (with the year in the file name), a folder with outlines   #
# (with the year in the file name), a required reference raster (to set the grid),                #
# an optional shapefile of firn, and an optional shapefile of debris cover.                       #
# It produces as output a folder ready to be used by DMBSim - with dhm, outline, surface type,    #
# and radiation.                                                                                  #
# Unlike the GUI version, it does little to no validation.                                        #
# Author: Enrico Mattea (University of Fribourg)                                                  #
###################################################################################################

library(insol2)
library(terra)
library(qgisprocess)
source("func_make_input_batch.R")

# File naming: the first set of four contiguous digits in a file name is interpreted as the reference year of the file.

# Algorithm:
# - List DEMs, for each extract the year, reproject to the reference grid, store in dhm/
# - List outlines, for each extract the year, reproject to the reference grid's CRS, store in outline/
# - Compute union of all outlines (i.e., full area ever touched by the glacier during the outlines period),
#   compute A SINGLE SURFACE TYPE on it (rock outside of the union, firn and debris as intersection with the union,
#   the rest is ice); later, DMBSim (func_repair_surface_type) repairs the surface for each individual outline)
# - Compute potential radiation from the most recent DEM (usually Pléiades, so better)


# The raw data are read from <name_glacier>/raw/{dems,outlines,surftype}/<files> and <name_glacier>/raw/ref_grid.tif
# The output folders will be <name_glacier>/{dhm,outline,surftype,radiation}/<files>
name_glacier       <- "abramov"

rawdir_fn          <- "raw"
rawdir_dem_fn      <- "dems"
rawdir_outl_fn     <- "outlines"
rawdir_surftype_fn <- "surftype"

ref_grid_fn        <- "ref_grid.tif"

ref_grid_fp        <- file.path(name_glacier, rawdir_fn, ref_grid_fn)
rawdir_dem_fp      <- file.path(name_glacier, rawdir_fn, rawdir_dem_fn)
outdir_dem_fp      <- file.path(name_glacier, "dhm")
rawdir_outl_fp     <- file.path(name_glacier, rawdir_fn, rawdir_outl_fn)
outdir_outl_fp     <- file.path(name_glacier, "outline")
raw_firn_fp        <- file.path(name_glacier, rawdir_surftype_fn, "firn.shp")
raw_debris_fp      <- file.path(name_glacier, rawdir_surftype_fn, "debris.shp")
outdir_surftype_fp <- file.path(name_glacier, "surftype")



# Load reference grid -----------------------------------------------------------------------------
ref_grid_r         <- rast(ref_grid_fp)



# Process DEMs ------------------------------------------------------------------------------------
dems_lf            <- list.files(rawdir_dem_fp, pattern = "[0-9]{4}.+")

if (length(dems_lf) == 0) {
  stop("No DEMs found")
}

dem_years_str      <- unlist(regmatches(dems_lf, regexec("[0-9]{4}", dems_lf)))

dir.create(outdir_dem_fp, recursive = TRUE)
for (i in 1:length(dems_lf)) {
  
  dem_r <- rast(file.path(rawdir_dem_fp, dems_lf[i]))
  
  dem_out_r <- project(dem_r, ref_grid_r, method = "bilinear")
  
  # . Fill any gaps with RST, default parameters --------------------------------------------------
  if (global(dem_out_r, "anyNA") == TRUE) {
    
    writeRaster(dem_out_r, file.path(outdir_dem_fp, "dhm_tmp.tif"))
    qgis_run_algorithm(alg = "grass:r.fillnulls",
                       input = file.path(normalizePath(outdir_dem_fp), "dhm_tmp.tif"),
                       output = file.path(normalizePath(outdir_dem_fp), "dhm_tmp_filled.tif"),
                       method = 2,
                       tension = 40,
                       smooth = 0.1,
                       edge = 3,
                       npmin = 600,
                       segmax = 300,
                       lambda = 0.01,
                       GRASS_REGION_PARAMETER = paste0(ext(dem_out_r)[1:4], collapse=", "),
                       GRASS_REGION_CELLSIZE_PARAMETER = xres(dem_out_r))
    dem_out_r <- setValues(ref_grid_r, values(rast(file.path(normalizePath(outdir_dem_fp), "dhm_tmp_filled.tif"))))
    if (global(dem_out_r, "anyNA") == TRUE) {
      stop("NA values remain in the DEM (i = ", i, ")")
    }
    writeRaster(dem_out_r, file.path(outdir_dem_fp, paste0("dhm_", name_glacier, "_", dem_years_str[i], ".tif")))
    file.remove(file.path(outdir_dem_fp, "dhm_tmp.tif"),
                file.path(outdir_dem_fp, "dhm_tmp_filled.tif"),
                file.path(outdir_dem_fp, "dhm_tmp_filled.tfw"))
  } else {
    writeRaster(dem_out_r, file.path(outdir_dem_fp, paste0("dhm_", name_glacier, "_", dem_years_str[i], ".tif")))
  }
  
}



# Process outlines --------------------------------------------------------------------------------
outl_lf            <- list.files(rawdir_outl_fp, pattern = "[0-9]{4}.*(shp|gpkg)$")

if (length(outl_lf) == 0) {
  stop("No outlines found")
}

outl_years_str      <- unlist(regmatches(outl_lf, regexec("[0-9]{4}", outl_lf)))


dir.create(outdir_outl_fp, recursive = TRUE)
for (i in 1:length(outl_lf)) {
  
  outl_v <- vect(file.path(rawdir_outl_fp, outl_lf[i]))
  
  # . Check validity, repair geometries -----------------------------------------------------------
  outl_v <- func_validate_vect(outl_v,
                               "glacier outline")
  if (is.character(outl_v)) {
    stop(outl_v)
  }
  
  
  # . Check CRS -----------------------------------------------------------------------------------
  outl_v <- func_repair_vect_crs(outl_v)
  if (is.null(outl_v)) {
    stop("Outline ", i, ": coordinates system of the outline file is not recognized.")
  }
  
  
  # . Reproject and write output ------------------------------------------------------------------
  outl_out_v <- project(outl_v, crs(ref_grid_r))
  writeVector(outl_out_v, file.path(outdir_outl_fp, paste0("outline_", name_glacier, "_", outl_years_str[i], ".shp")))
  
}



# Process surface type ----------------------------------------------------------------------------

# . Load all outlines and compute union -----------------------------------------------------------

outl_out_all_fn <- list.files(outdir_outl_fp, pattern = "\\.shp$")

outl_all_v <- terra::aggregate(vect(sapply(file.path(outdir_outl_fp, outl_out_all_fn), vect))) # Works also when there is a single outline

surftype_r <- 4*is.na(terra::mask(ref_grid_r, outl_all_v)) # This is the base rock/ice mask, for the union glacier extent.

# Add firn if we have it.
if (file.exists(raw_firn_fp)) {
  surftype_r <- terra::mask(surftype_r, terra::intersect(project(vect(raw_firn_fp), crs(ref_grid_r)), outl_all_v), inverse = TRUE, updatevalue = 1)
}

# Add debris if we have them.
if (file.exists(raw_debris_fp)) {
  surftype_r <- terra::mask(surftype_r, terra::intersect(project(vect(raw_debris_fp), crs(ref_grid_r)), outl_all_v), inverse = TRUE, updatevalue = 5)
}

dir.create(outdir_surftype_fp, recursive = TRUE)
writeRaster(surftype_r, file.path(outdir_surftype_fp, paste0("surface_type_", name_glacier, "_", dem_years_str[i], ".tif")))



# Process radiation -------------------------------------------------------------------------------
dhm_all_lf    <- list.files(outdir_dem_fp, pattern = "\\.tif$")
dhm_latest_fp <- file.path(outdir_dem_fp, dhm_all_lf[length(dhm_all_lf)])
func_compute_all_daily_pisr(rast(dhm_latest_fp),
                            2020,
                            0.1,
                            name_glacier)
