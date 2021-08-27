library(raster)
library(tools) # file_path_sans_ext().

# Latitude (degrees north), passed to SAGA to compute solar radiation.
lat <- 42.125

# Set paths.
out_dirpath <- "../input/barkrak2/radiation"
dhm_filepath <- "../input/barkrak2/dhm/dhm_barkrak2_2019.tif"


# Name used during processing, don't change.
raddir_name <- "raddir"

# Convert elevation model to SAGA format.
dhm_name <- file_path_sans_ext(basename(dhm_filepath))
dhm <- raster(dhm_filepath)
writeRaster(dhm, paste0(dhm_name, ".sgrd"), overwrite = T)

ts_seq <- seq.POSIXt(from = as.POSIXct("2018/01/01"), to = as.POSIXct("2018/12/31"), by = "1 day")
ts_format <- format(ts_seq, "%Y-%m-%d")

lat_string <- paste("-LATITUDE=", lat, sep="")

# Do every day!
for (i in 1:length(ts_seq)) {
  
  # Prepare command line options.
  grd_dhm <- paste("-GRD_DEM=", dhm_name, ".sgrd", sep="")
  day_cur <- paste("-DAY=", ts_format[i], sep="")
  
  
  # Compute direct radiation.
  system2("saga_cmd", args = c("ta_lighting", "2", grd_dhm, "-GRD_DIRECT=raddir", "-SOLARCONST=1367.0", "-UNITS=0", lat_string, "-PERIOD=1", "-MOMENT=12.000000", "-HOUR_RANGE_MIN=0", "-HOUR_RANGE_MAX=24", "-METHOD=0", "-ATMOSPHERE=11400.0", day_cur, "-HOUR_STEP=0.1"))
  
  # Convert from daily kWh/m2 to W/m2.
  raddir <- raster(paste(raddir_name, ".sdat", sep="")) * 1000 / 24

  # Write ASCII grid.
  raddir_outgridname_asc <- file.path(out_dirpath, paste0(raddir_name, ".asc"))
  writeRaster(raddir, raddir_outgridname_asc, overwrite = T)
  
  # Rename to correct daily name.
  raddir_final_name <- file.path(out_dirpath, paste0("dir", sprintf("%03d",as.integer(format(ts_seq[i], "%j"))), "24.grid"))
  file.rename(raddir_outgridname_asc, raddir_final_name)
  
  # Remove auxiliary files.
  file.remove(paste0(raddir_name, ".sdat"))
  file.remove(paste0(raddir_name, ".mgrd"))
  file.remove(paste0(raddir_name, ".sgrd"))
  file.remove(paste0(raddir_name, ".prj"))
}

# Remove auxiliary files.
file.remove(paste0(dhm_name, ".sdat"))
file.remove(paste0(dhm_name, ".sgrd"))
