# This utility converts a shapefile into an XYZN file.
# Multiple polygons and holes are supported.
# Any data besides XY coordinates are discarded.
# Elevation (Z) is set to 9999.
library(sf)

infilename <- "../../../mbmodel_presentation/example_shp/example.shp"
outfilename <- "../../../mbmodel_presentation/example_shp/example.xyzn"
dat <- st_read(infilename, check_ring_dir = TRUE)

out <- data.frame(X = numeric(0), Y = numeric(0), Z = numeric(0), N = numeric(0))

n_entries <- length(dat$id)

for (entry_id in 1:n_entries) {
  
  entry_cur <- data.frame(st_coordinates(dat$geometry[entry_id]))
  
  n_pols <- length(unique(entry_cur$L1))
  pol_ncols <- ncol(entry_cur) # This to handle as generally as possible the cases of a Z (and/or M) column after X and Y.
  
  # Here pol_id = 1 is the actual area, all subsequent ones are hole within the area.
  for (pol_id in 1:n_pols) {
    
    pol_cur <- entry_cur[which(entry_cur$L1 == pol_id),1:(pol_ncols-2)]
    pol_npoints <- nrow(pol_cur)
    pol_df <- data.frame(X = pol_cur$X,
                         Y = pol_cur$Y,
                         Z = 9999,
                         N = c(21, rep(22, pol_npoints-2), 23))
    
    out <- rbind(out, pol_df)
    
  }
  
}

write.table(out, outfilename, quote = F, row.names = F, col.names = F)
