###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the code to repair the surface type file in case it is in    #
#                 conflict with the DEM (i.e. some rock cells inside glacier outline, and/or      #
#                 some glacier cells outside).                                                    #
###################################################################################################
func_repair_surface_type <- function(run_params,
                                     data_dems,
                                     data_surftype) {
  
  ids_surftype_processed <- integer(0)
  
  for (year_id in 1:run_params$n_years) {
    
    surftype_id <- data_surftype$grid_year_id[year_id]
    
    # Don't process twice a grid already processed.
    if (!(surftype_id %in% ids_surftype_processed)) {
      
      ids_surftype_processed <- c(ids_surftype_processed, surftype_id)
      
      dem_id                 <- data_dems$grid_year_id[year_id]
      
      # Look for rock cells inside the glacier outline.
      surftype_rock_ids                <- which(getValues(data_surftype$grids[[surftype_id]]) == 4)
      surftype_rock_inside_outline_ids <- intersect(data_dems$glacier_cell_ids[[dem_id]], surftype_rock_ids)
      ncells_bad                       <- length(surftype_rock_inside_outline_ids)
      
      # If any found, replace them with the closest value
      # which is not rock. To do this, we set all rock to NA,
      # then we compute the distance() and direction() rasters
      # to the closest non-NA cells, we compute the position of
      # these cells and we extract() the value at these positions.
      if (ncells_bad > 0) {
        
        cat(paste0("* WARNING: found ", ncells_bad, " rock cells inside glacier outline, in surface type grid #", surftype_id, ". I am fixing them right now.\n"))
        
        data_surftype$grids[[surftype_id]][surftype_rock_ids] <- NA
        dist_ras <- distance(data_surftype$grids[[surftype_id]])
        dir_ras  <- direction(data_surftype$grids[[surftype_id]], from = FALSE)
        
        bad_cells_coords    <- coordinates(data_surftype$grids[[surftype_id]])[surftype_rock_inside_outline_ids,]
        replacement_cells_x <- bad_cells_coords[,1] + dist_ras[surftype_rock_inside_outline_ids] * sin(dir_ras[surftype_rock_inside_outline_ids])
        replacement_cells_y <- bad_cells_coords[,2] + dist_ras[surftype_rock_inside_outline_ids] * cos(dir_ras[surftype_rock_inside_outline_ids])
        
        replacement_values  <- extract(data_surftype$grids[[surftype_id]], cbind(replacement_cells_x, replacement_cells_y), method = "simple")
        data_surftype$grids[[surftype_id]][surftype_rock_inside_outline_ids] <- replacement_values
        
      }
      
      # Now set to rock the surface type of all
      # cells outside the glacier outline.
      # This also fixes any glacier-surface (i.e.
      # value != 4) outside the glacier outline.
      data_surftype$grids[[surftype_id]][data_dems$no_glacier_cell_ids[[dem_id]]] <- 4
      
    } # End check if grid already processed.
  }   # End loop over modeled years.
  
  return(data_surftype)
} 
