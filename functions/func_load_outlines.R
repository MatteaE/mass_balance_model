###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for the glacier vector outlines.         #
#                 At the moment only XYZN outlines are supported.                                 #
#                 Each outline is loaded only once and recycled as needed in the closest years    #
#                 which don't have their own outline.                                             #
#                 Vector outlines are used only for the plots, not for the processing.            #
###################################################################################################

func_load_outlines <- function(run_params) {
  
  cat("  Loading glacier outlines...\n")
  
  ext_str_raw <- str_split(run_params$filename_outline_suffix, fixed("."))[[1]]
  ext_str <- ext_str_raw[length(ext_str_raw)]
  if (ext_str == "xyzn") {
    outline_filetype <- "xyzn"
  } else if (ext_str == "shp") {
    outline_filetype <- "shapefile"
  } else {
    stop("    FATAL: outline file specification is not either XYZN or shapefile (.shp). Please convert outlines to the correct format and run the model again.")
  }
  
  outlines_out <- list(outlines = list(),
                       outline_year_id = rep(NA, run_params$n_years)) # Here we put all the loaded outlines.
  
  cat("    Looking for", outline_filetype, "outline files...\n")
  
  run_params <- func_find_input_files_single(run_params, "outline")
  outline_paths <- run_params$outline_paths
  outline_n <- length(outline_paths)
  
  if (outline_n == 0) {
    stop("    FATAL: no outline files found. Please check parameters dir_data_outline, filename_outline_prefix and filename_outline_suffix.")
  } else {
    cat("    Found", outline_n, "outline file(s). Available year(s):", run_params$outline_years, "\n")
  }
  
  # Load outlines
  for (outline_id in 1:length(outline_paths)) {
    if (outline_filetype == "xyzn") {
      outlines_out$outlines[[outline_id]] <- func_load_xyzn(outline_paths[outline_id], run_params$grids_crs)
    } else if (outline_filetype == "shapefile") {
      invisible(capture.output(outlines_out$outlines[[outline_id]] <- as(as_Spatial(st_read(outline_paths[outline_id])), "SpatialPolygons")))
    }
    # Aspect ratio: > 1 if tall glacier, < 1 if wide glacier. Used to add margins to the area plots,
    # in order to keep the plot titles within the page margins.
    outline_ext <- extent(outlines_out$outlines[[outline_id]])
    outlines_out$aspect_ratio[[outline_id]] <- (outline_ext[4] - outline_ext[3]) / (outline_ext[2] - outline_ext[1])
  }
  
  # For each modeled year find the closest grid year and use its grid.
  for (year_cur_id in 1:run_params$n_years) {
    year_cur <- run_params$years[year_cur_id]
    outline_year_closest_id <- which.min(abs(run_params$outline_years - year_cur))
    outlines_out$outline_year_id[year_cur_id] <- outline_year_closest_id
  }
  
  return(outlines_out)
  
}
