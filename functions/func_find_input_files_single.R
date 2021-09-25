###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to find input files according to the specified   #
#                 prefix and suffix patterns. It is generic and used for dhm, surface type and    #
#                 outline input.                                                                  #
###################################################################################################

# Logic:
# If input file is dhm or surface type, prefer .tif input, then .grid, then .asc.
# Second criterion: in case of files (in subdirs) with same name and extension,
# prefer files which are closer to the directory tree root.
# If input file is outline, then prefer .shp (second choice: .xyzn).
func_find_input_files_single <- function(run_params,
                                         input_type) {
  
  dirpath         <- run_params[[paste0("dir_data_", input_type)]]
  filename_prefix <- run_params[[paste0("filename_", input_type, "_prefix")]]
  filename_suffix <- run_params[[paste0("filename_", input_type, "_suffix")]]
  
  if (input_type %in% c("dhm", "surftype")) {
    filename_extensions <- c(".tif", ".grid", ".asc")
  } else {
    filename_extensions <- c(".shp", ".xyzn")
  }
  n_ext <- length(filename_extensions)
  
  filenames_allowed_no_ext <- paste0(filename_prefix, run_params$years_input_allowed, filename_suffix)
  
  dir_files_all_with_subdir_path <- list.files(dirpath, recursive = run_params$dir_data_recursive)
  dir_files_all_names_only       <- basename(dir_files_all_with_subdir_path) # Needed in case we find files in subdirs.
  dir_files_n <- length(dir_files_all_names_only)
  
  # Matrix 1 row per year, 1 column per extension (currently 3 allowed
  # extensions: .tif, .grid and .asc, in order of preference).
  # We handle the unlikely case in which there are two grids with same name
  # except for the extension, we prefer the extension which comes first in the
  # filename_extensions character vector.
  filenames_found_exts <- matrix(FALSE, nrow = run_params$years_input_allowed_n, ncol = n_ext)
  
  # To handle subdirs.
  filepaths_pmatch <- matrix(NA_integer_, nrow = dir_files_n, ncol = n_ext)
  filepaths_nchar <- nchar(dir_files_all_with_subdir_path)
  
  for (ext_id in 1:length(filename_extensions)) {
    ext_cur <- filename_extensions[ext_id]
    pmatch_res <- pmatch(dir_files_all_names_only, paste0(filenames_allowed_no_ext, ext_cur), duplicates.ok = TRUE)
    filenames_found_exts[as.integer(na.omit(pmatch_res)), ext_id] <- TRUE
    filepaths_pmatch[,ext_id] <- pmatch_res
    # Remove "found" assessment in case there was another file
    # with same name but different extension already found (we
    # prefer the first extension in filename_extensions).
    if (ext_id > 1) {
      if (ext_id > 2) {
        prev_exts_available <- rowSums(filenames_found_exts[,1:(ext_id-1)]) > 0
      } else {
        prev_exts_available <- as.integer(filenames_found_exts[,1]) > 0
      }
      filenames_found_exts[prev_exts_available, ext_id] <- FALSE
    }
  }
  years_available <- run_params$years_input_allowed[which(rowSums(filenames_found_exts) > 0)]

  # Handle the very rare case in which there is
  # a valid file name in the input dir and also (same
  # name and extension) in a subdir: we always pick the
  # file with the shortest path (i.e. outside subdirs
  # if possible, in any case least depth).
  paths_out_rel <- NULL
  for (year_id in 1:run_params$years_input_allowed_n) {
    if (run_params$years_input_allowed[year_id] %in% years_available) {
      year_ext_id <- which(filenames_found_exts[year_id,])
      filepaths_cur_year <- which(filepaths_pmatch[,year_ext_id] == year_id)
      year_ext_filepath_nchars <- filepaths_nchar[filepaths_cur_year]
      filepath_shortest_id <- which.min(year_ext_filepath_nchars)
      filepath_shortest <- dir_files_all_with_subdir_path[filepaths_cur_year][filepath_shortest_id]
      paths_out_rel <- append(paths_out_rel, filepath_shortest)
    }
  }
  
  paths_out_abs <- normalizePath(file.path(dirpath, paths_out_rel))
  
  run_params[[paste0(input_type, "_years")]] <- years_available
  run_params[[paste0(input_type, "_paths")]] <- paths_out_abs
  
  return(run_params)
  
}
