###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to find input files according to the specified   #
#                 prefix and suffix patterns. It is generic and used for dhm, surface type and    #
#                 outline input.                                                                  #
###################################################################################################

func_find_input_files_single <- function(run_params,
                                        input_type) {
  
  dirpath         <- run_params[[paste0("dir_data_", input_type)]]
  filename_prefix <- run_params[[paste0("filename_", input_type, "_prefix")]]
  filename_suffix <- run_params[[paste0("filename_", input_type, "_suffix")]]
  
  dir_files_all_with_subdir_path <- list.files(dirpath, recursive = TRUE)
  dir_files_all_names_only       <- basename(dir_files_all_with_subdir_path)
  dir_files_interesting_ids      <- which((startsWith(dir_files_all_names_only, filename_prefix)) &
                                       (endsWith(dir_files_all_names_only, filename_suffix)))
  
  dir_files_interesting          <- dir_files_all_with_subdir_path[dir_files_interesting_ids]
  dir_files_interesting_names    <- basename(dir_files_interesting)
  dir_files_interesting_years    <- suppressWarnings(as.integer(str_remove_all(str_remove_all(dir_files_interesting_names, coll(filename_prefix)), coll(filename_suffix))))
  
  dir_files_years_bad            <- which(is.na(dir_files_interesting_years))
  
  if (length(dir_files_years_bad) > 0) {
    dir_files_interesting        <- dir_files_interesting[-dir_files_years_bad]
    dir_files_interesting_years  <- dir_files_interesting_years[-dir_files_years_bad]
  }

  dir_files_interesting_years_sorted <- sort.int(dir_files_interesting_years, index.return = TRUE)
  dir_files_interesting_abspaths     <- normalizePath(file.path(dirpath, dir_files_interesting[dir_files_interesting_years_sorted$ix]))
  
  run_params[[paste0(input_type, "_years")]] <- dir_files_interesting_years_sorted$x
  run_params[[paste0(input_type, "_paths")]] <- dir_files_interesting_abspaths
  
  return(run_params)
  
}
