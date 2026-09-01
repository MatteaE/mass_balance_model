###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the loading routine for the point mass balance measurements. #
#                 load_what controls whether we load annual mass balance or winter mass balance.  #
#                 As output we get a data.frame:                                                  #
#                   id start_date end_date x y z dh_cm density                                    #
#                 start_date = NA is interpreted as <end of previous ablation season>, useful     #
#                 for probe/snowpit measurements.                                                 #
################################################################################################### 

func_load_massbalance_measurements <- function(run_params,
                                               load_what,
                                               data_dhms) {
  
  cat("  Loading", load_what, "mass balance measurements...\n")
  
  # Check whether a mass balance data file was supplied at all ------------------------------------
  if (load_what == "annual") {
    
    # No annual measurements. Return dummy data frame for them.
    if (nchar(run_params$filename_massbalance_annual) == 0) {
      
      data_massbalance_annual_dummy <- data.frame(id = "none",
                                                  start_date = as.Date("1000/10/01"),
                                                  end_date = as.Date("1001/09/30"),
                                                  x = 0,
                                                  y = 0,
                                                  z = 0,
                                                  dh_cm = 0,
                                                  density = 0)
      return(data_massbalance_annual_dummy)
    }
    
    massbalance_path <- file.path(normalizePath(run_params$dir_data_massbalance),
                                  run_params$filename_massbalance_annual)
    
  } else if (load_what == "winter") {
    
    # No winter measurements. Return dummy data frame for them.
    if (nchar(run_params$filename_massbalance_winter) == 0) {
      
      data_massbalance_winter_dummy <- data.frame(id = "none",
                                                  start_date = as.Date("1000/10/01"),
                                                  end_date = as.Date("1001/09/30"),
                                                  x = 0,
                                                  y = 0,
                                                  z = 0,
                                                  dh_cm = 0,
                                                  density = 0)
      return(data_massbalance_winter_dummy)
    }
    
    massbalance_path <- file.path(normalizePath(run_params$dir_data_massbalance),
                                  run_params$filename_massbalance_winter)
  }
  
  if (!file.exists(massbalance_path)) {
    func_customlog("The ", load_what, " mass balance file does not exist: ", massbalance_path, level = 2)
    func_stop()
  }
  
  
  # Read file, assign column names ----------------------------------------------------------------
  tryCatch({data_massbalance <- read.table(massbalance_path,
                                           header = FALSE,
                                           stringsAsFactors = FALSE,
                                           colClasses = c("character",    # Id is a character
                                                          "character",    # Dates start out as characters, to check their format
                                                          "character",    # Dates start out as characters, to check their format
                                                          "character",    # Coordinates start out as characters, to check their format
                                                          "character",    # Coordinates start out as characters, to check their format
                                                          "numeric",      # Provided z value can be converted to numeric, it is not used anyway
                                                          "character",    # dh starts out as character, to check its format
                                                          "character"))}, # density starts out as character, to check its format
           error = function(err) {
             func_customlog("Error reading the ", load_what, " mass balance file: ", massbalance_path, level = 2)
             func_stop()
           })
  
  
  massbal_cols <- c("id", "start_date", "end_date", "x", "y", "z", "dh_cm", "density")
  if (ncol(data_massbalance) != 8) {
    func_customlog("The ", load_what, " mass balance file does not have eight columns.", level = 2)
    func_customlog("        Please fix the file manually: ", massbalance_path, level = 0)
    func_customlog("        Expected columns (no titles): ", paste0(massbal_cols, collapse = " | "), level = 0)
    func_stop()
  }
  names(data_massbalance) <- massbal_cols
  
  # Process columns -------------------------------------------------------------------------------
  # Convert timestamps to Date objects.
  # Careful checks on NA - it is allowed as start date
  # (means "automatically determine mass balance minimum"),
  # but anything else should parse correctly.
  # . Validate start date -------------------------------------------------------------------------
  ids_start_date_na_before    <- which(is.na(data_massbalance$start_date))
  start_date_orig             <- data_massbalance$start_date
  data_massbalance$start_date <- as.Date(data_massbalance$start_date, format = "%d.%m.%Y")
  ids_start_date_na_after     <- which(is.na(data_massbalance$start_date))
  if (length(ids_start_date_na_after) != length(ids_start_date_na_before)) {
    wrong_ids      <- sort(setdiff(ids_start_date_na_after, ids_start_date_na_before))
    func_customlog("Found ", length(wrong_ids), " wrong start dates in the ", load_what, " mass balance file.", level = 2)
    func_customlog("        Please fix the file manually: ", massbalance_path, level = 0)
    func_customlog("        The first wrong value is: ", start_date_orig[wrong_ids[1]],
                   " (point id ", data_massbalance$id[wrong_ids[1]], " at line ", wrong_ids[1], ")", level = 0)
    func_stop()
  }
  
  # . Validate end date ---------------------------------------------------------------------------
  end_date_orig             <- data_massbalance$end_date
  data_massbalance$end_date <- as.Date(data_massbalance$end_date, format = "%d.%m.%Y")
  ids_end_date_na           <- which(is.na(data_massbalance$end_date))
  if (length(ids_end_date_na) > 0) {
    func_customlog("Found ", length(ids_end_date_na), " wrong end dates in the ", load_what, " mass balance file.", level = 2)
    func_customlog("        Please fix the file manually: ", massbalance_path, level = 0)
    func_customlog("        The first wrong value is: ", end_date_orig[ids_end_date_na[1]],
                   " (point id ", data_massbalance$id[ids_end_date_na[1]], " at line ", ids_end_date_na[1], ")", level = 0)
    func_stop()
  }
  
  
  # . Validate coordinates ------------------------------------------------------------------------
  x_orig <- data_massbalance$x
  y_orig <- data_massbalance$y
  
  data_massbalance$x <- suppressWarnings(as.numeric(data_massbalance$x))
  data_massbalance$y <- suppressWarnings(as.numeric(data_massbalance$y))
  
  ids_coords_bad <- which(is.na(data_massbalance$x) | is.na(data_massbalance$y))
  if (length(ids_coords_bad) > 0) {
    func_customlog("Found ", length(ids_coords_bad), " wrong (non-numeric) coordinate values in the ", load_what, " mass balance file.", level = 2)
    func_customlog("        Please fix the file manually: ", massbalance_path, level = 0)
    func_customlog("        The first wrong value is: ", x_orig[ids_coords_bad[1]], " | ", y_orig[ids_coords_bad[1]],
                   " (point id ", data_massbalance$id[ids_coords_bad[1]], " at line ", ids_coords_bad[1], ")", level = 0)
    func_stop()
  }
  
  # . Validate altitude change and density --------------------------------------------------------
  dh_orig <- data_massbalance$dh_cm
  data_massbalance$dh_cm <- suppressWarnings(as.numeric(data_massbalance$dh_cm))
  ids_dh_bad <- which(is.na(data_massbalance$dh_cm))
  if (length(ids_dh_bad) > 0) {
    func_customlog("Found ", length(ids_dh_bad), " wrong (non-numeric) mass balance values in the ", load_what, " mass balance file.", level = 2)
    func_customlog("        Please fix the file manually: ", massbalance_path, level = 0)
    func_customlog("        The first wrong value is: ", dh_orig[ids_dh_bad[1]],
                   " (point id ", data_massbalance$id[ids_dh_bad[1]], " at line ", ids_dh_bad[1], ")", level = 0)
    func_stop()
  }
  density_orig <- data_massbalance$density
  data_massbalance$density <- suppressWarnings(as.numeric(data_massbalance$density))
  ids_density_bad <- which(is.na(data_massbalance$density))
  if (length(ids_density_bad) > 0) {
    func_customlog("Found ", length(ids_density_bad), " wrong (non-numeric) density values in the ", load_what, " mass balance file.", level = 2)
    func_customlog("        Please fix the file manually: ", massbalance_path, level = 0)
    func_customlog("        The first wrong value is: ", density_orig[ids_density_bad[1]],
                   " (point id ", data_massbalance$id[ids_density_bad[1]], " at line ", ids_density_bad[1], ")", level = 0)
    func_stop()
  }
  
  
  
  
  # . Compute mass balance ------------------------------------------------------------------------
  data_massbalance$massbal <- data_massbalance$dh_cm * data_massbalance$density * 10 # 10: go from cm w.e. to mm w.e.
  
  
  # Spatial check on mass balance point coordinates -----------------------------------------------
  # Are there any mass balance points with coordinates outside the DHM?
  # If yes and the project uses UTM, first try to rescue them by assuming they have a wrong CRS
  # (test adjacent UTM zones as well as lon/lat). If that fails, drop them.
  # If the project does not use UTM, drop them.
  # We check over the combined extent of all DHMs.
  ext_limits <- ext(sprc(data_dhms$elevation))
  ids_df_bad <- which((data_massbalance$x < xmin(ext_limits)) |
                        (data_massbalance$x > xmax(ext_limits)) |
                        (data_massbalance$y < ymin(ext_limits)) |
                        (data_massbalance$y > ymax(ext_limits)))
  ids_bad_n <- length(ids_df_bad)
  
  if (ids_bad_n > 0) {
    func_customlog("The ", load_what, " mass balance file contains ", ids_bad_n, " entries which fall outside all the elevation grids.", level = 1)
    
    # This will be populated in case any stakes can be rescued.
    stake_coords_rescued_ids <- NULL
    
    # If the project uses UTM, check adjacent UTM zones.
    if (run_params$grids_crs %in% c(32601:32660, 32701:32760)) {
      
      func_customlog("          Checking whether they have the wrong reference system (wrong UTM zone or lon/lat)...", level = 0)
      utm_test_allowed <- run_params$grids_crs + c(-2, -1, 1, 2)
      utm_test_allowed <- utm_test_allowed[utm_test_allowed %in% c(32601:32660, 32701:32760)]
      
      for (i in 1:ids_bad_n) {
        stake_coords_fixed <- func_fix_stake_coordinates(data_massbalance$id[ids_df_bad[i]],
                                                         c(data_massbalance$x[ids_df_bad[i]], data_massbalance$y[ids_df_bad[i]]),
                                                         ext_limits,
                                                         c(utm_test_allowed, 4326),
                                                         run_params$grids_crs)
        # Successfully rescued the current pair by changing coordinates system.
        if (all(!is.na(stake_coords_fixed))) {
          data_massbalance$x[ids_df_bad[i]] <- stake_coords_fixed[1]
          data_massbalance$y[ids_df_bad[i]] <- stake_coords_fixed[2]
          stake_coords_rescued_ids <- c(stake_coords_rescued_ids, ids_df_bad[i])
        } # End if successfully rescued a stake.
      } # End loop on the bad stakes.
    } else { # End if the project uses UTM - else, don't try to guess adjacent UTM zones.
      func_customlog("          The project is not in UTM projection - it is not possible to guess an adjacent UTM zone.", level = 0)
    }
    
    stake_coords_rescued_n <- length(stake_coords_rescued_ids)
    if (stake_coords_rescued_n > 0) {
      func_customlog("          Successfully recovered ", stake_coords_rescued_n, " entries with a wrong coordinate system.", level = 0)
    } else {
      func_customlog("          No entries could be recovered. Please check them manually.", level = 0)
      if (ids_bad_n == nrow(data_massbalance)) {
        func_customlog("        All mass balance points are not usable. Please fix the mass balance file and run again.", level = 2)
        func_stop()
      }
    }
    ids_df_bad <- setdiff(ids_df_bad, stake_coords_rescued_ids) # Don't remove rescued stakes.
    if (stake_coords_rescued_n < ids_bad_n) {
      cat("\n")
      func_customlog("Discarding ", ids_bad_n - stake_coords_rescued_n, " mass balance entries with wrong coordinates, which could not be recovered.", level = 1)
      func_customlog("          Please investigate and correct these manually. Maybe X and Y are swapped?", level = 0)
      stake_bad_first            <- data_massbalance[ids_df_bad[1],]
      stake_bad_first$start_date <- format(stake_bad_first$start_date, "%d.%m.%Y")
      stake_bad_first$end_date   <- format(stake_bad_first$end_date, "%d.%m.%Y")
      func_customlog("          The first problematic entry is:", level = 0)
      func_customlog("          ", paste0(stake_bad_first[,1:(ncol(stake_bad_first)-1)], collapse = " | "), level = 0)
      data_massbalance           <- data_massbalance[-ids_df_bad,]
    } else {
      func_customlog("          All problematic entries were successfully recovered by reprojection.", level = 0)
    }
  }
  
  
  # Cluster measurements according to a user-defined distance -------------------------------------
  # This to improve the spatial distribution / representativity.
  # We skip this step in case we have only one measurement
  # (can be the case if we have a dummy file for winter stakes).
  if ((nrow(data_massbalance) > 1) && (run_params$stake_cluster_distance > 0)) {
    
    # We only cluster together stakes which are within the distance
    # AND were measured on the same date (both at the start
    # and at the end of their observation period).
    stakes_dists_spatial <- spDists(cbind(data_massbalance$x, data_massbalance$y), longlat = FALSE)
    # This temporary vector of starting dates is used
    # to cluster stakes only if they have the same
    # starting date. Since the starting date can also be NA,
    # we group together all NAs which have a same ending year
    # (NA means "end of the melting season"; since we only
    # cluster together stakes which are close to each other,
    # we assume that they end their melting season on the same day,
    # which is reasonable).
    stakes_start_date_temp <- data_massbalance$start_date
    stake_start_na_ids_logi <- is.na(stakes_start_date_temp)
    if (any(stake_start_na_ids_logi)) {
      stakes_start_date_temp[stake_start_na_ids_logi] <- as.Date(paste0(as.integer(format(data_massbalance$end_date[stake_start_na_ids_logi], "%Y")) - 1, "/01/01"))
    }
    
    stakes_dists_startdate <- as.matrix(stats::dist(stakes_start_date_temp))
    stakes_dists_enddate <- as.matrix(stats::dist(data_massbalance$end_date))
    stakes_dists_date <- 1/((stakes_dists_startdate == 0) * (stakes_dists_enddate == 0)) # 1 if two stakes have the same observation period, else Infinity.
    
    stakes_dist_proc <- stakes_dists_spatial*stakes_dists_date
    stakes_dist_proc[is.infinite(stakes_dist_proc)] <- 1e9 # Clustering does not like infinity. So we use a very big number instead.
    stakes_dist_proc[is.nan(stakes_dist_proc)]      <- 1e9 # This in case we have two stakes at the same place but on different years (0*Inf = NaN, we shouldn't merge them).
    
    # Clustering happens here.
    stakes_clusters     <- hclust(stats::as.dist(stakes_dist_proc))
    stakes_clusters_cut <- cutree(stakes_clusters, h = run_params$stake_cluster_distance)
    
    # Prepare output data frame. We discard the dh_cm and density columns, we only keep mass balance.
    data_massbalance_filtered <- data_massbalance[integer(0),c(1:6,9)]
    
    # Compute values of the clusters (arithmetic means;
    # for start/end dates we just take the value from the
    # first cluster element since they are all the same).
    # Add them to the output data frame.
    clusters_n <- max(stakes_clusters_cut)
    clusters_multistake_n <- 0
    for (cluster_id in 1:clusters_n) {
      cluster_stakes_id <- as.integer(which(stakes_clusters_cut == cluster_id))
      cluster_name <- ifelse(length(cluster_stakes_id) > 1, paste0("CL", sprintf("%03d", cluster_id)), data_massbalance$id[cluster_stakes_id])
      cluster_start_date <- data_massbalance$start_date[cluster_stakes_id[1]]
      cluster_end_date <- data_massbalance$end_date[cluster_stakes_id[1]]
      cluster_x <- mean(data_massbalance$x[cluster_stakes_id])
      cluster_y <- mean(data_massbalance$y[cluster_stakes_id])
      cluster_z <- mean(data_massbalance$z[cluster_stakes_id])
      cluster_massbal <- mean(data_massbalance$massbal[cluster_stakes_id])
      data_massbalance_filtered <- rbind(data_massbalance_filtered, data.frame(id = cluster_name,
                                                                               start_date = cluster_start_date,
                                                                               end_date = cluster_end_date,
                                                                               x = cluster_x,
                                                                               y = cluster_y,
                                                                               z = cluster_z,
                                                                               massbal = cluster_massbal,
                                                                               stringsAsFactors = FALSE))
      if (length(cluster_stakes_id) > 1) {
        clusters_multistake_n <- clusters_multistake_n + 1
      }
    }
    
  } else {
    data_massbalance_filtered <- data_massbalance[,c(1:6,9)]
    clusters_multistake_n <- 0
  }
  
  cat("    Loading complete. There are", nrow(data_massbalance_filtered), load_what, "mass balance values.\n")
  
  if (run_params$stake_cluster_distance > 0) {
    cat("    Of those,", clusters_multistake_n, "are clusters resulting from the aggregation of multiple mass balance points, as controlled by parameter stake_cluster_distance.\n")
  }
  
  return(data_massbalance_filtered)
  
}

