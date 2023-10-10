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

func_load_massbalance_measurements <- function(run_params, load_what, data_dhms) {
  
  cat("  Loading", load_what, "mass balance measurements...\n")
  
  if (load_what == "annual") {
    
    # No winter measurements. Return dummy data frame.
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
    
    massbalance_path <- file.path(run_params$dir_data_massbalance,
                                  run_params$filename_massbalance_annual)
    
  } else if (load_what == "winter") {
    
    # No winter measurements. Return dummy data frame.
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
    
    massbalance_path <- file.path(run_params$dir_data_massbalance,
                                  run_params$filename_massbalance_winter)
  }
  
  if (!file.exists(massbalance_path)) {
    cat("* FATAL: I could not find the file with mass balance measurements. The specified path is", massbalance_path, "\n")
    stop()
  }
  
  # Read file, assign column names.
  data_massbalance <- read.table(massbalance_path, header = FALSE, stringsAsFactors = FALSE)
  names(data_massbalance) <- c("id", "start_date", "end_date", "x", "y", "z", "dh_cm", "density")
  
  # Convert timestamps to Date objects.
  data_massbalance$start_date <- as.Date(data_massbalance$start_date, format = "%d.%m.%Y")
  data_massbalance$end_date <- as.Date(data_massbalance$end_date, format = "%d.%m.%Y")
  
  # Compute mass balance.
  data_massbalance$massbal <- data_massbalance$dh_cm * data_massbalance$density * 10 # 10: cm w.e. to mm w.e.
  
  # Remove from mass balance df any stakes with coordinates outside the DHM.
  ext_limits <- ext(data_dhms$elevation[[1]])
  ids_df_bad <- which((data_massbalance$x < xmin(ext_limits)) |
                        (data_massbalance$x > xmax(ext_limits)) |
                        (data_massbalance$y < ymin(ext_limits)) |
                        (data_massbalance$y > ymax(ext_limits)))
  ids_bad_n <- length(ids_df_bad)
  if (ids_bad_n > 0) {
    cat("* WARNING: the", load_what, "mass balance file contains", ids_bad_n, "entries which fall outside the DHM.\n")
    stake_coords_rescued_ids <- NULL
    for (i in 1:ids_bad_n) {
      stake_coords_fixed <- func_fix_stake_coordinates(c(data_massbalance$x[ids_df_bad[i]], data_massbalance$y[ids_df_bad[i]]),
                                                       ext_limits,
                                                       c(run_params$grids_crs + c(-2, -1, 1, 2), 4326),
                                                       run_params$grids_crs)
      # Rescued the current pair by changing coordinates system!
      if (all(!is.na(stake_coords_fixed))) {
        data_massbalance$x[ids_df_bad[i]] <- stake_coords_fixed[1]
        data_massbalance$y[ids_df_bad[i]] <- stake_coords_fixed[2]
        stake_coords_rescued_ids <- c(stake_coords_rescued_ids, ids_df_bad[i])
      }
    }
    stake_coords_rescued_n <- length(stake_coords_rescued_ids)
    cat("    I could rescue", stake_coords_rescued_n, "entries with a wrong coordinate system.\n")
    ids_df_bad <- setdiff(ids_df_bad, stake_coords_rescued_ids) # Don't remove rescued stakes.
    if (stake_coords_rescued_n < ids_bad_n) {
      cat("* WARNING: I am discarding the", ids_bad_n - stake_coords_rescued_n, "remaining entries, I could not fix them. You should investigate and correct them manually.\n")
      cat("    The first problematic entry reports these coordinates:", data_massbalance$x[ids_df_bad[1]], "|", data_massbalance$y[ids_df_bad[1]], "\n")
      data_massbalance <- data_massbalance[-ids_df_bad,]
    } else {
      cat("    All problematic entries could be rescued by reprojection.\n")
    }
  }
  
  
  # Cluster measurements according to a user-defined distance.
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
    stakes_dist_proc[is.nan(stakes_dist_proc)] <- 1e9 # This in case we have two stakes at the same place but on different years (0*Inf = NaN, we shouldn't merge them).
    
    # Clustering happens here.
    stakes_clusters <- hclust(stats::as.dist(stakes_dist_proc))
    stakes_clusters_cut <- cutree(stakes_clusters, h = run_params$stake_cluster_distance)
    
    # Prepare output data frame. We discard the dh_cm and density columns, we only keep mass balance.
    data_massbalance_filtered <- data_massbalance[integer(0),c(1:6,9)]
    
    # Compute values of the clusters (arithmetic means;
    # for start/end dates we just take the value from the
    # first cluster element since they are all the same).
    # Add them to the output data frame.
    clusters_n <- max(stakes_clusters_cut)
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
    }
    
  } else {
    data_massbalance_filtered <- data_massbalance[,c(1:6,9)]
  }
  
  cat("    Loading complete. I have", nrow(data_massbalance_filtered), load_what, "mass balance values.\n")
  
  return(data_massbalance_filtered)
  
}

