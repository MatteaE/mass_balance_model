###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the routine to run the mass balance model over a period      #
#                 (winter or year).                                                               #
###################################################################################################

# NOTE: this R implementation is actually quite fast.
# The C++ implementation (under directory old/) is actually
# slower, so we keep this one.

# year_cur_params are the melt/accumulation model parameters which we will optimize!
# (For the first iteration we take the ones loaded from file).
func_massbal_model <- function(run_params,
                               year_cur_params,
                               dhm_values,
                               glacier_cell_ids,
                               surftype_init_values,
                               snowdist_init_values,
                               radiation_values_list,
                               weather_series_cur,
                               snowdist_topographic_values_red,
                               snowdist_probes_norm_values_red,
                               grids_avalanche_cur,
                               grid_ice_albedo_fact_cur_values) {
  
  # t1 <- Sys.time()
  
  # Compute here because the snow and ice factors are subject
  # to optimization, this always stays in the middle of them.
  year_cur_params$rad_fact_firn <- (year_cur_params$rad_fact_ice + year_cur_params$rad_fact_snow) / 2 # As per IDL implementation.
  
  
  # This should be equal to the length of the selected weather series!
  # model_days_n <- as.integer(difftime(model_time_bounds[2], model_time_bounds[1], "days"))
  model_days_n <- nrow(weather_series_cur)
  
  
  #### CORRECT PRECIPITATION UNDERCATCH ####
  # Correct precipitation undercatch with given parameters
  # (lower correction in summer).
  # We have to do it here since the correction factor
  # is subject to optimization!
  # A prec_corr = 100 means no correction (100 % of the original precipitation.)
  weather_series_cur$precip_corr <- weather_series_cur$precip * (year_cur_params$prec_corr / 100.)
  ids_summer_logi <- (as.integer(format(weather_series_cur$timestamp, "%m")) %in% 5:9) # Logical indices: TRUE in May to September, FALSE elsewhere.
  weather_series_cur$precip_corr[ids_summer_logi] <- weather_series_cur$precip[ids_summer_logi] * (year_cur_params$prec_summer_fact * year_cur_params$prec_corr / 100.)
  
  
  #### CREATE OUTPUT VECTORS ####
  # vec_items_n is the length of each modeled vector.
  # NOTE: modeled vectors have one timestep more than weather vectors because
  # they also store the initial timestep.
  # We store each daily map in
  # (nrow * ncol) components of the vector, going sequentially.
  # The first stored map holds the initial conditions (before the first day),
  # the last holds the final conditions (after the last day).
  # The vector element at index (2*nrow*ncol) + (ncol + 1) is then
  # the first (leftmost) cell of the second row, after the second day
  # of melt and accumulation.
  # Remember that C++ indices start at 0 instead of 1!
  vec_items_n <- run_params$grid_ncol * run_params$grid_nrow * (model_days_n + 1)
  
  # The first three vectors will hold all the output of the mass balance model.
  # NOTE BUG BUG BUG: here we have 245 MB (for Pers) which are allocated but never freed, big memory leak!!
  vec_snow_swe      <- rep(NA_real_, vec_items_n)
  vec_surf_type     <- rep(NA_real_, vec_items_n) # We use 0 for ice, 1 for firn, 2 for snow, 4 for rock, 5 for debris.
  vec_massbal_cumul <- rep(NA_real_, vec_items_n)
  melt_cur          <- rep(NA_real_, run_params$grid_ncells) # This instead holds only a single timestep. Here goes the daily melt amount.
  gl_massbal_cumul  <- rep(0.0, model_days_n + 1)   # This holds the cumulative glacier-wide mass balance.
  gl_melt_daily     <- rep(0.0, model_days_n + 1)   # This holds the daily mean melt amount over the glacier.
  gl_accum_daily    <- rep(0.0, model_days_n + 1)   # This holds the daily mean accumulation amount over the glacier.
  
  # Fill vectors with initial conditions.
  vec_snow_swe[1:run_params$grid_ncells]  <- snowdist_init_values
  vec_surf_type[1:run_params$grid_ncells] <- surftype_init_values
  vec_surf_type[which(vec_snow_swe[1:run_params$grid_ncells] > 0)] <- 2  # Add computed snow to the initial ice/firn/debris map.
  vec_massbal_cumul[1:run_params$grid_ncells] <- 0
  
  # This below is a vector[ncells] to keep track of the swe
  # after the last avalanche: we assume that snow can be
  # avalanched only once, so the input grid to the avalanche
  # model will be the difference between the current swe
  # and the latest swe_post_previous_avalanche (clamped to
  # positive values in case of significant melt).
  swe_post_previous_avalanche <- snowdist_init_values
  
  
  #### MAIN SIMULATION LOOP ####
  for (day_id in 1:model_days_n) {
    
    # cat("\r", day_id, "/", model_days_n)
    
    #### .  COMPUTE GRIDDED WEATHER OF THE DAY ####
    # Temperature in °C, snowfall in mm w.e.
    temp_cur <- weather_series_cur$t2m_mean[day_id] + year_cur_params$temp_elegrad * (dhm_values - run_params$weather_aws_elevation) / 100
    solid_prec_frac_cur <- clamp(((1 + run_params$weather_snowfall_temp) - temp_cur) / 2, 0, 1)
    accumulation_cur    <- snowdist_probes_norm_values_red * snowdist_topographic_values_red * solid_prec_frac_cur * weather_series_cur$precip_corr[day_id] * (1 + (pmin(run_params$weather_max_precip_ele, dhm_values) - run_params$weather_aws_elevation) * year_cur_params$prec_elegrad / 1e4 ) # 1e4: gradient is in [% / 100 m], we want [fraction / m].
    
    
    #### .  SETUP INDICES ####
    doy <- weather_series_cur$doy[day_id]
    
    radiation_cur <- radiation_values_list[[doy]]
    
    offset_cur <- day_id * run_params$grid_ncells # The _cur start at the end of the first day (i.e. there is one "iteration" before which holds the initial state).
    offset_prev <- (day_id - 1) * run_params$grid_ncells
    cells_cur  <- offset_cur + 1:run_params$grid_ncells # Indices of all the grid cells with values at the end of the current day.
    cells_prev <- cells_cur - run_params$grid_ncells    # Indices of all the grid cells with values at the beginning of the current day.

    
    #### .  AVALANCHE ROUTINE ####
    avalanche_condition <- FALSE
    if (format(weather_series_cur$timestamp[day_id], "%m/%d") %in% run_params$model_avalanche_dates) {
      avalanche_condition <- TRUE
    }
    # If we are to have an avalanche today, make it so
    # (first thing of the day, before melt and accumulation).
    # Algorithm:
      # compute grid of swe contributing to avalanche, as difference between current swe and last post-avalanche swe, clamped to positives
      # run avalanche on it
      # compute the new swe (sum of avalanche deposit and previous non-avalanched mass)
      # update cumulative mass balance, swe, surface type
      # NOTE: to update the vectors we use the cells_prev indices, since those are used as input to the melt model just below. This means that the mass balance change due to the avalanche is assigned to the day BEFORE the avalanche.
    if (avalanche_condition) {
      
      # cat("Avalanche!\n")
      
      avalanche_input_values        <- pmax(0.0, vec_snow_swe[cells_prev] - swe_post_previous_avalanche)
      avalanche_output              <- func_avalanche(run_params,
                                                      grids_avalanche_cur,
                                                      avalanche_input_values,
                                                      deposition_max_multiplier = 1.0,
                                                      preserve_edges = TRUE)

      # writeRaster(setValues(data_dhms$elevation[[1]], avalanche_output), "avalanche_output.tif", overwrite = T)
      
      # Update to current avalanche result.
      swe_post_previous_avalanche   <- avalanche_output + (vec_snow_swe[cells_prev] - avalanche_input_values)
      
      vec_massbal_cumul[cells_prev] <- vec_massbal_cumul[cells_prev] + swe_post_previous_avalanche - vec_snow_swe[cells_prev]
      gl_accum_daily[day_id + 1] <- gl_accum_daily[day_id + 1] + mean(swe_post_previous_avalanche[glacier_cell_ids] - vec_snow_swe[cells_prev][glacier_cell_ids]) # Update accumulation after avalanche.
      
      vec_snow_swe[cells_prev]      <- swe_post_previous_avalanche
      
      ids_snow_logi                             <- vec_snow_swe[cells_prev] > 0
      vec_surf_type[cells_prev][ids_snow_logi]  <- 2
      vec_surf_type[cells_prev][!ids_snow_logi] <- surftype_init_values[!ids_snow_logi]
    }

    
    #### .  MELT MODEL ####
    # Set the entire melt_cur to NA before computing,
    # to avoid any possible problems from values of the
    # previous iteration.
    melt_cur[1:run_params$grid_ncells] <- NA_real_
    # Melt ice, firn and debris-covered cells.
    # We take the surf type from the previous
    # timestep to find out which cells to consider.
    # The result is a vector of cell indices
    # directly applicable to the melt_cur vector
    # (i.e. indices starting at 1).
    surf_type_prev <- vec_surf_type[cells_prev]
    cells_ice      <- which(surf_type_prev == 0)
    cells_firn     <- which(surf_type_prev == 1)
    cells_snow     <- which(surf_type_prev == 2)
    cells_debris   <- which(surf_type_prev == 5)
    
    
    # Compute melt amounts.
    melt_cur[cells_ice]    <- (year_cur_params$melt_factor + 24 * year_cur_params$rad_fact_ice * grid_ice_albedo_fact_cur_values[cells_ice] / 1000. * radiation_cur[cells_ice]) * temp_cur[cells_ice] # We use offset_prev in the temperature vector because it has one timestep less than the modeled grids (which also have the initial conditions as first timestep).
    melt_cur[cells_firn]   <- (year_cur_params$melt_factor + 24 * year_cur_params$rad_fact_firn / 1000. * radiation_cur[cells_firn]) * temp_cur[cells_firn]
    melt_cur[cells_snow]   <- (year_cur_params$melt_factor + 24 * year_cur_params$rad_fact_snow / 1000. * radiation_cur[cells_snow]) * temp_cur[cells_snow]
    melt_cur[cells_debris] <- run_params$debris_red_fac * (year_cur_params$melt_factor + 24  * year_cur_params$rad_fact_ice / 1000. * radiation_cur[cells_debris]) * temp_cur[cells_debris]
    
    melt_cur[is.na(melt_cur)] <- 0.0 # Don't melt rock, but never go into the NAs (we care about the SWE over rock, for avalanches!)
    melt_cur <- pmax(0.0, melt_cur)  # Clamp to positive values: negative PDDs do not add mass.
    
    # We check which cells have had their snow cover depleted
    # at the current time step, to change their surface type.
    # We ignore the "mixed" melting regime arising from a day
    # where snow cover is depleted (radiation factor should in
    # principle be partly snow, partly ice or firn or debris).
    ids_swe_depleted <- which(melt_cur[cells_snow] >= vec_snow_swe[cells_prev][cells_snow])
    vec_snow_swe[cells_cur] <- pmax(0, vec_snow_swe[cells_prev] - melt_cur)
    vec_surf_type[cells_cur] <- vec_surf_type[cells_prev]
    vec_surf_type[cells_cur][cells_snow][ids_swe_depleted] <- surftype_init_values[cells_snow][ids_swe_depleted]
    
    
    #### .  ACCUMULATION and MASS BALANCE ####
    # Add accumulation and update cumulative mass balance.
    vec_snow_swe[cells_cur] <- vec_snow_swe[cells_cur] + accumulation_cur
    vec_massbal_cumul[cells_cur] <- vec_massbal_cumul[cells_prev] - melt_cur + accumulation_cur
    vec_surf_type[cells_cur][which(accumulation_cur > 0.0)] <- 2 # Mark surface as snow after snowfall.
    gl_massbal_cumul[day_id + 1] <- mean(vec_massbal_cumul[offset_cur + glacier_cell_ids])
    gl_melt_daily[day_id + 1] <- mean(melt_cur[glacier_cell_ids])
    gl_accum_daily[day_id + 1] <- gl_accum_daily[day_id + 1] + mean(accumulation_cur[glacier_cell_ids]) # We use the sum because we may already have a non-zero value here in case there has been an avalanche.

  }
  
  mb_model_output <- list(vec_swe_all       = vec_snow_swe,
                          vec_surftype_all  = vec_surf_type,
                          vec_massbal_cumul = vec_massbal_cumul,
                          gl_massbal_cumul  = gl_massbal_cumul,
                          gl_melt_cumul     = cumsum(gl_melt_daily),
                          gl_accum_cumul    = cumsum(gl_accum_daily))
  
  # t2 <- Sys.time()
  # print(t2-t1)
  
  return(mb_model_output)
  
}
