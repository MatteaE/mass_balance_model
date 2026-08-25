###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the selection of the indices of the mass balance points      #
#                 which will be considered for the given year YYYY.                               #
#                 A measurement is included in the set of the current year if the end date of
#                 its observation period is between run_params$stake_end_earliest and
#                 run_params$stake_end_latest.
#                 
################################################################################################### 

func_select_year_mb_measurements <- function(data_massbal,
                                             year,
                                             run_params) {
  
  # Convert the given acceptable time bounds from MM/DD to an actual date,
  # which can be either YYYY or YYYY-1 depending on the month and the location
  # (Northern or Southern).
  end_earliest_month <- as.integer(unlist(strsplit(run_params$stake_end_earliest, "/", fixed = TRUE))[1])
  end_latest_month   <- as.integer(unlist(strsplit(run_params$stake_end_earliest, "/", fixed = TRUE))[1])
  
  
  # In the Northern Hemisphere, stake_end_earliest with month
  # in [10,12] is interpreted as YYYY-1, else YYYY.
  # stake_end_latest is always interpreted as YYYY.
  
  # This gives access to all use cases:
  # a stake_end_earliest of 10/01 includes all stakes which
  # end at the very start of the hydrological year, i.e.
  # fully inclusive (any stakes ending earlier than that contribute
  # nothing to the hydrological-year mass balance, so they do not
  # belong to the current year). A stake_end_earliest of 09/30 excludes
  # all stakes which end during the current hydrological year, i.e.
  # fully exclusive. All dates in between can be used for stake_end_earliest.
  # For stake_end_latest, surveys can actually take place after the end of
  # the hydrological year (e.g. in October). So, the allowed range for
  # stake_end_latest is between (start of hydro year + 3 months) and
  # (end of hydro year + 3 months).
  if (run_params$north_south == "North") {
    if (end_earliest_month %in% 10:12) {
      end_earliest_year <- year-1
    } else {
      end_earliest_year <- year
    }
    end_latest_year <- year
    
    # In the Southern Hemisphere, stake_end_earliest with month
    # in [4,12] is interpreted as YYYY-1, else YYYY.
    # stake_end_latest with month in [7,12] is interpreted as
    # YYYY-1, else YYYY.
    # Everything is the same as in the Northern Hemisphere, but
    # earlier by 6 calendar months.
  } else {
    if (end_earliest_month %in% 4:12) {
      end_earliest_year <- year-1
    } else {
      end_earliest_year <- year
    }
    if (end_latest_month %in% 7:12) {
      end_latest_year <- year-1
    } else {
      end_latest_year <- year
    }
  }
  
  
  
  end_dates_allowed <- seq.Date(as.Date(paste0(end_earliest_year, "/", run_params$stake_end_earliest)),
                            as.Date(paste0(end_latest_year, "/", run_params$stake_end_latest)),
                            "1 day")
  
  ids_year <- which(data_massbal$end_date %in% end_dates_allowed)
  
  return(ids_year)
  
}
