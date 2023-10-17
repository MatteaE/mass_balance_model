###################################################################################################
# This program takes two files (temperature and precipitation) with sub-daily resolution and      #
# produces daily aggregates suitable for use with the glacier mass balance model DMBSim v1.0.     #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2022/08/24                                                                       #
###################################################################################################

# Algorithm:
# Get data-file extension.
# Read file (stringsAsFactors=F) with appropriate read.* function, using the user-provided rows/cols for read.xlsx, and extracting the relevant row/col if read.csv.
# Output of the previous step shall be a 2-column data.frame with (character) timestamp and (character or numeric) value.
# Try to convert timestamp to POSIXct according to given format. Check for NAs in output, if any found, write error message to report file, and stop with error.
# Check for duplicate timestamps, remove them with warning or stop with error. 
# Sort data frame by ascending timestamp (give and record warning if unsorted).
# Check timestamp differences: if multiple differences are found, give warning or error depending on checkbox.
# Check type of data column: if character or integer, try to convert to numeric. Then check for any NAs (newly generated or preexisting) - if any, depending on checkbox, either stop with error or drop them and continue with ts info on the first and warning.
# (For precipitation: after all these, also clamp negative values to 0, with a warning)
# If we get here, we have a 2-column data.frame with sorted POSIXct timestamps and all good values. Now:
# Generate NA_real_-valued daily series covering the extent of all POSIXct timestamps.
# For each day, find which timestamps belong to the day and average/accumulate them.
# Check the daily series, if a day is found with no data, stop with error or continue with warning depending on checkbox, always giving info on which is the first day with no data. If continue with warning, at the end do linear/zeros interpolation of NAs.
# Enjoy the daily series!


suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(timeSeries))
suppressPackageStartupMessages(library(tools))
suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(shinyjs))



# Read file as XLS(X). ------------------------------------------------------------------------------------------------
# Output is a 2-column data.frame with ts and value.
fileread_xls <- function(filepath,
                         startrow,
                         endrow,
                         column_ts,
                         column_data,
                         timestamp_format,
                         datatype) {
  cat("Reading Excel file...\n")
  if (endrow == 0) {
    endrow <- NA
  }
  
  
  # How many columns provide date/time specification?
  # Either everything in a single column, or two columns
  # (assumed to be date and time, in this order),
  # or three columns (assumed to be date, time and AM/PM
  # specification, in this order).
  column_ts_n <- length(column_ts)
  if (!(column_ts_n %in% 1:3)) {
    cat("** ERROR: you must specify exactly 1, 2 or 3 columns for date/time specification. Your input:", column_ts, "\n")
    return(NULL)
  }
  
  
  #### . Single column for date/time specification ------------------------------------------------
  # This is the case where the date/time specification spans exactly one
  # column (with date and possibly time and AM/PM specification).
  if (column_ts_n == 1) {
    
    dat_raw_ts <- tryCatch(as.data.frame(read_excel(filepath, sheet = 1, range = cell_limits(c(startrow, column_ts), c(endrow, column_ts)), col_names = "ts"), stringsAsFactors = FALSE),
                           error = function(e) {
                             cat("** ERROR: there was a problem reading the date/time from the Excel file. Please check it manually.\n")
                             cat("   Sometimes you can fix this error by opening the file in Excel and saving it with a different name.\n")
                             return(NULL)
                           })
    # Stop in case the previous lines returned no date/time-related data.
    if (is.null(dat_raw_ts)) {
      return(NULL)
    }
    
    # If no error was encountered, we simply save the ts
    # for later combination with the data column.
    dat_ts_out <- dat_raw_ts
    dat_ts_out$ts <- as.POSIXct(dat_ts_out$ts, tz = "UTC", format = timestamp_format)
    
    #### . Two/three columns for date/time specification --------------------------------------------
    # Case where the date/time specification spans two or three
    # columns (assumed to be date, time and possibly AM/PM, in that order).
  } else {
    
    # timestamp_format is comma-separated, in case
    # the user specifies multiple columns for date/time.
    # Here we split it.
    # timestamp_format_split[1] is the format of the date,
    # timestamp_format_split[2] is the format of the hour (optionally with AM/PM specification already in it),
    # timestamp_format_split[3] is the AM/PM specification (optional).
    timestamp_format_split <- strsplit(timestamp_format, " *,{1} *", fixed = FALSE)[[1]]
    if (length(timestamp_format_split) != column_ts_n) {
      cat("** ERROR: you specified", column_ts_n, "columns for date/time, but", length(timestamp_format_split), "format strings. The two numbers must be the same, please check.")
      return(NULL)
    }
    
    # If there is a %p (AM/PM) in the second or third specification,
    # we automatically replace all instances of %H in the second
    # specification with %I (which is 12-hours hour).
    am_pm <- grep("%p", paste(timestamp_format_split[2:length(timestamp_format_split)], collapse = " "), fixed = T)
    if (length(am_pm) > 0) {
      cat("** WARNING: you specified a time format including %H, but you also specified a %p which stands for AM/PM. Thus, I am replacing your %H (24-hour) with a %I (12-hour).\n")
      timestamp_format_split[2] <- sub("%H", # Remove AM/PM if present.
                                       "%I",
                                       x = timestamp_format_split[2])
    }
    
    # First read the date column.
    dat_raw_ts_date <- tryCatch(as.data.frame(read_excel(filepath, sheet = 1, range = cell_limits(c(startrow, column_ts[1]), c(endrow, column_ts[1])), col_names = "ts"), stringsAsFactors = FALSE),
                                error = function(e) {
                                  cat("** ERROR: there was a problem reading the first date/time column from the Excel file. Please check it manually.\n")
                                  cat("   Sometimes you can fix this error by opening the file in Excel and saving it with a different name.\n")
                                  return(NULL)
                                })
    # Stop in case the previous lines returned no date/time-related data.
    if (is.null(dat_raw_ts_date)) {
      return(NULL)
    }
    
    
    #### . . Date column produced a POSIXct class -------------------------------------------------
    # -- Case 1: we have a date column with POSIXct class. This is good!
    # In this case, we assume that the file is well-formed (i.e. that
    # day and month are not mixed up in strange MM/DD ways by Excel).
    if ("POSIXct" %in% class(dat_raw_ts_date$ts)) {
      cat("Date loading as POSIXct was successful for", datatype, "\n")
      
      # Convert POSIXct to Date.
      dat_raw_ts_date$ts <- as.Date(dat_raw_ts_date$ts)
      
      # If the dates are all before 100 CE, then we assume the user
      # incorrectly specified format "%Y" (4-digit year) instead of
      # "%y" (2-digit year).
      # We fix this with a warning.
      if (max(dat_raw_ts_date$ts, na.rm = T) < as.Date("100/01/01", format = "%Y/%m/%d")) {
        cat("** WARNING: dates appear to be very early (all before year 100 CE). If the year format has no century, you should use %y instead of %Y. I am fixing this for you this time.\n")
        dat_raw_ts_date$ts <- format(as.Date(format(dat_raw_ts_date$ts, "%y-%m-%d"), format = "%y-%m-%d"), timestamp_format_split[1])
      }
      dat_ts_date <- dat_raw_ts_date
      dat_ts_date$ts <- as.Date(dat_ts_date$ts, format = timestamp_format_split[1])
      
      
      #### . . Date column produced a character class -----------------------------------------------
      # -- Case 2: we have a date column with character class.
    } else if ("character" %in% class(dat_raw_ts_date$ts)) {
      
      # We check whether we can successfully convert to Date at least 90 % of the items.
      ts_date <- as.Date(dat_raw_ts_date$ts, format = timestamp_format_split[1])
      
      # Fix also here the case there ts_date is missing the century.
      if (max(ts_date, na.rm = T) < as.Date("100/01/01", format = "%Y/%m/%d")) {
        cat("** WARNING: all loaded dates appear to be very early (all before year 100 CE). If the year format has no century, you should use %y instead of %Y. I am fixing this for you this time.\n")
        ts_date <- as.Date(format(ts_date, "%y-%m-%d"), format = "%y-%m-%d")
      }
      
      ts_date_na_n <- length(which(is.na(ts_date)))
      
      #### . . . Character class is well behaved --------------------------------------------------
      # If yes, we assume the date format is good.
      if (ts_date_na_n < 0.1*length(ts_date)) {
        cat("Date loading was successful for", datatype, "\n")
        dat_ts_date <- data.frame(ts = ts_date)
        
        #### . . . Character class needs to be repaired ---------------------------------------------
        # If not, the problem could be that the XLS sheet has dates as M/D/Y
        # but does not support them (or expects them as D/M/Y). In that case,
        # dates compatible with both formats are loaded in the "serial" format
        # (character representation of integer day count since 1899/12/30) BUT
        # with the day and month switched, the others (day > 12) are kept in their
        # original string format (timestamp_format_split[1]).
        # To detect this, we check whether there are any integers in the series,
        # if yes we try to convert them to a date, we fix that date by swapping
        # day and month, and we check whether these new dates all have day < 12,
        # don't match any of the already-available dates and are actually filling
        # in gaps in the series (i.e. we are not randomly adding spurious dates,
        # but we are close to what is already there. In that case we know
        # we are repairing the data correctly, else we give up.
      } else {
        
        cat("** WARNING: date column specified for", datatype, "has some problems, probably days and months are mixed up in Excel with the wrong specification. I am attempting to repair it.\n")
        
        ts_date_int <- as.integer(dat_raw_ts_date$ts)
        ts_date_int_valid_ids <- which(!is.na(ts_date_int))
        
        # Do we have a lot of these integer (serial) dates? If not, give up.
        if (length(ts_date_int_valid_ids) < 0.1 * length(ts_date)) {
          cat("** ERROR: failed to find enough integer serial dates to repair.")
          return(NULL)
        }
        
        ts_int_dates <- as.Date(ts_date_int[ts_date_int_valid_ids], origin = "1899/12/30")
        ts_int_days <- as.integer(format(ts_int_dates, "%d"))
        
        # Do we have only days within 1-12 in the integer dates? If not, give up (Excel would have kept the string format: timestamp_format_split[1]).
        if (any(ts_int_days > 12)) {
          cat("** ERROR: found integer serial dates, but they include days greater than 12.")
          return(NULL)
        }
        
        # Here we swap the day and month, getting a character vector
        # which is compatible with the one of the dates with day > 13.
        ts_int_dates_fixed <- as.Date(format(ts_int_dates, "%Y-%d-%m"))
        ts_int_dates_fixed_str <- format(ts_int_dates_fixed, timestamp_format_split[1])
        
        if (any(ts_int_dates_fixed %in% ts_date)) {
          cat("** ERROR: found integer serial dates, but they match some dates already reported in text format. Please check the cell types of the date column in the Excel sheet.")
          return(NULL)
        }
        
        if ((max(ts_int_dates_fixed, na.rm = T) < min(ts_date, na.rm = T)) ||
            (min(ts_int_dates_fixed, na.rm = T) > max(ts_date, na.rm = T))) {
          cat("** ERROR: found some valid integer serial dates, but their period does not overlap with the period of the valid dates.")
          return(NULL)
        }
        
        cat("** Successfully repaired date format.\n")
        
        # If we have arrived here, we are confident that
        # we have succesfully repaired the date column.
        # We produce the final series of dates.
        dat_ts_date <- data.frame(ts = format(ts_date, format = timestamp_format_split[1]))
        dat_ts_date$ts[ts_date_int_valid_ids] <- ts_int_dates_fixed_str
        dat_ts_date$ts <- as.Date(dat_ts_date$ts, format = timestamp_format_split[1])
        
      }
      
      
      #### . . Date column produced something other than POSIXct or character, just give up ---------
    } else {
      cat("** ERROR: I don't understand the date column for", datatype, "- please check.")
      return(NULL)
    }
    
    
    
    # If we get here, we have a dat_ts_date with proper Date class.
    # Now we process the other part (time).
    # Algorithm:
    # Read time column
    # Read third column if present
    # Paste Y-M-D date, read time and third column and try to interpret with given format.
    #   If ok: we have our timestamps!
    #   If fail: try to interpret second column as fractional day, that is:
    #     Paste third column if present
    #     Check if AM/PM is present at the end of most items (can be either from the pasting or from the untouhed second column)
    #       If yes, save AM/PM to a vector, convert to numeric, fix according to AM/PM: all AM must be in [0.0, 0.5); all PM must be in [0.5,1.0)
    #       Then in all cases convert fractional to HH:MM:SS: function(x) {c(x %/% 3600, (x%%3600) %/% 60, x%%60)}
    
    
    #### . . Read the time column -----------------------------------------------------------------
    dat_raw_ts_time <- tryCatch(as.data.frame(read_excel(filepath, sheet = 1, range = cell_limits(c(startrow, column_ts[2]), c(endrow, column_ts[2])), col_names = "ts"), stringsAsFactors = FALSE),
                                error = function(e) {
                                  cat("** ERROR: there was a problem reading the time column from the Excel file. Please check it manually.\n")
                                  return(NULL)
                                })
    # Stop in case the previous lines returned no time-related data.
    if (is.null(dat_raw_ts_time)) {
      return(NULL)
    }
    
    # We only accept character or POSIXct class.
    if (!(any(c("character", "POSIXct") %in% class(dat_raw_ts_time$ts)))) {
      cat("** ERROR: I don't understand the time column", paste0("(", column_ts[2], ")"), "for", datatype, "- please check.")
      return(NULL)
    }
    
    # If we have a POSIXct, we immediately convert it to character
    # and continue as if it was originally a character.
    if ("POSIXct" %in% class(dat_raw_ts_time$ts)) {
      dat_raw_ts_time$ts <- format(dat_raw_ts_time$ts, timestamp_format_split[2])
    }
    
    # If we have a third column, load it.
    if (column_ts_n == 3) {
      dat_raw_ts_time2 <- tryCatch(as.data.frame(read_excel(filepath, sheet = 1, range = cell_limits(c(startrow, column_ts[3]), c(endrow, column_ts[3])), col_names = "ts"), stringsAsFactors = FALSE),
                                   error = function(e) {
                                     cat("** ERROR: there was a problem reading the time column from the Excel file. Please check it manually.\n")
                                     return(NULL)
                                   })
      # Stop in case the previous lines returned no time-related data.
      if (is.null(dat_raw_ts_time2)) {
        return(NULL)
      }
      
      dat_raw_ts_time_pasted <- data.frame(ts = paste(dat_raw_ts_time$ts, dat_raw_ts_time2$ts, sep = " "))
      
      # Just one time column, but keep names consistent for subsequent processing.
    } else {
      dat_raw_ts_time_pasted <- dat_raw_ts_time
    }
    dat_raw_datetime_pasted <- data.frame(ts = paste(dat_ts_date$ts, dat_raw_ts_time_pasted$ts, sep = " "))
    
    # Try to convert to POSIXct according to time specification
    # (we use the proper date that we have already repaired above).
    ts_datetime <- as.POSIXct(dat_raw_datetime_pasted$ts,
                              tz = "UTC",
                              format = paste("%Y-%m-%d", paste(timestamp_format_split[2:length(timestamp_format_split)], sep = " ", collapse = " "), sep = " ", collapse = " "))
    
    # Deal with Excel's fractional days.
    if (length(which(is.na(ts_datetime))) > 0.1 * length(ts_datetime)) {
      cat("** WARNING: time column for", datatype, "has some issues with the format. I will try to repair it as fractional days.\n")
      
      # This function converts from fractional day to c(H,M,S).
      # Vectorized (always returns a 3-column matrix).
      frac2hms <- function(x) {cbind(x %/% 3600, (x%%3600) %/% 60, x%%60)}
      
        
      am_pm_n <- length(grep(" am *$| pm *$| AM *$| PM *$",
                             x = dat_raw_ts_time_pasted$ts))
      
      # We have some AM/PM specification.
      if (am_pm_n > 0) {
        
        # If it is incomplete then give up.
        if (am_pm_n < 0.9 * nrow(dat_raw_ts_time_pasted)) {
          cat("** ERROR: there is some AM/PM specification in the time column of", datatype, "but it is not complete. Please check.")
          return(NULL)
        }
        
        # Else, correct fractional days with AM/PM.
        # NOTE: we don't touch ids which don't have AM/PM.
        fractional_days <- as.numeric(sub(" am *$| pm *$| AM *$| PM *$", # Remove AM/PM if present.
                                          "",
                                          x = dat_raw_ts_time_pasted$ts))
        am_ids <- grep(" am *$| AM *$",
                       x = dat_raw_ts_time_pasted$ts)
        am_ids_need_fix <- intersect(am_ids, which(fractional_days >= 0.5))
        
        pm_ids <- grep(" pm *$| PM *$",
                       x = dat_raw_ts_time_pasted$ts)
        pm_ids_need_fix <- intersect(pm_ids, which(fractional_days < 0.5))
        
        # Fix according to AM/PM.
        fractional_days[am_ids_need_fix] <- fractional_days[am_ids_need_fix] - 0.5
        fractional_days[pm_ids_need_fix] <- fractional_days[pm_ids_need_fix] + 0.5
        
        
        # No AM/PM specification, assume fractional time already covers [0-1).
      } else {
        fractional_days <- as.numeric(dat_raw_ts_time_pasted$ts)
      }
      
      time_hms <- frac2hms(86400*fractional_days)
      dat_datetime_out <- data.frame(ts = paste(dat_ts_date$ts, time_hms[,1], time_hms[,2], time_hms[,3], sep = " "))
      ts_datetime <- as.POSIXct(dat_datetime_out$ts, tz = "UTC", format = "%Y-%m-%d %H %M %S")
      cat("** Succesfully repaired time format.\n")
        
      # Else: not many NAs when converting time column using given format.
    } else {
      cat("** Date-time format is correct.\n")
    }
      
    dat_ts_out <- data.frame(ts = ts_datetime)
    
    
    # End else (if there are two or three columns for date/time). 
  }
  
  
  
  
  # If we get here, we have a decent series of timestamps.
  #### Now load the actual data values ------------------------------------------------------------
  dat_raw_val<- tryCatch(as.data.frame(read_excel(filepath, sheet = 1, range = cell_limits(c(startrow, column_data), c(endrow, column_data)), col_names = "value"), stringsAsFactors = FALSE),
                         error = function(e) {
                           cat("** ERROR: there was a problem reading the data from the Excel file. Please check it manually.\n")
                           cat("   Sometimes you can fix this error by opening the file in Excel and saving it with a different name.\n")
                           return(NULL)
                         })
  if (is.null(dat_raw_val)) {
    return(NULL)
  }
  # If endrow is NA, read_excel will stop reading at the last non-empty cell.
  # If the file uses empty cells for e.g. zero precipitation, then dat_raw_val
  # will only have entries up to the last nonzero precipitation, i.e. it will
  # be shorter than the timestamps series. So we have to add values at the end
  # to match the length.
  if (nrow(dat_raw_val) < nrow(dat_ts_out)) {
    dat_raw_val <- rbind(dat_raw_val, data.frame(value = rep(NA_real_, nrow(dat_ts_out) - nrow(dat_raw_val))))
  }
  # Conversely from the previous comment, if the values series is longer
  # than the timestamps series, then we have a problem with the file
  # and we request manual intervention.
  if (nrow(dat_raw_val) > nrow(dat_ts_out)) {
    cat("** ERROR: there are more values than timestamps in the", datatype, "series!\n")
    cat("   Please check the file manually and also check the rows and columns you selected.\n")
    return(NULL)
  }
  dat_out <- cbind(dat_ts_out, dat_raw_val)
  names(dat_out) <- c("ts", "value")
  return(dat_out)
  
  # End of fileread_xls() definition.
}


# Read file as CSV. ---------------------------------------------------------------------------------------------------
# Output is a 2-column data.frame with ts and value.
# NOTE: we support only single-column datetime specification (unlike with Excel files).
fileread_csv <- function(filepath,
                         startrow,
                         endrow,
                         column_ts,
                         column_data,
                         timestamp_format) {
  cat("Reading CSV file...\n")
  dat_raw <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE, skip = startrow - 1)
  if (nrow(dat_raw) == 0) {
    cat("** ERROR: there was a problem reading the input file as CSV.\n")
    cat("   Please check the file manually.\n")
    return(NULL)
  }
  
  # endrow = 0 means take everything.
  if (endrow == 0) {
    endrow <- nrow(dat_raw)
  }
  dat_out <- dat_raw[1:endrow, c(column_ts, column_data)]
  names(dat_out) <- c("ts", "value")
  
  # Convert to POSIXct already here, since we also have to do
  # that in the XLSX case to try to repair Excel date problems.
  dat_out$ts <- as.POSIXct(dat_out$ts, tz = "UTC", format = timestamp_format)
  return(dat_out)
}


# Determine file type and read accordingly. ---------------------------------------------------------------------------
# Output is a 2-column data.frame with ts and value.
# datatype is "temperature" or "precipitation".
fileread <- function(filepath,
                     startrow,
                     endrow,
                     column_ts_raw,
                     column_data,
                     timestamp_format,
                     datatype) {
  
  cat("Filepath of", datatype, "is", filepath, "\n")
  
  if ((startrow < 1) || (endrow < 0) || ((endrow > 0) && (startrow > endrow))) {
    cat("** ERROR: range of selected rows (for", datatype, "is invalid. Please correct.\n")
    cat("   startrow =", startrow, "-- endrow =", endrow, "\n")
    return(NULL)
  }
  
  # We convert column_ts_raw from a (possibly comma-separated) character vector
  # into an integer vector (with length > 1 only if the timestamp specification
  # spans multiple columns in the file, e.g. a date column and a time column).
  column_ts <- as.integer(strsplit(column_ts_raw, " *,{1} *", fixed = FALSE)[[1]])
  if ((length(column_ts) == 0) || any(is.na(column_ts))) {
    cat("** ERROR: problem with the selection of date/time column(s) for", dataype, "- please correct.\n")
    cat("   Your input was:", column_ts_raw, "\n")
    return(NULL)
  }
  
  if ((any(column_ts < 1)) || (column_data < 1) || (any(column_ts == column_data))) {
    cat("** ERROR: selected columns (for", paste0(datatype, ")"), "are invalid. Please correct.\n")
    cat("   column_ts =", column_ts, "-- column_data =", column_data, "\n")
    return(NULL)
  }
  
  cat("Checking file type...\n")
  extensions_csv <- c("csv", "dat", "CSV", "DAT")
  extensions_xls <- c("xls", "xlsx", "XLS", "XLSX")
  fileext <- file_ext(filepath)
  if (fileext %in% extensions_xls) {
    df <- fileread_xls(filepath, startrow, endrow, column_ts, column_data, timestamp_format, datatype)
  } else if (fileext %in% extensions_csv) {
    df <- fileread_csv(filepath, startrow, endrow, column_ts, column_data, timestamp_format)
  } else {
    cat("** ERROR:", datatype, "file type not recognized! It must be either CSV or XLS. Please check manually.\n")
    return(NULL)
  }
  
  return(df) # df is NULL in case we have had an error reading the file. We handle this in process_file().
}


# Process timestamps series, checking for common issues. --------------------------------------------------------------
process_ts <- function(df,
                       timestamp_format,
                       timestamp_duplicate_stop_logi,
                       timestamp_diff_irregular_stop_logi,
                       timestamp_invalid_stop_logi,
                       datatype) {
  
  
  cat("Processing timestamps...\n")
  df_proc <- df
  ts_na <- which(is.na(df_proc$ts))
  ts_na_n <- length(ts_na)
  
  # If a lot of timestamps (> 10%) are NA, it is likely that the format string is wrong.
  # Else, it is likely a problem with the file.
  if (ts_na_n > 0) {
    
    # Any valid timestamps?
    if (ts_na_n == nrow(df_proc)) {
      cat("** ERROR: no valid timestamps in the", datatype, "series. Please check.\n")
      return(NULL)
    }
    
    if (timestamp_invalid_stop_logi) {
      cat("** ERROR:", ts_na_n, "problematic timestamp(s) encountered in the", datatype, "series.\n")
      if (ts_na_n > 0.1 * nrow(df_proc)) {
        cat("   Please check the timestamp format that you entered, it is probably wrong.\n")
        cat("   The first problematic timestamp is number", ts_na[1], "and it is", df$ts[ts_na[1]], "\n")
      } else {
        cat("   The timestamp format that you entered appears correct, please check the input file.\n")
        cat("   The first problematic timestamp is number", ts_na[1], "and it is", paste0("\"", df$ts[ts_na[1]], "\""), "\n")
      }
      return(NULL)
      
      # timestamp_invalid_stop_logi = FALSE --> Drop problematic timestamps with a warning.
    } else {
      cat("** WARNING:", ts_na_n, "problematic timestamp(s) encountered in the", datatype, "series. But \"stop on invalid timestamps\" is not checked, so I will drop those timestamps and continue. Make sure that this is what you want!\n")
      df_proc <- df_proc[-ts_na,]
    }
  }
  cat("Timestamp format is now good.\n")
  
  cat("Checking for duplicate timestamps...\n")
  ts_dup <- which(duplicated(df_proc$ts))
  ts_dup_n <- length(ts_dup)
  if (ts_dup_n > 0) {
    if (timestamp_duplicate_stop_logi) {
      cat("** ERROR: found", ts_dup_n, "duplicate timestamp(s)! Please correct this.\n")
      cat("   The first duplicate is number", ts_dup[1], "and it is", format(df_proc$ts[ts_dup[1]], "%F %T."))
      return(NULL)
    } else {
      cat("** WARNING: found", ts_dup_n, "duplicate timestamp(s)!\n")
      cat("   I will delete them and proceed, but make sure that this is what you want.\n")
      cat("   The first duplicate is number", ts_dup[1], "and it is", format(df_proc$ts[ts_dup[1]], "%F %T."))
      df_proc <- df_proc[-ts_dup,]
    }
  }
  
  cat("Checking timestamp order...\n")
  df_sort <- df_proc
  ts_sorting <- sort.int(df_proc$ts, decreasing = FALSE, index.return = T)$ix
  if (any(ts_sorting != (1:length(ts_sorting)))) {
    cat("** WARNING: timestamps are not sorted in ascending order. I am sorting them now.\n")
    df_sort <- df_proc[ts_sorting,]
  }
  
  cat("Checking timestamp intervals...\n")
  ts_diff <- diff(as.integer(df_sort$ts)) # Differences in seconds.
  ts_diff_u <- unique(ts_diff)
  ts_diff_u_n <- length(ts_diff_u)
  if (ts_diff_u_n > 1) {
    ts_diff_count <- integer(ts_diff_u_n)
    for (d_id in 1:length(ts_diff_u)) {
      ts_diff_count[d_id] <- length(which(ts_diff == ts_diff_u[d_id]))
    }
    if (timestamp_diff_irregular_stop_logi) {
      cat("** ERROR: found multiple different time intervals in the series!\n")
      cat("   The intervals (in hours) and the number of times they appear:", paste0(ts_diff_u/3600, "h (", ts_diff_count, "x)", collapse = ", "), "\n")
      cat("   Please correct this manually.\n")
      return(NULL)
    } else {
      cat("** WARNING: found multiple different time intervals in the series!\n")
      cat("   I will proceed, but make sure that this is what you want (especially at the start/end of the series).\n")
      cat("   The intervals (in hours) and the number of times they appear:", paste0(ts_diff_u/3600, "h (", ts_diff_count, "x)", collapse = ", "), "\n")
    }
  }
  
  return(df_sort)
}


# Process values in the series, checking for common issues. -----------------------------------------------------------
process_values <- function(df,
                           timestamp_format,
                           value_na_stop_logi,
                           datatype) {
  
  cat("Checking values in the", datatype, "series...\n")
  df$value <- suppressWarnings(as.numeric(df$value))
  values_na <- which(is.na(df$value))
  values_na_n <- length(values_na)
  
  df_pruned <- df
  if (values_na_n > 0) {
    if (value_na_stop_logi) {
      cat("** ERROR: found", values_na_n, "non-numeric value(s) in the", datatype, "series!\n")
      cat("   The first non-numeric value is at timestamp", format(df$ts[values_na[1]], "%F %T."), "\n")
      cat("   Please correct this manually.\n")
      return(NULL)
    } else {
      cat("** WARNING: found", values_na_n, "non-numeric value(s) in the", datatype, "series!\n")
      if (datatype == "temperature") {
        cat("   I will discard them and proceed, but make sure that this is what you want.\n")
        cat("   The first non-numeric value is at timestamp", format(df$ts[values_na[1]], "%F %T."), "\n")
        df_pruned <- df[-values_na,]
      } else {
        cat("   I will replace them with zeroes and proceed, but make sure that this is what you want.\n")
        cat("   The first non-numeric value is at timestamp", format(df$ts[values_na[1]], "%F %T."), "\n")
        df_pruned$value[values_na] <- 0.0
      }
      
    }
  }
  
  
  if (datatype == "precipitation") {
    val_negative <- which(df_pruned$value < 0)
    val_negative_n <- length(val_negative)
    if (val_negative_n > 0) {
      cat("** WARNING: found", val_negative_n, "negative value(s) in the", datatype, "series! This is not possible.\n")
      cat("   I will replace them with zeroes, but you should check the series carefully.\n")
      cat("   The first negative value is at timestamp", format(df$ts[val_negative[1]], "%F %T."), "\n")
      df_pruned$value <- pmax(df_pruned$value, 0.0)
    }
  }
  
  return(df_pruned)
}


# Aggregate to daily resolution. --------------------------------------------------------------------------------------
# Temperature is averaged, precipitation is accumulated.
generate_daily_series <- function(df,
                                  value_daily_missing_stop_logi,
                                  datatype) {
  
  cat("Generating daily", datatype, "series...\n")
  day1 <- as.Date(df$ts[1])
  day2 <- as.Date(df$ts[nrow(df)])
  
  df_daily <- data.frame(date = seq.Date(day1, day2, by = "1 day"),
                         value = NA_real_)
  days_n <- nrow(df_daily)
  
  df_days <- as.Date(df$ts)
  for (day_id in 1:days_n) {
    dat_ids <- which(df_days == df_daily$date[day_id])
    if (length(dat_ids) > 0) {
      if (datatype == "temperature") {
        df_daily$value[day_id] <- mean(df$value[dat_ids])
      } else {
        df_daily$value[day_id] <- sum(df$value[dat_ids])
      }
    }
  }
  
  cat("Checking for missing values in the daily", datatype, "series...")
  daily_na <- which(is.na(df_daily$value))
  daily_na_n <- length(daily_na)
  if (daily_na_n > 0) {
    cat("\n")
    if (value_daily_missing_stop_logi) {
      cat("** ERROR: found", daily_na_n, "missing value(s) in the daily", datatype, "series.\n")
      cat("   Please check the input series manually. The first missing value is for day", format(df_daily$date[daily_na[1]], "%Y-%m-%d."), "\n")
      return(NULL)
    } else {
      cat("** WARNING: found", daily_na_n, "missing value(s) in the daily", datatype, "series.\n")
      if (datatype == "temperature") {
        cat("   I will proceed and linearly interpolate them, but make sure that this is what you want.\n")
        df_daily$value <- as.numeric(interpNA(timeSeries(df_daily$value), method = "linear"))
      } else {
        cat("   I will proceed and replace them with zeroes, but make sure that this is what you want.\n")
        df_daily$value[daily_na] <- 0.0
      }
      cat("   The first missing value is for day", format(df_daily$date[daily_na[1]], "%Y-%m-%d."), "\n")
      
    }
  } else {
    cat(" None found.\n")
  }
  
  if (nrow(df_daily) < 2) {
    cat("** ERROR: the generated daily series of", datatype, "appears to be empty.\n")
    cat("   Please check the input file manually!")
    return(NULL)
  }
  
  return(df_daily) # Returned value is NULL instead if there was an error. We handle this in func_do_processing().
}


# Wrapper function to call the entire processing of one file. ---------------------------------------------------------
process_file <- function(process_params,
                         stop_flags_logi,
                         datatype) {
  
  cat("\n\n=== Processing", datatype, "===\n")
  
  filepath                            <- process_params[[1]]
  startrow                            <- process_params[[2]]
  endrow                              <- process_params[[3]]
  column_ts_raw                       <- process_params[[4]] # This is a string, it can have multiple columns if the file splits date, time and AM/PM specification.
  column_data                         <- process_params[[5]]
  timestamp_format                    <- process_params[[6]]
  
  timestamp_duplicate_stop_logi       <- stop_flags_logi[1]
  timestamp_diff_irregular_stop_logi  <- stop_flags_logi[2]
  timestamp_invalid_stop_logi         <- stop_flags_logi[3]
  value_na_stop_logi                  <- stop_flags_logi[4]
  value_daily_missing_stop_logi       <- stop_flags_logi[5]
  
  df1 <- fileread(filepath,
                  startrow,
                  endrow,
                  column_ts_raw,
                  column_data,
                  timestamp_format,
                  datatype)
  if (is.null(df1)) {
    return(NULL)
  }
  
  df2 <- process_ts(df1,
                    timestamp_format,
                    timestamp_duplicate_stop_logi,
                    timestamp_diff_irregular_stop_logi,
                    timestamp_invalid_stop_logi,
                    datatype)
  if (is.null(df2)) {
    return(NULL)
  }
  
  df3 <- process_values(df2,
                        timestamp_format,
                        value_na_stop_logi,
                        datatype)
  if (is.null(df3)) {
    return(NULL)
  }
  
  df_daily <- generate_daily_series(df3,
                                    value_daily_missing_stop_logi,
                                    datatype)
  
  return(df_daily)
}


# Combine daily series of temperature and precipitation, producing the output data.frame. -----------------------------
combine_T_P <- function(df_daily_T,
                        df_daily_P) {
  
  d1 <- max(df_daily_T$date[1], df_daily_P$date[1])
  d2 <- min(df_daily_T$date[nrow(df_daily_T)], df_daily_P$date[nrow(df_daily_P)])
  if (as.integer(d2-d1) < 365) {
    if (as.integer(d2-d1) < 1) {
      cat("** ERROR: the temperature and precipitation series do not overlap in time. Please check the input manually!\n")
      return(NULL)
    }
    cat("** WARNING: the temperature and precipitation series overlap in time for less than one year.\n")
    cat("   I will produce the output file, but it is too short to run the mass balance model! Please check the input manually.\n")
  }
  idT1 <- which(df_daily_T$date == d1)
  idT2 <- which(df_daily_T$date == d2)
  idP1 <- which(df_daily_P$date == d1)
  idP2 <- which(df_daily_P$date == d2)
  df_combi <- cbind(df_daily_T[idT1:idT2,], df_daily_P[idP1:idP2,2])
  df_out <- data.frame(year = as.integer(format(df_combi$date, "%Y")),
                       doy = as.integer(format(df_combi$date, "%j")),
                       hour = 12,
                       t2m_mean = df_combi[,2],
                       precip = df_combi[,3])
  return(df_out)  
}


# Wrapper to call the entire processing when the user presses the RUN! button. ----------------------------------------
func_do_processing <- function(process_params_T,
                               process_params_P,
                               stop_flags_logi,
                               outdir) {
  
  #sink(file = file.path(paste0("meteo_aggregate_run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")), split = TRUE)
  cat("Meteo aggregate called at", as.character(Sys.time()), paste0("(", Sys.timezone(), ")"), "\n")
  cat("System info:\n")
  print(R.version)
  cat("\n")
  
  df_daily_T <- process_file(process_params_T,
                             stop_flags_logi,
                             datatype = "temperature")
  if (is.null(df_daily_T)) {
    return(NULL)
  }
  df_daily_P <- process_file(process_params_P,
                             stop_flags_logi,
                             datatype = "precipitation")
  if (is.null(df_daily_P)) {
    return(NULL)
  }
  
  df_daily_out <- combine_T_P(df_daily_T, df_daily_P)
  if (is.null(df_daily_out)) {
    return(NULL)
  }
  df_daily_out$t2m_mean <- round(df_daily_out$t2m_mean, 3)
  write.table(df_daily_out, file.path(outdir, "meteo_daily.dat"), quote = F, row.names = F, col.names = T)
  
  #### Finish messages ####
  cat("\nPROGRAM FINISHED SUCCESSFULLY!\n")
  cat("Your output daily file is located here:\n")
  cat(normalizePath(file.path(outdir, "meteo_daily.dat")), "\n")
  cat("You can now close the app and RStudio.")
  
  #sink()
  return(0)
}




#### Definition of shiny app to use the above function ####
# Define UI for app ----
ui <- fluidPage(useShinyjs(),
                
                # . App title and description ----
                titlePanel("Aggregate meteo data for DMBSim v1.0"),
                
                checkboxInput("checkbox_show_help_text", "Show help text", FALSE),
                
                # . . Help text which can be shown by ticking a checkbox.
                conditionalPanel(condition = "input.checkbox_show_help_text == 1",
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("This program aggregates meteorological series of temperature and precipitation: from sub-daily frequency (also irregular) to daily. The output file can be directly used as input for the glacier mass balance model DMBSim v1.0.")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("As"), strong(" INPUT DATA "), em("please provide:")),
                                 tags$div(tags$ul(
                                   tags$li(em("a ", strong("temperature file"), " in CSV or XLS(X) format")),
                                   tags$li(em("a ", strong("precipitation file"), " in CSV or XLS(X) format (it can be the same as the temperature file).")),
                                   style = "margin-top: 0px; margin-bottom: 5px; text-align: justify;")),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("Input files must have at least ", strong("two columns:"), " one with ", strong("date/time"), " and one with ", strong("value.")),
                                    em("Date/time can also be in ", strong("two or three columns instead of one:"), " in this case, the first column is date, the second column is time, and the third column (optional, not mandatory) is the AM/PM specification.")),
                                 p(style="margin-bottom: 15px;"),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("As"), strong(" INPUT PARAMETERS "), em("please provide:")),
                                 tags$div(tags$ul(
                                   tags$li(em("the numbers of the ", strong("first and last data rows."), "You can use 0 (zero) as last data row to automatically detect the end of the file")),
                                   tags$li(em("the numbers of the ", strong("columns with date/time and data values."), "If date and time are in different columns, you can separate them with a comma (example: 1,2)")),
                                   tags$li(em("the ", strong("exact format of the date/time column."), "Example: %Y-%m-%d %H:%M:%S. If the date and time are split on two or three columns, you can separate them with a comma just like the date/time column numbers, for example: %Y/%m/%d,%H:%M:%S,%p. Explanation of the symbols: %Y = year (YYYY), %y = year (YY), %m = month, %d = day, %H = hour (00-24), %I = hour (00-12), %M = minute, %S = second, %p = AM/PM.")),
                                   style = "margin-top: 0px; margin-bottom: 5px; text-align: justify;")),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("With the ", strong("five checkboxes (\"Stop on...\")"), "you can decide what happens in case of problems. If the checkbox is ticked, the app will stop when it finds a problem, and you can check it manually. If it is not ticked, the app will try to continue with a warning message (in this case, the output might be wrong or unexpected).")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("To aggregate values, temperature is ", strong("averaged,"), " precipitation is ", strong("accumulated.")))),
                
                # . UI layout below the help text ----
                p(),
                
                # .. Input: choose temperature file and enter its row/column specs ----
                shinyFilesButton("choose_file_T", strong("Choose temperature file"), "Choose temperature file", multiple = FALSE, style = "width: 86.6%;"),
                p(),
                div(style="display:inline-block",textInput("choose_firstrow_T", label = NULL,placeholder = "Temperature file: first data row")),
                div(style="display:inline-block",textInput("choose_lastrow_T", label = NULL, placeholder = "Temperature file: last data row")),
                div(),
                div(style="display:inline-block",textInput("choose_column_ts_T", label = NULL, placeholder = "Temperature file: date/time column(s)")),
                div(style="display:inline-block",textInput("choose_column_data_T", label = NULL, placeholder = "Temperature file: values column")),
                textInput("choose_ts_format_T", label = NULL, placeholder = "Temperature file: date/time format (example: %Y-%m-%d %H:%M:%S)", width = "86.6%"),
                
                # .. Input: choose precipitation file and enter its row/column specs ----
                shinyFilesButton("choose_file_P", strong("Choose precipitation file"), "Choose precipitation file", multiple = FALSE, style = "width: 86.6%;"),
                p(),
                div(style="display:inline-block",textInput("choose_firstrow_P", label = NULL,placeholder = "Precipitation file: first data row")),
                div(style="display:inline-block",textInput("choose_lastrow_P", label = NULL, placeholder = "Precipitation file: last data row")),
                div(),
                div(style="display:inline-block",textInput("choose_column_ts_P", label = NULL, placeholder = "Precipitation file: date/time column(s)")),
                div(style="display:inline-block",textInput("choose_column_data_P", label = NULL, placeholder = "Precipitation file: values column")),
                textInput("choose_ts_format_P", label = NULL, placeholder = "Precipitation file: date/time format (example: %Y-%m-%d %H:%M:%S)", width = "86.6%"),
                
                # .. Input: checkboxes to decide error vs warning on various unexpected inputs ----
                div(style="display:inline-block;margin-right:1%;margin-bottom:-2%;width:18%",checkboxInput("checkbox_ts_duplicate", strong("Stop on duplicate timestamps"), TRUE, width = "100%")),
                div(style="display:inline-block;margin-right:1%;margin-bottom:-2%;width:18%",checkboxInput("checkbox_ts_irregular", strong("Stop on irregular timestamps"), FALSE, width = "100%")),
                div(style="display:inline-block;margin-right:1%;margin-bottom:-2%;width:18%",checkboxInput("checkbox_ts_invalid", strong("Stop on invalid timestamps"), TRUE, width = "100%")),
                div(style="display:inline-block;margin-right:1%;margin-bottom:-2%;width:18%",checkboxInput("checkbox_value_na", strong("Stop on invalid data values"), FALSE, width = "100%")),
                div(style="display:inline-block;margin-bottom:-2%;width:18%;",checkboxInput("checkbox_daily_missing", strong("Stop on missing daily values"), TRUE, width = "100%")),
                p(),
                
                # .. Text fields: show the full path of the chosen temperature and precipitation files ----
                htmlOutput("temp_chosen_string"),
                htmlOutput("temp_specs_string"),
                htmlOutput("prec_chosen_string"),
                htmlOutput("prec_specs_string"),
                p(),
                
                # .. Input: do-it button ----
                actionButton(inputId = "startprocessing",
                             label = strong("RUN!")),
                p()
                
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  volumes <- c(getVolumes()(), setNames(dirname(getwd()), basename(dirname(getwd()))), setNames(dirname(dirname(getwd())), basename(dirname(dirname(getwd())))))
  shinyFileChoose(input, "choose_file_T", roots=volumes, session=session)
  shinyFileChoose(input, "choose_file_P", roots=volumes, session=session)
  
  filepath_T       <- reactive(as.character(parseFilePaths(volumes, input$choose_file_T)$datapath))
  filepath_P       <- reactive(as.character(parseFilePaths(volumes, input$choose_file_P)$datapath))
  
  firstrow_T <- reactive(as.integer(input$choose_firstrow_T))
  lastrow_T <- reactive(as.integer(input$choose_lastrow_T))
  column_ts_T <- reactive(as.character(input$choose_column_ts_T))
  column_data_T <- reactive(as.integer(input$choose_column_data_T))
  ts_format_T <- reactive(input$choose_ts_format_T)
  
  firstrow_P <- reactive(as.integer(input$choose_firstrow_P))
  lastrow_P <- reactive(as.integer(input$choose_lastrow_P))
  column_ts_P <- reactive(as.character(input$choose_column_ts_P))
  column_data_P <- reactive(as.integer(input$choose_column_data_P))
  ts_format_P <- reactive(input$choose_ts_format_P)
  
  
  # Disable "RUN!" button if the required input is missing.
  # NOTE: isTruthy(as.integer("a")) is FALSE.
  # This, together with the definition of firstrow_T above,
  # does the job of sanitizing the input for us.
  observe({
    toggleState("startprocessing", isTruthy(filepath_T()) && isTruthy(filepath_P()) && isTruthy(firstrow_T()) && isTruthy(lastrow_T()) && isTruthy(column_ts_T()) && isTruthy(column_data_T()) && isTruthy(ts_format_T()) && isTruthy(firstrow_P()) && isTruthy(lastrow_P()) && isTruthy(column_ts_P()) && isTruthy(column_data_P()) && isTruthy(ts_format_P()))
  })
  
  # Show the user which input has been provided and which is still missing.
  output$temp_chosen_string <- renderText({
    ifelse(isTruthy(filepath_T()),
           "<font color=\"#00C000\"><b>Temperature file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Temperature file not yet selected.</b></font color>")
  })
  output$temp_specs_string <- renderText({
    ifelse(isTruthy(firstrow_T()) && isTruthy(lastrow_T()) && isTruthy(column_ts_T()) && isTruthy(column_data_T()) && isTruthy(ts_format_T()),
           "<font color=\"#00C000\"><b>Temperature file specification complete.</b></font color>",
           "<font color=\"#FF0000\"><b>Temperature file specification not yet complete.</b></font color>")
  })
  
  output$prec_chosen_string <- renderText({
    ifelse(isTruthy(filepath_P()),
           "<font color=\"#00C000\"><b>Precipitation file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Precipitation file not yet selected.</b></font color>")
  })
  output$prec_specs_string <- renderText({
    ifelse(isTruthy(firstrow_P()) && isTruthy(lastrow_P()) && isTruthy(column_ts_P()) && isTruthy(column_data_P()) && isTruthy(ts_format_P()),
           "<font color=\"#00C000\"><b>Precipitation file specification complete.</b></font color>",
           "<font color=\"#FF0000\"><b>Precipitation file specification not yet complete.</b></font color>")
  })
  
  
  # Button to start processing.
  observeEvent(input$startprocessing, {
    showModal(modalDialog(h3("Processing... See RStudio console for progress."), footer=NULL))
    
    stop_flags_logi <- c(input$checkbox_ts_duplicate, input$checkbox_ts_irregular, input$checkbox_ts_invalid, input$checkbox_value_na, input$checkbox_daily_missing)
    process_params_T <- list(filepath_T(), firstrow_T(), lastrow_T(), column_ts_T(), column_data_T(), ts_format_T())
    process_params_P <- list(filepath_P(), firstrow_P(), lastrow_P(), column_ts_P(), column_data_P(), ts_format_P())
    outdir <- dirname(rstudioapi::getSourceEditorContext()$path) # Write output file to same directory as the script.
    processing_output <- func_do_processing(process_params_T,
                                            process_params_P,
                                            stop_flags_logi,
                                            outdir)
    removeModal()
    
    if (is.null(processing_output)) {
      message_finish <- list(h3("Error: processing ", strong(style="color: #FF0000", "FAILED!")),
                             h3("Output file was NOT generated."),
                             h3("Check the RStudio console for more information."),
                             h3("Please CORRECT THE ERROR and try again."))
      showModal(modalDialog(h3(message_finish),div(style="margin:auto;margin-top:7%;width:20%;",modalButton(strong("Ok, I try again"))),
                            footer = NULL))
    } else {
      message_finish <- list(h3("Processing finished -", strong(style="color: #00C000", "SUCCESS!")),
                             h3("Your output daily file is located here:"),
                             h5(normalizePath(file.path(outdir, "meteo_daily.dat"))),
                             h3("You can now close the app and RStudio."))
      showModal(modalDialog(h3(message_finish), footer = NULL))
    }
    
  })
  
}
# Run the app.
shinyApp(ui, server)
