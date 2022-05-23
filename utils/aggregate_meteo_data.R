###################################################################################################
# This program takes two files (temperature and precipitation) with sub-daily resolution,         #
# and produces daily (optionally weekly and monthly) aggregates.                                  #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2021/11/26                                                                       #
###################################################################################################

suppressPackageStartupMessages(library(timeSeries))
suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(shinyjs))

#### Functions called by the app ####

# This function does the entire app processing when the user presses the button.
func_do_processing <- function(temp_filepath,
                               prec_filepath,
                               compute_week_month_logi,
                               outpath) {
  
  #### Load input ####
  temp <- read.csv(temp_filepath, header = TRUE)
  prec <- read.csv(prec_filepath, header = TRUE)
  
  cat("NAs in temperature:", length(which(is.na(temp[,2]))), "\n")
  cat("NAs in precipitation:", length(which(is.na(prec[,2]))), "\n")
  prec_neg_n <- length(which(prec[,2] < 0))
  cat("precipitation < 0:", prec_neg_n, "\n")
  if (prec_neg_n > 0) {
    message("WARNING: found ", prec_neg_n, " measurements with precipitation < 0. Please investigate!")
  }
  
  
  temp$timestamp <- as.POSIXct(temp[,1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  prec$timestamp <- as.POSIXct(prec[,1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  temp_n <- nrow(temp)
  prec_n <- nrow(prec)
  
  
  # Now handle irregular series frequency.
  # We resample both series to 1 minute,
  # should be always safe - if it does not work
  # automatically move to 1 second.
  temp_ts_diff <- unique(diff(temp$timestamp))
  prec_ts_diff <- unique(diff(prec$timestamp))
  
  if (length(temp_ts_diff) > 1) {
    cat("Temperature series has an irregular frequency. No problem, I can deal with this.\n")
  }
  if (length(prec_ts_diff) > 1) {
    cat("Precipitation series has an irregular frequency. No problem, I can deal with this.\n")
  }
  
  # Compute series bounds, extended from/into midnight.
  ts_start <- min(temp$timestamp[1], prec$timestamp[1])
  ts_start_midnight <- as.POSIXct(paste(format(ts_start, "%Y-%m-%d"), "00:00:00"), tz = "UTC")
  ts_end <- max(temp$timestamp[temp_n], prec$timestamp[prec_n])
  ts_end_midnight <- as.POSIXct(paste(format(ts_end, "%Y-%m-%d"), "00:00:00"), tz = "UTC") + 86400
  

  # If the two series are just (N-)hourly and
  # well behaved, no need to go to minute resolution.
  # Work hourly.
  if (((as.numeric(temp_ts_diff) %% 1) == 0) &&
    ((as.numeric(prec_ts_diff) %% 1) == 0)) {
    proc_freq <- "1 hour"
  } else {
    proc_freq <- "1 min"
  }
  
  # Else, try a 1-minute interval. It should usually work
  # unless the input series is really odd.
  ts_all <- seq.POSIXt(ts_start_midnight, ts_end_midnight, by = proc_freq)
  
  # Now insert the input series into the 1-minute one.
  temp_match_ids <- match(temp$timestamp, ts_all)
  prec_match_ids <- match(prec$timestamp, ts_all)
  
  # If we don't find a place for some indices, it means that
  # the input series have sub-minutely offsets. We handle them
  # by dropping the seconds information (else processing is too
  # slow and memory hungry).
  if (any(is.na(temp_match_ids)) || any(is.na(prec_match_ids))) {
    message("WARNING: there are some measurements with strange (sub-minute) offsets.")
    message("I am dropping the seconds information, to work at 1 minute intervals.")
    temp$timestamp <- as.POSIXct(format(temp$timestamp, format = "%Y%m%d%H%M"), format = "%Y%m%d%H%M", tz = "UTC")
    prec$timestamp <- as.POSIXct(format(prec$timestamp, format = "%Y%m%d%H%M"), format = "%Y%m%d%H%M", tz = "UTC")
    temp_match_ids <- match(temp$timestamp, ts_all)
    prec_match_ids <- match(prec$timestamp, ts_all)
  } else {
    cat("All measurements have found their place in the series...\n")
  }

  # Create 1-minute (or 1-hour) series with longest possible extent
  # (extended to midnights on the first and last days).
  df_all <- data.frame(timestamp = ts_all, date = as.Date(ts_all), temp = NA_real_, prec = NA_real_)  
  all_n <- length(ts_all)
  
  # Here insert the data into the new data frame.  
  df_all$temp[temp_match_ids] <- temp[,2]
  df_all$prec[prec_match_ids] <- prec[,2]
  
  # Temperatures are linearly interpolated,
  # precipitation NAs are changed into zeros.
  cat("Resampling temperature...\n")
  df_all$temp_interp <- as.numeric(na.omit(timeSeries(df_all$temp), method = "ie", interp = "linear"))
  cat("Resampling precipitation...\n")
  df_all$prec_interp <- as.numeric(na.omit(timeSeries(df_all$prec), method = "z"))
  
  # Now aggregate to daily resolution.
  cat("Aggregating to daily resolution...\n")
  day_start <- as.Date(df_all$timestamp[1])
  day_end <- as.Date(df_all$timestamp[all_n-1]) # -1: the last timestep is always midnight on the day just after the measurements, so we remove it.
  day_ts <- seq.Date(day_start, day_end, by = "1 day")
  day_n <- length(day_ts)
  
  df_daily <- data.frame(day = day_ts, temp = NA_real_, prec = NA_real_)
  ts_in_day_n <- (all_n-1)/day_n
  
  for (day_id in 1:day_n) {
    day_ts_ids <- (day_id-1)*ts_in_day_n + 1:ts_in_day_n
    df_daily$temp[day_id] <- mean(df_all$temp_interp[day_ts_ids])
    df_daily$prec[day_id] <- sum(df_all$prec_interp[day_ts_ids])
  }
  
  # Write output, same directory as temperature input.
  df_daily_out <- data.frame(year = as.integer(format(df_daily$day, "%Y")),
                             doy = as.integer(format(df_daily$day, "%j")),
                             hour = 12, 
                             t2m_mean = df_daily$temp,
                             precip = df_daily$prec)
  df_daily_out$t2m_mean <- round(df_daily_out$t2m_mean, 3)
  write.table(df_daily_out, file.path(outpath, "meteo_daily.dat"), quote = F, row.names = F, col.names = T)
  
  
  if (compute_week_month_logi == TRUE) {
    # Aggregate to weekly and write output.
    cat("Aggregating to weekly resolution...\n")
    df_weekly <- data.frame(week = 1:(floor(day_n / 7)))
    df_weekly$temp <- round(rowMeans(matrix(df_daily$temp[1:(day_n-(day_n%%7))], ncol = 7, byrow = TRUE)), 3)
    df_weekly$prec <- rowSums(matrix(df_daily$prec[1:(day_n-(day_n%%7))], ncol = 7, byrow = TRUE))
    write.table(df_weekly, file.path(outpath, "meteo_weekly.dat"), quote = F, row.names = F, col.names = T)
    
    
    # Aggregate to monthly and write output.
    cat("Aggregating to monthly resolution...\n")
    months_seq <- unique(format(df_daily$day, "%Y-%m"))
    df_monthly <- data.frame(month = months_seq,
                             temp = NA_real_,
                             prec = NA_real_)
    for (month_id in 1:length(months_seq)) {
      df_monthly$temp[month_id] <- round(mean(df_daily$temp[which(format(df_daily$day, "%Y-%m") == months_seq[month_id])]), 3)
      df_monthly$prec[month_id] <- sum(df_daily$prec[which(format(df_daily$day, "%Y-%m") == months_seq[month_id])])
    }
    write.table(df_monthly, file.path(outpath, "meteo_monthly.dat"), quote = F, row.names = F, col.names = T)
  }
  
  
  #### Finish messages ####
  cat("Program finished!\n")
  message("Your new files are located here:")
  cat(normalizePath(outpath), "\n")
  message("You can now close the program.")
  
  return(0)
  
} # End of func_do_processing() definition.


#### Definition of shiny app to use the above function ####
# Define UI for app ----
ui <- fluidPage(useShinyjs(),
                
                # . App title and description ----
                titlePanel("Aggregate meteo data"),
                
                checkboxInput("checkbox_show_help_text", "Show help text", FALSE),
                
                # . . Help text which can be shown by ticking a checkbox.
                conditionalPanel(condition = "input.checkbox_show_help_text == 1",
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("This program aggregates temperature and precipitation data, from sub-daily frequency (also irregular) to daily, weekly and monthly. The daily aggregation can be used as input for the mass balance model.")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("As"), strong(" INPUT DATA "), em("please provide:")),
                                 tags$div(tags$ul(
                                   tags$li(em("a CSV ", strong("temperature file"))),
                                   tags$li(em("a CSV ", strong("precipitation file"))),
                                   style = "margin-top: 0px; margin-bottom: 5px; text-align: justify;")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("Input files must have two columns: ", strong("time"), " and ", strong("value."),
                                    br(),
                                    "The first line of each column must have the ", strong("column name"), " (like \"time\" and \"temperature\").",
                                    br(),
                                    "To aggregate values, temperature is ", strong("averaged,"), " precipitation is ", strong("accumulated.")))),
                
                # . UI layout below the help text ----
                p(),
                
                # .. Input: choose temperature and precipitation files.
                shinyFilesButton("choose_temp_file", strong("Choose temperature file"), "Choose temperature file", multiple = FALSE, style = "width: 60%;"),
                p(),
                shinyFilesButton("choose_prec_file", strong("Choose precipitation file"), "Choose precipitation file", multiple = FALSE, style = "width: 60%;"),
                p(),
                # .. Input: should we also compute weekly and monthly files? ----
                checkboxInput("checkbox_compute_week_month", div("Compute also ", strong("weekly"), " and ", strong("monthly"), "aggregations."), FALSE, width = "100%"),
                p(),
                
                # .. Text fields: show the full path of the chosen temperature and precipitation files ----
                htmlOutput("temp_chosen_string"),
                htmlOutput("prec_chosen_string"),
                p(),
                
                # .. Input: do-it button ----
                actionButton(inputId = "startprocessing",
                             label = strong("RUN!")),
                p()
                
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  volumes <- c(getVolumes()(), setNames(dirname(getwd()), basename(dirname(getwd()))), setNames(dirname(dirname(getwd())), basename(dirname(dirname(getwd())))))
  shinyFileChoose(input, "choose_temp_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_prec_file", roots=volumes, session=session)
  
  tempfilepath       <- reactive(as.character(parseFilePaths(volumes, input$choose_temp_file)$datapath))
  precfilepath       <- reactive(as.character(parseFilePaths(volumes, input$choose_prec_file)$datapath))
  
  # Disable "RUN!" button if the required input is missing.
  observe({
    toggleState("startprocessing", isTruthy(tempfilepath()) && isTruthy(precfilepath()))
  })
  
  # Show the user which input has been provided and which is still missing.
  output$temp_chosen_string <- renderText({
    ifelse(isTruthy(tempfilepath()),
           "<font color=\"#00C000\"><b>Temperature file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Temperature file not yet selected.</b></font color>")
  })
  output$prec_chosen_string <- renderText({
    ifelse(isTruthy(precfilepath()),
           "<font color=\"#00C000\"><b>Precipitation file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Precipitation file not yet selected.</b></font color>")
  })
  
  
  # Button to start processing.
  observeEvent(input$startprocessing, {
    # These 2 below are probably not needed since the RUN! button is
    # disabled by shinyjs, but we keep them anyway since they make sense.
    req(input$choose_temp_file)
    req(input$choose_prec_file)
    showModal(modalDialog(h3("Processing... See RStudio console for progress."), footer=NULL))
    processing_output <- func_do_processing(tempfilepath(), precfilepath(), input$checkbox_compute_week_month, dirname(tempfilepath()))
    removeModal()
    showModal(modalDialog(h3("Processing finished. See RStudio console for details. You can now close the program."), footer=NULL))
  })
  
}
# Run the app.
shinyApp(ui, server)
