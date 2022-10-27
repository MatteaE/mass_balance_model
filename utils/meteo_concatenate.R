###################################################################################################
# This program takes one folder and concatenates all text files inside into a single file.        #
# It is used for example to make one single file out of (e.g.) daily meteorological data files.   #
# NOTES:                                                                                          #
# 1. The program discards the first row of each file, which is assumed to be a title/header.      #
# 2. The files are NOT CHECKED at all. Make sure that they are in a suitable format.              #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2022/10/27                                                                       #
################################################################################################### 

suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(tools))


# Wrapper to call the entire processing when the user presses the RUN! button. ----------------------------------------
func_do_processing <- function(input_dir,
                               headerlines_n) {
  
  #sink(file = file.path(paste0("meteo_aggregate_run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")), split = TRUE)
  
  cat("Concatenate files called at", as.character(Sys.time()), paste0("(", Sys.timezone(), ")"), "\n")
  cat("System info:\n")
  print(R.version)
  cat("\n")
  
  lf <- file.path(input_dir, list.files(input_dir))
  lf_n <- length(lf)
  
  if (lf_n == 0) {
    return(NULL)
  }
  
  
  # Take header and extension from first file.
  fp_first <- lf[1]
  ext_first <- file_ext(fp_first)
  f_out <- file.path(input_dir, paste0("output_concatenated", ".", ext_first))
  h1 <- readLines(fp_first, n = headerlines_n)
  writeLines(h1, f_out) # This overwrites anything already in place.
  
  f_out_con <- file(f_out, open = "at") # Then we start appending.
  
  
  # Write files sequentially.
  for (f_id in 1:lf_n) {
    fp_cur <- lf[f_id]
    f_cur <- readLines(fp_cur)
    if (length(f_cur) > headerlines_n) {
      writeLines(f_cur[(1+headerlines_n):length(f_cur)], f_out_con)
    }
  }
  
  
  flush(f_out_con)
  close(f_out_con)
 
  
  #### Finish messages ####
  cat("\nPROGRAM FINISHED SUCCESSFULLY!\n")
  cat("Your output file is located here:\n")
  cat(normalizePath(f_out), "\n")
  cat("You can now close the app and RStudio.")
  
  # sink()
  
  return(normalizePath(f_out))
}




#### Definition of shiny app to use the above function ####
# Define UI for app ----
ui <- fluidPage(useShinyjs(),
                
                # . App title and description ----
                titlePanel("Concatenate meteo files"),
                
                checkboxInput("checkbox_show_help_text", "Show help text", FALSE),
                
                # . . Help text which can be shown by ticking a checkbox.
                conditionalPanel(condition = "input.checkbox_show_help_text == 1",
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("This program concatenates multiple text files into a single one, discarding the first (title/header) line of each file.")),
                                 p(),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("As"), strong(" INPUT "), em("please provide:")),
                                 tags$div(tags$ul(
                                   tags$li(em("the ", strong("folder"), " containing all the text files")),
                                   tags$li(em("the number of ", strong("header lines"), " at the beginning of each file.")),
                                   style = "margin-top: 0px; margin-bottom: 5px; text-align: justify;")),
                                 h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
                                    em("The program performs ", strong("absolutely no checks"), " on the files. Make sure that the folder you select contains ", strong("only"), " the files that you want to concatenate.")),
                                 p()),
                
                # . UI layout below the help text ----
                p(),
                
                # .. Input: choose folder which contains the wanted files ----
                shinyDirButton(id = "choose_input_folder", label = "Choose input folder", title = "Choose input folder", style = "width: 86.6%;font-weight:bold;"),
                p(),
                
                # .. Input: choose number of header lines ----
                tags$head(
                  tags$style(type="text/css", "#inline1 label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 20px; } 
                #inline1 .form-group { display: table-row;}")
                ),
                tags$div(id = "inline1", numericInput(inputId = "header_n", label = "Number of header lines:", value = NA_integer_, min = 0, max = Inf, step = 1)),
                p(),

                # .. Text fields: have we chosen the input directory and number of header lines? ----
                htmlOutput("inputdir_chosen_string"),
                htmlOutput("headerlines_chosen_string"),
                p(),
                
                # .. Input: do-it button ----
                actionButton(inputId = "startprocessing",
                             label = strong("RUN!")),
                p()
                
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  volumes <- c(getVolumes()(), setNames(dirname(getwd()), basename(dirname(getwd()))), setNames(dirname(dirname(getwd())), basename(dirname(dirname(getwd())))))
  shinyDirChoose(input, "choose_input_folder", roots=volumes, session=session)
  
  input_dir       <- reactive(as.character(parseDirPath(volumes, input$choose_input_folder)))
  headerlines_n   <- reactive(as.integer(input$header_n))
  
  # Disable "RUN!" button if the required input is missing.
  # NOTE: isTruthy(as.integer("a")) is FALSE.
  # This, together with the definition of firstrow_T above,
  # does the job of sanitizing the input for us.
  observe({
    toggleState("startprocessing", isTruthy(input_dir()))
  })
  
  # Show the user which input has been provided and which is still missing.
  output$inputdir_chosen_string <- renderText({
    ifelse(isTruthy(input_dir()),
           "<font color=\"#00C000\"><b>Input folder selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Input folder not yet selected.</b></font color>")
  })
  output$headerlines_chosen_string <- renderText({
    ifelse(isTruthy(headerlines_n()),
           "<font color=\"#00C000\"><b>Number of header lines selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Number of header lines not yet selected.</b></font color>")
  })

  
  # Button to start processing.
  observeEvent(input$startprocessing, {
    showModal(modalDialog(h3("Processing... See RStudio console for progress."), footer=NULL))
    
    processing_output <- func_do_processing(input_dir(),
                                            headerlines_n())
    
    if (is.null(processing_output)) {
      message_finish <- list(h3("ERROR: there was an error while processing the input."),
                             h3("Output file was NOT generated."),
                             h3("Check the RStudio console for more information."),
                             h3("Please correct the error and run the app again."))
    } else {
      message_finish <- list(h3("Processing finished successfully."),
                             h3("Your output file is located here:"),
                             h5(processing_output),
                             h3("You can now close the app and RStudio."))
    }
    removeModal()
    showModal(modalDialog(h3(message_finish), footer=NULL))
  })
  
}
# Run the app.
shinyApp(ui, server)
