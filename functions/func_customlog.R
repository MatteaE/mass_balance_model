###################################################################################################
# Author:         Enrico Mattea (@unifr.ch)                                                       #
# Description:    this program models the distributed mass balance of a glacier at daily          #
#                 resolution, optimizing model parameters towards the best fit with point         #
#                 mass balance measurements.                                                      #
#                 This file contains the definition of a function to write warnings and errors    #
#                 with custom styling, while also writing them to the logfile, bypassing          #
#                 the default sink() behavior which does not capture the output of message().     #
################################################################################################### 

# Note: this function (1) does not automatically add a whitespace between multiple arguments,
#                     (2) does automatically add a new line at the end.

# 0 or "b(asic)" calls message() with no formatting
# 1 or "w(arning)" calls message() prepending a bold, yellow-shaded "WARNING" to the text
# 2 or "f(atal)" calls message() prepending a bold, red-shaded "FATAL: " to the text
# 3 or "s(uccess)" calls message() prepending a bold, green-shaded "SUCCESS" to the text
# 4 or "i(nfo)" calls message() with bold text
func_customlog <- function(..., level = c("basic", "warning", "fatal", "success", "info")) {
  
  # Find out which level is the log (either numeric or text).
  if (is.character(level)) {
    level <- match.arg(level)
  } else {
    if (length(level) != 1) {
      cat("Wrong error level (provided: ", paste0(level, collapse = " "), ")")
      func_stop()
    }
    allowed_levels <- 0:4
    if (level %in% allowed_levels) {
      level <- c("basic", "warning", "fatal", "success", "info")[level[1]+1]
    } else {
      cat("Wrong error level (should be one of: ", paste0(allowed_levels, collapse = " "), "; provided: ", paste0(level, collapse = " "), ")")
      func_stop()
    }
  }
  
  
  # Concatenate input into a single character.
  txt <- paste(lapply(list(...), as.character), collapse = "")
  
  # Select OS-specific function to write to console.
  # RStudio on Windows uses an ugly red text for message(),
  # so we use cat() and ANSI bold text instead.
  if (Sys.info()["sysname"] == "Windows") {
    
    func_consolewrite <- function(prefix, txt) {
      cat(paste0(prefix,
                 "\033[1m", txt, "\033[0m", "\n"))
    }
    
    # On other systems use message(): RStudio uses a nice grey background.
  } else {
    
    func_consolewrite <- function(prefix, txt) {
      message(paste0(prefix,
                     txt))
    }
  }
  
  
  # Temporarily suspend the split sink(), since we need to write to the log file and not to the console.
  flush(logcon)
  sink()
  
  # Write the message to console and log file.
  if (level == "basic") {
    
    func_consolewrite("", txt)
    writeLines(paste0(txt, "\n"), con = logcon, sep = "")
    
  } else if (level == "warning") {
    
    func_consolewrite("\033[1;48;5;226;38;5;16m WARNING \033[0m ", txt) # WARNING in bold black text on yellow background
    writeLines(paste0("* WARNING: ", txt, "\n"), con = logcon, sep = "")
    
  } else if (level == "fatal") {
    
    func_consolewrite("\033[1;48;5;196;38;5;231m FATAL \033[0m ", txt) # FATAL in bold white text on red background
    writeLines(paste0("** FATAL: ", txt, "\n"), con = logcon, sep = "")
    
  } else if (level == "success") {
    
    func_consolewrite("\033[1;48;5;28;38;5;231m SUCCESS \033[0m ", txt) # SUCCESS in bold white text on green background
    writeLines(paste0("** SUCCESS: ", txt, "\n"), con = logcon, sep = "")
    
  } else {
    
    func_consolewrite("\033[1;48;5;45;38;5;16m INFO \033[0m ", txt) # INFO in bold black text on light blue background
    writeLines(paste0("INFO: ", txt, "\n"), con = logcon, sep = "")
    
  }
  
  flush(logcon)
  sink(logcon, split = TRUE) # Restore the split sink()
  
}
