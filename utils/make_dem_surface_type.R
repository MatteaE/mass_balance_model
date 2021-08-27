###################################################################################################
# This program takes a shapefile glacier outline and up to additional 2 outlines                  #
# (firn, debris), and produces a grid of surface type (sometimes called "firn file"),             #
# which can be used in the mass balance model.                                                    #
# The 2 additional shapefiles are optional: in case they are not provided, the output grid        #
# only has ice and rock (no firn and no debris).                                                  #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2021/8/16                                                                        #
###################################################################################################

suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(sf))
rasterOptions(todisk = TRUE) # Saves a lot of memory usage when processing large rasters.

#### Functions called by the app ####
func_long2utmzonenumber <- function(long) { # long is longitude in decimal degrees.
  return(ceiling((long + 180) / 6))
}


func_make_dem_dhm_surftype <- function(dem_filepath, outline_filepath, firn_filepath, debris_filepath, dem_buffer, dem_outpath, dhm_outpath, surftype_outpath) {
  
  resolution_proj_raster <- 20 # Always project grids to 20 m.
  
  has_firn   <- !is.na(firn_filepath)
  has_debris <- !is.na(debris_filepath)
  
  #### Load input ####
  dem_l1     <- raster(dem_filepath)
  outline_l1 <- st_read(outline_filepath, quiet = TRUE)
  if (has_firn)   firn_l1   <- st_read(firn_filepath, quiet = TRUE)
  if (has_debris) debris_l1 <- st_read(debris_filepath, quiet = TRUE)
  gc()
  
  
  #### Fix coordinate systems ####
  # Check CRS of both DEM and outline.
  # If both have same CRS and it is not 4326: leave as is and proceed.
  # If both have same CRS and it is 4326: project both to UTM, then proceed.
  # If CRS is not the same:
  # Check whether either DEM or shapefile is UTM (allow for a 1-zone tolerance for glaciers spanning UTM zone borders)
  # If yes: project only the other to the same UTM and proceed (preferentially reproject outline as it is faster)
  # If no: project both to UTM, then proceed.
  dem_crs            <- crs(dem_l1)
  outline_crs        <- crs(outline_l1)
  wgs84_crs          <- crs("EPSG:4326")
  
  # Find which UTM zone we should be using here in principle.
  # Also works if the outline is in some weird CRS.
  if (outline_crs@projargs == wgs84_crs@projargs) {
    outline_centroid   <- suppressWarnings(st_coordinates(st_centroid(outline_l1)))
  } else {
    outline_wgs84      <- st_transform(outline_l1, crs("EPSG:4326"))
    outline_centroid   <- suppressWarnings(st_coordinates(st_centroid(outline_wgs84)))
  }
  utm_crs_number       <- func_long2utmzonenumber(outline_centroid[1])
  utm_crs              <- crs(paste0("EPSG:", 32600 + utm_crs_number))
  
  # Flags which are set in the following *if* routine.
  reproj_dem         <- FALSE
  reproj_outline     <- FALSE
  if ((dem_crs@projargs == outline_crs@projargs) && (dem_crs@projargs != wgs84_crs@projargs)) {
    
    cat("DEM and shapefile are already in the same projected coordinates. I don't reproject them.\n")
    
  } else if ((dem_crs@projargs == outline_crs@projargs) && (dem_crs@projargs == wgs84_crs@projargs)) {
    
    cat(paste0("DEM and shapefile are both in WGS84 (EPSG:4326). I am reprojecting them to UTM (zone ", utm_crs_number, "N) before proceeding.\n"))
    cat("This can take some minutes if the DEM is big.\n")
    
    reproj_dem       <- TRUE
    reproj_outline   <- TRUE
    target_crs       <- utm_crs 
    
  } else if (dem_crs@projargs != outline_crs@projargs) {
    
    cat ("DEM and shapefile do not have the same coordinate system.\n")
    utm_crs_allowed  <- sapply(paste0("EPSG:", 32600 + utm_crs_number + -1:1), function(x) crs(x)@projargs) # Allow a 1-zone tolerance, for glaciers near the UTM zone borders.
    
    if (dem_crs@projargs %in% utm_crs_allowed) { # Reproject outline.
      cat("DEM coordinate system is good, I am reprojecting the shapefile.\n")
      reproj_outline <- TRUE
      target_crs     <- dem_crs
      
    } else if (outline_crs@projargs %in% utm_crs_allowed) { # Reproject DEM.
      
      cat("Shapefile coordinate system is good, I am reprojecting the DEM. This can take some minutes if the DEM is big.\n")
      reproj_dem     <- TRUE
      target_crs     <- outline_crs
      
    } else { # Reproject both.
      
      cat(paste0("I am reprojecting both DEM and shapefile to UTM (zone ", utm_crs_number, "N). This can take some minutes if the DEM is big.\n"))
      reproj_dem     <- TRUE
      reproj_outline <- TRUE
      target_crs     <- utm_crs 
      
    }
  }
  
  # Now do the reprojections which we have decided above.
  dem_l2             <- dem_l1
  outline_l2         <- outline_l1
  if (reproj_dem)     dem_l2      <- projectRaster(dem_l1, res = resolution_proj_raster, crs = target_crs, method = "bilinear")
  if (reproj_outline) outline_l2  <- st_transform(outline_l1, target_crs)
  # Always reproject firn and debris shapefiles. Easier than doing all comparisons of the projection.
  if (has_firn)   firn_l2   <- st_transform(firn_l1, crs(outline_l2))
  if (has_debris) debris_l2 <- st_transform(debris_l1, crs(outline_l2))
  gc()
  
  #### Extract grids with buffer ####
  cat("Preparing output...\n")
  ext_out <- st_bbox(outline_l2) + dem_buffer * c(-1,-1,1,1)
  dhm_out <- crop(dem_l2, extent(ext_out))
  dem_out <- mask(dhm_out, outline_l2)
  surftype_out <- 4*is.na(dem_out) # This is the base rock/ice mask.
  if (has_firn)   surftype_out <- mask(surftype_out, firn_l2, inverse = TRUE, updatevalue = 1)   # Add firn if we have it.
  if (has_debris) surftype_out <- mask(surftype_out, debris_l2, inverse = TRUE, updatevalue = 5) # Add debris if we have them.

  NAvalue(dhm_out)      <- -9999
  NAvalue(dem_out)      <- -9999
  NAvalue(surftype_out) <- -9999
  
  #### Write grids to output ####
  cat("Writing output to disk...\n")
  writeRaster(dhm_out, dhm_outpath, overwrite = TRUE)
  writeRaster(dem_out, dem_outpath, overwrite = TRUE)
  writeRaster(surftype_out, surftype_outpath, overwrite = TRUE)
  cat("Program finished succesfully!\n")
  cat("The new grid files are located here:\n")
  cat(normalizePath(dirname(dhm_outpath)), "\n")
  cat("Remember to change the file names to use them in the model.\n")
  cat("You can now close the program.")
  
} # End of function definition.


#### Definition of shiny app to use the above function ####
# Define UI for app ----
ui <- fluidPage(
  
  # . App title and description ----
  titlePanel("Mass balance model assistant"),
  h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
     em("This program generates the grids of DEM and surface type which are used in the glacier mass balance model.")),
  h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
     em("As input data, please provide:")),
  tags$div(tags$ul(
    tags$li("a big DEM file (altitude grid, for example .tif or .hgt, from EarthExplorer, SRTM, ASTER or any other)"),
    tags$li("a shapefile for the glacier outline, for example .shp"),
    tags$li("(OPTIONAL): a shapefile with the firn area"),
    tags$li("(OPTIONAL): a shapefile with the debris cover.")),
    style = "margin-top: 0px; margin-bottom: 5px; font-style: italic;"),
  h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
     em("The program will create three grids:")),
  tags$div(tags$ul(
    tags$li("a DHM (altitude grid, as a full rectangle around the glacier)"),
    tags$li("a DEM (altitude grid, only where there is ice, with no data outside the glacier)"),
    tags$li("a grid of surface type (rock/ice/firn/debris, important for albedo).")),
    style = "margin-top: 0px; margin-bottom: 5px; font-style: italic;"),
  h5(style="text-align: justify; margin-top: 0px;",
     em("The coordinate system (UTM / WGS84) is adjusted automatically.")),
  
  # . UI layout ----
  verticalLayout(
    
    p(),
    p(),
    
    # .. Input: choose DEM, outline file, and (optionally) firn and debris shapefiles ----
    shinyFilesButton("choose_dem_file", strong("Choose input DEM file"), "Choose input DEM file", FALSE),
    p(),
    shinyFilesButton("choose_shp_file", strong("Choose input glacier shapefile"), "Choose input glacier shapefile", FALSE),
    p(),
    shinyFilesButton("choose_firn_file", strong("Choose input firn shapefile (optional)"), "Choose input firn shapefile (optional)", FALSE),
    p(),
    shinyFilesButton("choose_debris_file", strong("Choose input debris shapefile (optional)"), "Choose input debris shapefile (optional)", FALSE),
    p(),
    
    # .. Input: choose margin in meters ----
    numericInput(inputId = "buffersize",
                 label = "Choose margin distance all around the outline, in meters:",
                 value = 500,
                 min = 1,
                 max = 10000),
    
    # .. Text fields: show the full path of the chosen DEM and outline file ----
    htmlOutput("dem_chosen_string"),
    htmlOutput("shp_chosen_string"),
    htmlOutput("firn_chosen_string"),
    htmlOutput("debris_chosen_string"),
    p(),
    
    # .. Input: do-it button ----
    actionButton(inputId = "startprocessing",
                 label = strong("RUN!")),
    p()
    
  )
  
)

# Define server logic to read selected file ----
server <- function(input, output, session) {

  volumes <- c(getVolumes()(), setNames(getwd(), basename(getwd())))
  shinyFileChoose(input, "choose_dem_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_shp_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_firn_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_debris_file", roots=volumes, session=session)

  demfilepath    <- reactive(as.character(parseFilePaths(volumes, input$choose_dem_file)$datapath))
  shpfilepath    <- reactive(as.character(parseFilePaths(volumes, input$choose_shp_file)$datapath))
  firnfilepath   <- reactive(as.character(parseFilePaths(volumes, input$choose_firn_file)$datapath))
  debrisfilepath <- reactive(as.character(parseFilePaths(volumes, input$choose_debris_file)$datapath))
  
  # Show the user which files have been provided and which are still missing.
  output$dem_chosen_string <- renderText({
    ifelse(isTruthy(demfilepath()),
           "<font color=\"#00DD00\"><b>Input DEM file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Input DEM file not yet selected.</b></font color>")
  })
  output$shp_chosen_string <- renderText({
    ifelse(isTruthy(shpfilepath()),
           "<font color=\"#00DD00\"><b>Input outline file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Input outline file not yet selected.</b></font color>")
  })
  output$firn_chosen_string <- renderText({
    ifelse(isTruthy(firnfilepath()),
           "<font color=\"#00DD00\"><b>Input firn shapefile selected.</b></font color>",
           "<font color=\"#FF8000\"><b>Input firn shapefile (optional) not yet selected.</b></font color>")
  })
  output$debris_chosen_string <- renderText({
    ifelse(isTruthy(debrisfilepath()),
           "<font color=\"#00DD00\"><b>Input debris shapefile selected.</b></font color>",
           "<font color=\"#FF8000\"><b>Input debris shapefile (optional) not yet selected.</b></font color>")
  })

  
  observeEvent(input$startprocessing, {
    req(input$choose_dem_file)
    req(input$choose_shp_file)
    firnfilepath_sel   <- ifelse(isTruthy(firnfilepath()), firnfilepath(), NA)
    debrisfilepath_sel <- ifelse(isTruthy(debrisfilepath()), debrisfilepath(), NA)
    showModal(modalDialog(h3("Processing... See RStudio console for progress."), footer=NULL))
    func_make_dem_dhm_surftype(demfilepath(), shpfilepath(), firnfilepath_sel, debrisfilepath_sel, input$buffersize, file.path(getwd(), "dem_glacier.asc"), file.path(getwd(), "dhm_glacier.asc"), file.path(getwd(), "surface_type_glacier.asc"))
    file.rename(file.path(getwd(), "dem_glacier.asc"), file.path(getwd(), "dem_glacier.grid"))
    file.rename(file.path(getwd(), "dhm_glacier.asc"), file.path(getwd(), "dhm_glacier.grid"))
    file.rename(file.path(getwd(), "surface_type_glacier.asc"), file.path(getwd(), "surface_type_glacier.grid"))
    removeModal()
    showModal(modalDialog(h3("Processing finished. See RStudio console for details. You can now close the program."), footer=NULL))
  })
  
}
# Run the app.
shinyApp(ui, server)
