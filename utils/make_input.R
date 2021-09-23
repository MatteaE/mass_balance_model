###################################################################################################
# This program takes a shapefile glacier outline and up to additional 2 outlines                  #
# (firn, debris), and produces grids which can be used in the mass balance model:                 #
# DHM, surface type and optionally daily incoming solar radiation.                                #
# The 2 additional shapefiles are optional: in case they are not provided, the output grid        #
# only has ice and rock (no firn and no debris).                                                  #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2021/9/21                                                                        #
###################################################################################################

suppressPackageStartupMessages(library(insol))
suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tools))
rasterOptions(todisk = TRUE) # Saves a lot of memory usage when processing large rasters.

#### Functions called by the app ####
# This function computes gridded total potential incoming solar radiation for one specific day.
  # norm_mat: matrix with surface normals
  # lat, lon: DEM center, as reference to compute day length
  # ele_ref: elevation to use for the calculation. We use the mean of the DEM.
  # delta_t: time-step of the computation, in hours
func_compute_day_rad <- function(dem_mat,
                                 norm_mat,
                                 dem_res,
                                 lat,
                                 lon,
                                 ele_ref,
                                 year_cur,
                                 doy_cur,
                                 delta_t) {
  
  date_cur <- as.Date(paste(year_cur, doy_cur), format = "%Y %j")
  month_cur <- as.integer(format(date_cur, "%m"))
  day_cur <- as.integer(format(date_cur, "%d"))
  jd_cur <- JDymd(year_cur, month_cur, day_cur)
  
  Iglobal <- array(0, dim = dim(dem_mat))
  
  dayl <- daylength(lat, lon, jd_cur, 0)
  
  for (hour_cur in seq(dayl[1], dayl[2], delta_t)) {
    
    jd_cur <- JDymd(year_cur, month_cur, day_cur, hour_cur)
    sun_vec <- sunvector(jd_cur, lat, lon, 0)
    hillshade_cur <- hillshading(norm_mat, sun_vec)
    shaded <- doshade(dem_mat, sun_vec, dem_res)
    sun_zenith <- degrees(acos(sun_vec[,3]))
    # Compute direct radiation modified by terrain + diffuse irradiation (sky-view factor ignored).
    Idirdif = insolation(sun_zenith, jd_cur, ele_ref, visibility, rh, tempK, O3, alphag)
    Iglobal = Iglobal + (Idirdif[,1] * hillshade_cur + Idirdif[,2] ) * delta_t / 24 # Values in W m^-2
    
  }
  
  return(Iglobal)
}


# This function computes 365 daily radiation grids and stores them on disk.
# It calls the previous function.
  # delta_t: time-step for the calculation, in hours.
func_compute_all_daily_pisr <- function(dem,
                                        year_cur,
                                        delta_t,
                                        outpath_base) {
  
  dir.create(file.path(outpath_base, "radiation"), showWarnings = FALSE)
  
  # Setup useful DEM variables.
  dem_mat <- as.matrix(dem)
  norm_mat <- cgrad(dem)
  
  dem_res <- xres(dem)
  dem_crs <- crs(dem)
  dem_ext <- extent(dem)
  
  # Get lat/lon extent, to compute midpoint lat/lon.
  dem_proj_ext <- projectExtent(dem, crs("EPSG:4326"))@extent
  lon <- (dem_proj_ext[1] + dem_proj_ext[2]) / 2
  lat <- (dem_proj_ext[3] + dem_proj_ext[4]) / 2
  
  # Reference altitude for irradiance calculation: the mean of the DEM.
  ele_ref <- mean(getValues(dem))
  
  # Irradiance model constants.
  visibility <<- 50   # [km]
  rh         <<- 60    # [%]
  tempK      <<- 280   # [K]
  O3         <<- 0.002 # [cm]
  alphag     <<- 0.5   # [-]
  
  for (doy_cur in 1:365) {
    
    cat("\r", doy_cur, "/", "365...")
    
    # Compute daily radiation.
    rad_cur_mat <- func_compute_day_rad(dem_mat,
                                        norm_mat,
                                        dem_res,
                                        lat,
                                        lon,
                                        ele_ref,
                                        year_cur,
                                        doy_cur,
                                        delta_t)
    
    # Convert matrix to raster.
    rad_cur_ras <- round(raster(rad_cur_mat, crs = dem_crs))
    extent(rad_cur_ras) <- dem_ext
    NAvalue(rad_cur_ras) <- -9999
    
    # Write file to geotiff.
    rad_out_filepath_cur <- file.path(outpath_base, "radiation", paste0("dir", sprintf("%03d", doy_cur), "24.tif"))
    writeRaster(rad_cur_ras, rad_out_filepath_cur, overwrite = TRUE, datatype = "FLT4S")
    file.rename(rad_out_filepath_cur, paste0(file_path_sans_ext(rad_out_filepath_cur), ".tif"))
  }
  
  cat("\n")
}


# This function converts from longitude to UTM zone number.
func_long2utmzonenumber <- function(long) { # long is longitude in decimal degrees.
  return(ceiling((long + 180) / 6))
}


# This function does the entire app processing when the user presses the button.
func_do_processing <- function(dem_filepath,
                               outline_filepath,
                               firn_filepath,
                               debris_filepath,
                               reference_filepath,
                               dem_buffer,
                               compute_radiation_bool,
                               outpath_base) {
  
  resolution_proj_raster <- 20 # Always project grids to 20 m.
  
  has_firn   <- !is.na(firn_filepath)
  has_debris <- !is.na(debris_filepath)
  has_reference <- !is.na(reference_filepath)
  
  #### Load input ####
  dem_l1     <- raster(dem_filepath)
  outline_l1 <- st_zm(st_read(outline_filepath, quiet = TRUE))
  if (has_firn)      firn_l1      <- st_zm(st_read(firn_filepath, quiet = TRUE))
  if (has_debris)    debris_l1    <- st_zm(st_read(debris_filepath, quiet = TRUE))
  if (has_reference) reference_l1 <- raster(reference_filepath)
  gc()
  
  
  #### Fix coordinate systems ####
  # If reference grid is given: use its CRS.
  # Else check CRS of both DEM and outline.
  # If both have same CRS and it is not 4326: leave as is and proceed.
  # If both have same CRS and it is 4326: project both to UTM, then proceed.
  # If CRS is not the same:
  # Check whether either DEM or shapefile is UTM (allow for a 1-zone tolerance for glaciers spanning UTM zone borders)
  # If yes: project only the other to the same UTM and proceed (preferentially reproject outline as it is faster)
  # If no: project both to UTM, then proceed.
  dem_crs            <- crs(dem_l1)
  outline_crs        <- crs(outline_l1)
  wgs84_crs          <- crs("EPSG:4326")
  
  # Flags which are set according to the following logic routine.
  reproj_dem         <- FALSE
  reproj_outline     <- FALSE
  
  cat("\nCoordinate system is checked...\n")
  
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
    
    if (has_reference) {
      reference_crs <- crs(reference_l1)
      
      # If the reference is a .grid file
      # (e.g. which we have just produced),
      # it has no CRS! So in that case we
      # assume that the grid uses the UTM CRS
      # of our choice.
      if (is.na(reference_crs@projargs)) {
        crs(reference_l1) <- utm_crs
      }
      
      target_crs    <- reference_crs
      
      if (dem_crs@projargs     != reference_crs@projargs) reproj_dem     <- TRUE
      if (outline_crs@projargs != reference_crs@projargs) reproj_outline <- TRUE
      
    # If there is no reference grid supplied for alignment.
    } else {
    
    if ((dem_crs@projargs == outline_crs@projargs) && (dem_crs@projargs != wgs84_crs@projargs)) {
      
      cat("DEM and shapefile are already in the same projected coordinates. I don't reproject them.\n")
      
    } else if ((dem_crs@projargs == outline_crs@projargs) && (dem_crs@projargs == wgs84_crs@projargs)) {
      
      message(paste0("DEM and shapefile are both in WGS84 (EPSG:4326). I am reprojecting them to UTM (zone ", utm_crs_number, "N) before proceeding."))
      message("This can take some minutes if the DEM is big.")
      
      reproj_dem       <- TRUE
      reproj_outline   <- TRUE
      target_crs       <- utm_crs 
      
    } else if (dem_crs@projargs != outline_crs@projargs) {
      
      message("DEM and shapefile do not have the same coordinate system.")
      utm_crs_allowed  <- sapply(paste0("EPSG:", 32600 + utm_crs_number + -1:1), function(x) crs(x)@projargs) # Allow a 1-zone tolerance, for glaciers near the UTM zone borders.
      
      if (dem_crs@projargs %in% utm_crs_allowed) { # Reproject outline.
        message("DEM coordinate system is good, I am reprojecting the shapefile.")
        reproj_outline <- TRUE
        target_crs     <- dem_crs
        
      } else if (outline_crs@projargs %in% utm_crs_allowed) { # Reproject DEM.
        
        cat("Shapefile coordinate system is good, I am reprojecting the DEM. This can take some minutes if the DEM is big.")
        reproj_dem     <- TRUE
        target_crs     <- outline_crs
        
      } else { # Reproject both.
        
        message(paste0("I am reprojecting both DEM and shapefile to UTM (zone ", utm_crs_number, "N). This can take some minutes if the DEM is big."))
        reproj_dem     <- TRUE
        reproj_outline <- TRUE
        target_crs     <- utm_crs
        
      } # End of "Reproject both".
    } # End of "DEM and shapefile do not have the same coordinate system".
  } # End of "if (has_reference)".
  
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
  # If we just give the buffer size, we just extract the DHM region.
  # Instead, if we give a reference DEM we may have to resample (bilinear filter) ours, because
  # resolution/origin/extent could be different (even after adjusting projection, which we have done above).
  cat("\nPreparing output...\n")
  if (!has_reference) {
    ext_out <- st_bbox(outline_l2) + dem_buffer * c(-1,-1,1,1)
    dhm_out <- crop(dem_l2, ext_out)
  } else {
    ext_out <- extent(reference_l1)
    
    # Check resolution/origin/extent
    # (actually, number of rows/columns and extent,
    # that's enough also for origin).
    # If needed, do the resampling.
    if ((nrow(dem_l2)   != nrow(reference_l1))   ||
        (ncol(dem_l2)   != ncol(reference_l1))   ||
        (extent(dem_l2) != extent(reference_l1))) {
          
          dhm_out <- resample(dem_l2, reference_l1, method = "bilinear")
      
    }
  }

  dem_out <- mask(dhm_out, outline_l2)
  surftype_out <- 4*is.na(dem_out) # This is the base rock/ice mask.
  if (has_firn)   surftype_out <- mask(surftype_out, firn_l2, inverse = TRUE, updatevalue = 1)   # Add firn if we have it.
  if (has_debris) surftype_out <- mask(surftype_out, debris_l2, inverse = TRUE, updatevalue = 5) # Add debris if we have them.
  
  
  #### Write grids to output ####
  NAvalue(dhm_out)      <- -9999
  NAvalue(surftype_out) <- -9999
  dir.create(file.path(outpath_base, "dhm"), recursive = TRUE)
  dir.create(file.path(outpath_base, "surftype"))
  writeRaster(dhm_out, file.path(outpath_base, "dhm", "dhm_glacier.tif"), overwrite = TRUE)
  writeRaster(surftype_out, file.path(outpath_base, "surftype", "surface_type_glacier.tif"), overwrite = TRUE)
  
  #### Compute radiation if asked to do so ####
  if (compute_radiation_bool) {
    cat("Computing daily solar radiation. This can take a few minutes...\n")
    func_compute_all_daily_pisr(dhm_out,
                                2020,
                                0.1,
                                outpath_base)
  }
  
  
  #### Finish messages ####
  cat("Program finished succesfully!\n")
  message("Your new files are located here:")
  cat(normalizePath(file.path(getwd(), "input")), "\n")
  cat("Before you run the mass balance model, remember to move them to the correct place.\n")
  message("You can now close the program.")
  
} # End of function definition.


#### Definition of shiny app to use the above function ####
# Define UI for app ----
ui <- fluidPage(useShinyjs(),
  
  # . App title and description ----
  titlePanel("Mass balance model assistant"),
  
  
  checkboxInput("checkbox_show_help_text", "Show help text", FALSE),

  # . . Help text which can be shown by ticking a checkbox.
  conditionalPanel(condition = "input.checkbox_show_help_text == 1",
    h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
       em("This program generates the grids of DEM, surface type and radiation which are used in the glacier mass balance model.")),
    p(),
    h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
       em("As"), strong(" INPUT DATA "), em("please provide:")),
    tags$div(tags$ul(
      tags$li(em("the ", strong("glacier name "), "with no whitespaces")),
      tags$li(em("an ", strong("elevation grid "), "of the region of interest (for example .tif or .hgt, from EarthExplorer, SRTM, ASTER or any other)")),
      tags$li(em("a ", strong("glacier outline, "), "for example as shapefile (.shp)")),
      tags$li("(OPTIONAL): ", em("a shapefile with the ", strong("firn area"))),
      tags$li("(OPTIONAL): ", em("a shapefile with the ", strong("debris cover"))),
      tags$li("(OPTIONAL): ", em("a ", strong("reference grid file, to align"), " the output grids (useful to create input for multi-year simulations)")),
      tags$li("(OPTIONAL): ", em("the ", strong("margin distance"), " around the outline, in meters.", strong("This is ignored if you provide the reference grid file."))),
      style = "margin-top: 0px; margin-bottom: 5px; text-align: justify;")),
    p(),
    h5(style="text-align: justify; margin-top: 0px; margin-bottom: 5px;",
       em("As"), strong(" OUTPUT "), em("the model will create several grids:")),
    tags$div(tags$ul(
      tags$li(em("a ", strong("DHM"), " (altitude grid, as a full rectangle around the glacier)")),
      # tags$li(em("a ", strong("DEM"), " (altitude grid, only where there is ice, with no data outside the glacier)")),
      tags$li(em("a grid of ", strong("surface type"), " (rock/ice/firn/debris, important for albedo)")),
      tags$li("(OPTIONAL): ", em("365 grids of ", strong("daily potential solar radiation."))),
      style = "margin-top: 0px; margin-bottom: 5px;")),
    h5(style="text-align: justify; margin-top: 0px; margin-bottom: 30px; text-align: justify;",
       em("The ", strong("coordinate system"), " (UTM / WGS84) is adjusted automatically."))),
  
  # . UI layout below the help text ----
  p(),
  
  # .. Input: choose glacier name (with inline CSS modifier to have label and field on same row),
  # DEM, outline file, and (optionally) firn and debris shapefiles, as well as reference grid for alignment ----
  tags$head(
    tags$style(type="text/css", "#inline1 label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 10px; } 
                #inline1 .form-group { display: table-row;}"),
  tags$style(type="text/css", "#inline2 label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 22px; } 
                #inline2 .form-group { display: table-row;}")
  ),
  tags$div(id = "inline1", textInput("choose_glacier_name", "Choose glacier name:", placeholder = "Glacier name")),
  p(),
  tags$div(id = "inline2", numericInput("choose_model_year", "Choose model year:", value = NA, min = 0, max = 3000, step = 1)),
  p(),
  shinyFilesButton("choose_dem_file", strong("Choose input DEM file"), "Choose input DEM file", FALSE, style = "width: 60%;"),
  p(),
  shinyFilesButton("choose_shp_file", strong("Choose input glacier shapefile"), "Choose input glacier shapefile", FALSE, style = "width: 60%;"),
  p(),
  shinyFilesButton("choose_firn_file", strong("Choose input firn shapefile (optional)"), "Choose input firn shapefile (optional)", FALSE, style = "width: 60%;"),
  p(),
  shinyFilesButton("choose_debris_file", strong("Choose input debris shapefile (optional)"), "Choose input debris shapefile (optional)", FALSE, style = "width: 60%;"),
  p(),
  shinyFilesButton("choose_reference_file", strong("Choose reference grid file (optional)"), "Choose reference grid file (optional)", FALSE, style = "width: 60%; margin-bottom: 20px"),

  
  # .. Input: choose margin in meters ----
  numericInput(inputId = "buffersize",
               label = "Choose margin size around the outline, in meters:",
               value = 500,
               min = 1,
               max = 10000,
               width = "60%"),
  p(),
  
  # .. Input: should we also compute radiation files? ----
  checkboxInput("checkbox_compute_radiation", strong("Compute daily potential solar radiation (SLOW!)"), FALSE, width = "100%"),
  p(),
  
  # .. Text fields: show the full path of the chosen DEM and outline file ----
  htmlOutput("glaciername_chosen_string"),
  htmlOutput("modelyear_chosen_string"),
  htmlOutput("dem_chosen_string"),
  htmlOutput("shp_chosen_string"),
  htmlOutput("firn_chosen_string"),
  htmlOutput("debris_chosen_string"),
  htmlOutput("reference_chosen_string"),
  p(),
  
  # .. Input: do-it button ----
  actionButton(inputId = "startprocessing",
               label = strong("RUN!")),
  p()
  
)

# Define server logic to read selected file ----
server <- function(input, output, session) {

  volumes <- c(getVolumes()(), setNames(getwd(), basename(getwd())))
  shinyFileChoose(input, "choose_dem_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_shp_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_firn_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_debris_file", roots=volumes, session=session)
  shinyFileChoose(input, "choose_reference_file", roots=volumes, session=session)

  glaciername       <- reactive(input$choose_glacier_name)
  modelyear         <- reactive(input$choose_model_year)
  demfilepath       <- reactive(as.character(parseFilePaths(volumes, input$choose_dem_file)$datapath))
  shpfilepath       <- reactive(as.character(parseFilePaths(volumes, input$choose_shp_file)$datapath))
  firnfilepath      <- reactive(as.character(parseFilePaths(volumes, input$choose_firn_file)$datapath))
  debrisfilepath    <- reactive(as.character(parseFilePaths(volumes, input$choose_debris_file)$datapath))
  referencefilepath <- reactive(as.character(parseFilePaths(volumes, input$choose_reference_file)$datapath))
  
  # Disable "RUN!" button if the required input is missing.
  observe({
    toggleState("startprocessing", isTruthy(glaciername()) && isTruthy(modelyear()) && isTruthy(demfilepath()) && isTruthy(shpfilepath()))
  })
  
  # Show the user which input has been provided and which is still missing.
  output$glaciername_chosen_string <- renderText({
    ifelse(isTruthy(glaciername()),
           "<font color=\"#00C000\"><b>Glacier name selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Glacier name not yet selected.</b></font color>")
  })
  output$modelyear_chosen_string <- renderText({
    ifelse(isTruthy(modelyear()),
           "<font color=\"#00C000\"><b>Model year selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Model year not yet selected.</b></font color>")
  })
  output$dem_chosen_string <- renderText({
    ifelse(isTruthy(demfilepath()),
           "<font color=\"#00C000\"><b>Input DEM file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Input DEM file not yet selected.</b></font color>")
  })
  output$shp_chosen_string <- renderText({
    ifelse(isTruthy(shpfilepath()),
           "<font color=\"#00C000\"><b>Input outline file selected.</b></font color>",
           "<font color=\"#FF0000\"><b>Input outline file not yet selected.</b></font color>")
  })
  output$firn_chosen_string <- renderText({
    ifelse(isTruthy(firnfilepath()),
           "<font color=\"#00C000\"><b>Input firn shapefile selected.</b></font color>",
           "<font color=\"#FF8000\"><b>Input firn shapefile (optional) not yet selected.</b></font color>")
  })
  output$debris_chosen_string <- renderText({
    ifelse(isTruthy(debrisfilepath()),
           "<font color=\"#00C000\"><b>Input debris shapefile selected.</b></font color>",
           "<font color=\"#FF8000\"><b>Input debris shapefile (optional) not yet selected.</b></font color>")
  })
  output$reference_chosen_string <- renderText({
    ifelse(isTruthy(referencefilepath()),
           "<font color=\"#00C000\"><b>Input reference grid file selected. <i>NOTE: margin distance will be ignored.</i></b></font color>",
           "<font color=\"#FF8000\"><b>Input reference grid file (optional) not yet selected.</b></font color>")
  })

  
  # Button to start processing.
  observeEvent(input$startprocessing, {
    # These 4 below are probably not needed since the RUN! button is
    # disabled by shinyjs, but we keep them anyway since the make sense.
    req(input$choose_glacier_name)
    req(input$choose_model_year)
    req(input$choose_dem_file)
    req(input$choose_shp_file)
    firnfilepath_sel      <- ifelse(isTruthy(firnfilepath()), firnfilepath(), NA)
    debrisfilepath_sel    <- ifelse(isTruthy(debrisfilepath()), debrisfilepath(), NA)
    referencefilepath_sel <- ifelse(isTruthy(referencefilepath()), referencefilepath(), NA)
    showModal(modalDialog(h3("Processing... See RStudio console for progress."), footer=NULL))
    func_do_processing(demfilepath(), shpfilepath(), firnfilepath_sel, debrisfilepath_sel, referencefilepath_sel, input$buffersize, input$checkbox_compute_radiation, file.path("input", glaciername()))
    file.rename(file.path(getwd(), "input", glaciername(), "dhm", "dhm_glacier.tif"), file.path(getwd(), "input", glaciername(), "dhm", paste0("dhm_", glaciername(), "_", modelyear(), ".tif")))
    file.rename(file.path(getwd(), "input", glaciername(), "surftype", "surface_type_glacier.tif"), file.path(getwd(), "input", glaciername(), "surftype", paste0("surface_type_", glaciername(), "_", modelyear(), ".tif")))
    removeModal()
    showModal(modalDialog(h3("Processing finished. See RStudio console for details. You can now close the program."), footer=NULL))
  })
  
}
# Run the app.
shinyApp(ui, server)
