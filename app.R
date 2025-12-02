library(shiny)
library(leaflet)
library(terra)
library(sf)
library(leaflet.extras)

# ---------------------------
# Predefined raster paths (server-side)
# ---------------------------
ndvi_path <- "data/NDVI.tif"
b11_path  <- "data/B11.tif"
lulc_path <- "data/LULC_Sat.tif"

# Load full rasters once at server start
ndvi_full <- rast(ndvi_path)
b11_full  <- rast(b11_path)
lulc_full <- rast(lulc_path)

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Potential denitrification Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Your Area on the map"),
      helpText("Draw a rectangle on the map to define the area for PDR computation."),
      numericInput("class1", "Intercept for LULC class 1", 0.3983),
      numericInput("class2", "Intercept for LULC class 2", -0.2820),
      numericInput("class3", "Intercept for LULC class 3", 0.0437),
      numericInput("class4", "Intercept for LULC class 4", -0.1599),
      numericInput("class5", "Intercept for LULC class 5", 0.1230),
      actionButton("compute", "Compute Dentifrication for Selected Area")
    ),
    
    mainPanel(
      leafletOutput("map", height = 500),
      plotOutput("pdr_plot", height = 300),
      plotOutput("pdr_hist", height = 300)
    )
  )
)

# ---------------------------
# ---------------------------
server <- function(input, output, session) {
  
  # Initialize leaflet map(for initialize viewing map)
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = 90.3563, lat = 23.6850, zoom = 7) %>%
      addDrawToolbar(
        targetGroup = "draw",
        rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(color = "red")),
        editOptions = editToolbarOptions()
      )
  })
  
  # user Reactive: user-drawn area on demand on leaflet map
  aoi <- reactiveVal(NULL)
  
  # Capture draw events(when someone draw a polygon)
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    coords <- feat$geometry$coordinates[[1]]
    # Create sf polygon(its a geometry format)
    poly <- st_polygon(list(do.call(rbind, lapply(coords, function(x) c(x[[1]], x[[2]]))))) %>%
      st_sfc(crs = 4326)
    aoi(poly)
  })
  
  # Compute PDR on AOI
  observeEvent(input$compute, {
    req(aoi())
    poly <- st_transform(aoi(), crs(ndvi_full))  # Transform AOI to raster CRS
    
    #this part for pop-up warning
    # Convert raster extent to sf polygon geometry
    raster_extent <- st_as_sfc(st_bbox(ndvi_full))
    
    # Check intersection for selected area on map
    if (!st_intersects(poly, raster_extent, sparse = FALSE)[1,1]) {
      showModal(modalDialog(
        title = "Warning",
        "The selected area is outside the raster extent. Please select a valid area.",
        easyClose = TRUE,
        footer = NULL
      ))
      
      # Reset plots
      output$pdr_plot <- renderPlot({
        plot(0, type="n", axes=FALSE, xlab="", ylab="", main="No data in selected area")
      })
      output$pdr_hist <- renderPlot({
        plot(0, type="n", axes=FALSE, xlab="", ylab="", main="No data")
      })
      
      return()  # stop processing
    }
    
    
    # Crop rasters to AOI
    ndvi <- crop(ndvi_full, vect(poly))
    b11  <- crop(b11_full, vect(poly))
    lulc <- crop(lulc_full, vect(poly))
    
    
    
    
    # Align resolutions / CRS
    if (!all(ext(ndvi) == ext(lulc)) || !all(res(ndvi) == res(lulc))) {
      lulc <- resample(lulc, ndvi, method = "near")
    }
    if (!all(ext(ndvi) == ext(b11)) || !all(res(ndvi) == res(b11))) {
      b11 <- resample(b11, ndvi, method = "bilinear")
    }
    
    # LULC intercept raster
    rcl <- matrix(c(
      1, input$class1,
      2, input$class2,
      3, input$class3,
      4, input$class4,
      5, input$class5
    ), ncol = 2, byrow = TRUE)
    
    intc_r <- classify(lulc, rcl, others = NA)
    
    # PDR calculation
    exp_term <- (-1.357) + intc_r + 1.264 * ndvi - 2.271 * b11
    PDR <- 240 * (10 ^ exp_term)
    PDR_masked <- mask(PDR, ndvi)
    
    # Store reactive PDR
    output$pdr_plot <- renderPlot({
      plot(PDR_masked, main = "PDR (Denitrification Rate)", col = hcl.colors(50, "Viridis"))
    })
    
    output$pdr_hist <- renderPlot({
      hist(values(PDR_masked), main = "PDR Distribution", xlab = "PDR", breaks = 50)
    })
    
    # Optionally, store for download/export
    session$userData$PDR <- PDR_masked
  })
}

# ---------------------------
# Run App
# ---------------------------
shinyApp(ui = ui, server = server)
