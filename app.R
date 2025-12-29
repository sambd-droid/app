# app.R
library(shiny)
library(terra)

# Posit-friendly upload limit
options(shiny.maxRequestSize = 150 * 1024^2)  # 100 MB

# ----------------------------------------
# Helper: pretty bytes
# ----------------------------------------
fmt_bytes <- function(b) {
  if (is.na(b)) return("NA")
  units <- c("B","KB","MB","GB")
  p <- pmax(1, floor(log(b, 1024)))
  sprintf("%.1f %s", b / (1024 ^ (p - 1)), units[p])
}

# ----------------------------------------
# UI
# ----------------------------------------
ui <- fluidPage(

  tags$head(
    tags$style(HTML("\n      .sidebar-panel {background:#df602b; padding: 12px; }\n      .main-panel { padding: 12px;background:#e6ddd6; }\n      .app-title { font-weight: 700; font-size: 30px;color:#df602b; }\n    "))
  ),
  
  # App header design
  div(class = "container",
      div(class = "row mb-3",
          div(class = "col-12",
              span(class = "app-title", "Potential Denitrification Rate Visualization - Manual Model"),br(), hr(),
             
          )
      )
  ),
 
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Upload raster image file."),br(),
                 fileInput("ndvi", "Upload NDVI.tif", accept = c(".tif", ".tiff")),
                 fileInput("b11",  "Upload B11.tif",  accept = c(".tif", ".tiff")),
                 fileInput("lulc", "Upload LULC_Sat.tif", accept = c(".tif", ".tiff")),
             
                 actionButton("run", "Run Model", class = "btn-primary",style="color:black;background:#78cee1;"),
                 hr(),downloadButton("download_plot", "Download Plot (PNG)"),hr(),

                 downloadButton("download_csv", "Download PDR CSV"),
   
                 hr(),
                 verbatimTextOutput("status_small")
    ),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Plots",
                         fluidRow(
                           column(6, plotOutput("pdr_plot", height = 300)),
                           column(6, plotOutput("pdr_hist", height = 300))
                         ),
                         fluidRow(
                           column(12, verbatimTextOutput("stats"))
                         )
                ),
                tabPanel("Logs / Info",
                         verbatimTextOutput("log_text")
                )
              )
    )
  )
)

# ----------------------------------------
# SERVER
# ----------------------------------------
server <- function(input, output, session) {
  
  result_rast <- reactiveVal(NULL)
  last_info   <- reactiveVal(NULL)
  log_lines   <- reactiveVal(character())
  
  add_log <- function(txt) {
    log_lines(c(log_lines(), paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", txt)))
  }
  
  observeEvent(input$run, {
    
    result_rast(NULL)
    last_info(NULL)
    log_lines(character())
    
    # ---- Validation ----
    if (is.null(input$ndvi) || is.null(input$b11) || is.null(input$lulc)) {
      showNotification("Please upload NDVI, B11, and LULC rasters.", type = "error")
      return(NULL)
    }
    
    withProgress(message = "Running PDR model...", value = 0, {
      
      # ---- File paths ----
      ndvi_path <- input$ndvi$datapath
      b11_path  <- input$b11$datapath
      lulc_path <- input$lulc$datapath
      
      # ---- File sizes ----
      finfo <- list(
        ndvi_size = file.info(ndvi_path)$size,
        b11_size  = file.info(b11_path)$size,
        lulc_size = file.info(lulc_path)$size
      )
      last_info(finfo)
      
      add_log("Files uploaded successfully")
      incProgress(0.3, detail = "Files uploaded successfully")
      
      # ---- Load rasters ----
      ndvi <- rast(ndvi_path)
      b11  <- rast(b11_path)
      lulc <- rast(lulc_path)
      
      add_log("Rasters loaded")
      
      
      # ---- Downscale if large ----
      downscale_factor <- 1L
      if (any(unlist(finfo) > 80 * 1024^2, na.rm = TRUE)) {
        downscale_factor <- 2L
      }
      
      if (downscale_factor > 1) {
        add_log("Downscaling rasters by factor 2 (memory safety)")
        ndvi <- aggregate(ndvi, fact = 2, fun = "mean",
                          filename = tempfile(fileext = ".tif"), overwrite = TRUE)
        b11  <- aggregate(b11,  fact = 2, fun = "mean",
                          filename = tempfile(fileext = ".tif"), overwrite = TRUE)
        
      }
      
      # ---- CRS alignment ----
      if (!crs(ndvi) == crs(b11)) {
        add_log("Reprojecting B11 to NDVI CRS")
        b11 <- project(b11, ndvi, method = "bilinear")
      }
      if (!crs(ndvi) == crs(lulc)) {
        add_log("Reprojecting LULC to NDVI CRS")
        lulc <- project(lulc, ndvi, method = "near")
      }
      incProgress(0.1, detail = "Reprojecting LULC to NDVI")
      
      # ---- Resample ----
      b11  <- resample(b11, ndvi, method = "bilinear",
                       filename = tempfile(fileext = ".tif"), overwrite = TRUE)
      lulc <- resample(lulc, ndvi, method = "near",
                       filename = tempfile(fileext = ".tif"), overwrite = TRUE)
      
      add_log("Resampling completed")
     
      
      # ---- LULC intercepts ----
      rcl <- matrix(c(
        1,  0.3983,
        2, -0.2820,
        3,  0.0437,
        4, -0.1599,
        5,  0.1230
      ), ncol = 2, byrow = TRUE)
      
      intc <- classify(lulc, rcl, others = NA,
                       filename = tempfile(fileext = ".tif"), overwrite = TRUE)
      
      add_log("LULC intercepts applied")
      incProgress(0.2, detail = "LULC intercepts applied")
      
      # ---- PDR computation ----
      exp_term <- (-1.357) + intc + 1.264 * ndvi - 2.271 * b11
      
      pdr <- writeRaster(
        240 * (10 ^ exp_term),
        filename = tempfile(fileext = ".tif"),
        overwrite = TRUE
      )
      incProgress(0.2, detail = "Applying PDR Formula")
      
      pdr <- mask(pdr, ndvi,
                  filename = tempfile(fileext = ".tif"), overwrite = TRUE)
      
      add_log("PDR computation finished")
      incProgress(0.2, detail = "PDR computation finished")
      
      result_rast(pdr)
      add_log("Model completed successfully")
    })
  })
  
  # ----------------------------------------
  # Outputs
  # ----------------------------------------
  output$status_small <- renderText({
    fi <- last_info()
    if (is.null(fi)) return("No files uploaded yet.")
    paste0(
      "NDVI size: ", fmt_bytes(fi$ndvi_size), "\n",
      "B11 size:  ", fmt_bytes(fi$b11_size),  "\n",
      "LULC size: ", fmt_bytes(fi$lulc_size), "\n",
      "Large rasters are auto-downscaled for memory safety."
    )
  })
  
  output$log_text <- renderText({
    paste(log_lines(), collapse = "\n")
  })
  
  output$pdr_plot <- renderPlot({
    req(result_rast())
    plot(result_rast(),
         main = "Denitrification Rate (mg N2O-N / m² / h)",
         col = hcl.colors(50, "Viridis"))
  })
  
  output$pdr_hist <- renderPlot({
    req(result_rast())
    v <- values(result_rast(), na.rm = TRUE)
    hist(v, breaks = 50, main = "PDR Distribution", xlab = "PDR")
  })
  
  output$stats <- renderText({
    req(result_rast())
    v <- values(result_rast())
    v <- v[!is.na(v) & is.finite(v)]
    sprintf(
      "PDR stats (n=%d): mean=%.3f | sd=%.3f | min=%.3f | max=%.3f",
      length(v), mean(v), sd(v), min(v), max(v)
    )
  })
  
  # ---- Downloads ----
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("PDR_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(result_rast())
      
      png(
        filename = file,
        width = 1200,
        height = 800,
        res = 150
      )
      
      plot(
        result_rast(),
        main = "Denitrification Rate (mg N2O-N / m² / h)",
        col = hcl.colors(50, "Viridis")
      )
      
      dev.off()
    }
  )
  
   
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("PDR_xy_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- as.data.frame(result_rast(), xy = TRUE, na.rm = TRUE)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  session$onSessionEnded(function() {
    gc()
  })
}

shinyApp(ui, server)
