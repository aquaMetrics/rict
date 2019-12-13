#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shiny)
library(rict)
library(leaflet)
library(htmltools)

# Define UI for application

  ui <- tagList(
  #  shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "RICT",
      tabPanel("Predict & Classify",
               sidebarPanel(
      h3("Options"),
      radioButtons(
        "model", "Model Type:",
        c(
          "Physcial - Slope, depth, width..." = "physical",
          "GIS - Geology, catchment area..." = "gis"
        )
      ),
      radioButtons(
        "year_type", "Year Type:",
        c(
          "Single-year" = "single",
          "Multi-year" = "multi"
        )
      ),
      radioButtons(
        "area", "Area:",
        c(
          "Great Britain" = "gb",
          "Northern Ireland" = "ni"
        )
      ),
      radioButtons(
        "output", "Output:",
        c(
          "Prediction & Classification" = "predict_classify",
          "Prediction only" = "predict"
        )
      ),
      fileInput("dataset", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    ),
    # Show tables
    mainPanel(
      leafletOutput("map"),
      p(),
      htmlOutput("tables")
    )
  ),
  tabPanel("GIS variables", "This panel is intentionally left blank. This will be for future GIS variables map")
)
)

# Define server logic ------------------------------------------------------------------
server <- function(input, output) {
  output$tables <- renderUI({
    inFile <- input$dataset
    if (is.null(inFile)) {
      return(NULL)
    }
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating", value = 1)
    data <- read.csv(inFile$datapath, check.names = F)
    predictions <- rict_predict(data, model = input$model, area = input$area)

    predictions_table <- predictions
    # [, c(
    #   "SITE", "YEAR",
    #   "TL2_WHPT_NTAXA_AbW_DistFam_spr", "TL2_WHPT_ASPT_AbW_DistFam_spr",
    #   "TL2_WHPT_NTAXA_AbW_DistFam_aut", "TL2_WHPT_ASPT_AbW_DistFam_aut",
    #   "TL2_WHPT_NTAXA_AbW_DistFam_sum", "TL2_WHPT_ASPT_AbW_DistFam_sum"
    # )]

    output_files <- list(predictions)
    classification_table <- data.frame()
    if (!is.null(predictions) & input$output == "predict_classify") {
      results <- rict_classify(predictions,
        year_type = input$year_type
      )
      classification_table <- results
      # [, c(
      #   "SITE", "YEAR",
      #   "mintawhpt_spr_aut_mostProb_MINTA_"
      # )]

      output_files <- list(predictions, results)
    }

    output$download_file <- downloadHandler(
      filename = function() {
        paste("rict-output", "zip", sep = ".")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        for (i in seq_along(output_files)) {
          path <- paste0("output_", i, ".csv")
          fs <- c(fs, path)
          write.csv(output_files[[i]], file = path)
        }
        zip(zipfile = fname, files = fs)
      }
    )

    download_data <- renderUI({
      downloadButton("download_file", "Download Outputs")
    })

    map <- leaflet(predictions) %>%
             addTiles() %>%
              addMarkers(~LONGITUDE, ~LATITUDE, popup = ~ htmlEscape(SITE))

    output$map <- renderLeaflet(map)

    return(list(
      download_data,
      h3("Predictions"), DT::renderDataTable({
        predictions_table
      }),
      h3("Classification"), DT::renderDataTable({
        classification_table
      })
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
