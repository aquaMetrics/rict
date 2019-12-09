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
ui <- fluidPage(
  # Application title
  titlePanel("RICT"),
  h4("River Invertebrate Classification Tool"),

  # Sidebar input
  sidebarLayout(
    sidebarPanel(
      h3("Options"),
      radioButtons(
        "model", "Model Type:",
        c(
          "GIS - Geology, Catchment area etc" = "gis",
          "Physcial - Slope, depth, discharge etc" = "physical"
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
  )
)

# Define server logic
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
    predictions_table <- predictions[, c(
      "SITE", "YEAR",
      "TL2_WHPT_NTAXA_AbW_DistFam_spr", "TL2_WHPT_ASPT_AbW_DistFam_spr",
      "TL2_WHPT_NTAXA_AbW_DistFam_aut", "TL2_WHPT_ASPT_AbW_DistFam_aut",
      "TL2_WHPT_NTAXA_AbW_DistFam_sum", "TL2_WHPT_ASPT_AbW_DistFam_sum"
    )]

    if (!is.null(predictions)) {
      results <- rict_classify(predictions,
        year_type = input$year_type
      )
    }
    classification_table <- results[, c(
      "SITE", "YEAR",
      "mintawhpt_spr_aut_mostProb_MINTA_"
    )]

    files <- list(predictions, results)
    output$download_file <- downloadHandler(
      filename = function() {
        paste("rict-output", "zip", sep = ".")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        for (i in 1:seq_len(files)) {
          path <- paste0("sample_", i, ".csv")
          fs <- c(fs, path)
          write.csv(files[[i]], file = path)
        }
        zip(zipfile = fname, files = fs)
      }
    )

    download_data <- renderUI({
      downloadButton("download_file", "Data Download")
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
