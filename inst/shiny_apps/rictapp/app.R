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
                   h4("This app is a work in progress -
                      use the following for official uses: "),
                   a("Azure Experiments", href="https://www.fba.org.uk/FBA/Public/Discover-and-Learn/Projects/RICT%20Application.aspx"),
                   p(),
                   fileInput("dataset", "Choose CSV File",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"
                             )
                   ),
                    h3("Options"),
      radioButtons(
        "year_type", "Year Type:",
        c(
          "Single-year" = "single",
          "Multi-year" = "multi"
        )
      ),
      radioButtons(
        "output", "Output:",
        c(
          "Prediction & Classification" = "predict_classify",
          "Prediction Only" = "predict"
        )
      ),
      checkboxGroupInput(
        "include", "Include: ",
        c(
          "Don't include taxa or predictions" = "none",
          "Include Taxa Prediction" = "taxa",
          "All Indices" = "all_indices"
        ), selected = "none"
      ),
      checkboxGroupInput(
        "tl", "Taxa Lists: ",
        c(
          "TL1" = "TL1",
          "TL2" = "TL2",
          "TL3" = "TL3",
          "TL4" = "TL4",
          "TL5" = "TL5"
        ), selected = "TL2"
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
    predictions <- rict_predict(data)
    predictions_table <- predictions

    output_files <- list(predictions)
    results <- data.frame()
    if (!is.null(predictions) & input$output == "predict_classify") {
      results <- rict_classify(predictions,
        year_type = input$year_type
      )
    }
    classification_table <- results


    taxa <- data.frame()
    if (!is.null(predictions) & any(input$include %in% "taxa")) {
      taxa <- rict_predict(data, taxa = T, taxa_list = input$tl)
    }
    taxa_table <- taxa


    indices <- data.frame()
    if (!is.null(predictions) & any(input$include %in% "all_indices")) {
      indices <- rict_predict(data, all_indices = T)
    }
    indices_table <- indices

    output_files <- list(predictions, results, taxa, indices)


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
      }),
      h3("Taxa"), DT::renderDataTable({
        taxa_table
      }),
      h3("All Indices"), DT::renderDataTable({
        indices_table
      })
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
