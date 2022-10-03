#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shiny)
library(leaflet)
library(rict)
library(htmltools)
library(DT)

# Define UI for application

ui <- tagList(
  #  shinythemes::themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "RICT",
    tabPanel(
      "Predict & Classify",
      sidebarPanel(
        h4("This app is a work in progress -
                      use the following for official uses: "),
        a("Azure Experiments",
          href = "https://www.fba.org.uk/FBA/Public/Discover-and-Learn/Projects/RICT%20Application.aspx"
        ),
        p(),
        fileInput("dataset", "Choose CSV input file",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        h4("Options"),
        radioButtons(
          "year_type", "Year Type",
          c(
            "Single-year" = "single",
            "Multi-year" = "multi"
          )
        ),
        radioButtons(
          "output", "Outputs",
          c(
            "Prediction & Classification" = "predict_classify",
            "Prediction Only" = "predict"
          )
        ),
        checkboxGroupInput(
          "include", "Include",
          c(
            "Don't include taxa or predictions" = "none",
            "Include Taxa Prediction" = "taxa",
            "All Indices" = "all_indices"
          ),
          selected = "none"
        ),
        checkboxGroupInput(
          "tl", "Taxa Lists",
          c(
            "TL1" = "TL1",
            "TL2" = "TL2",
            "TL3" = "TL3",
            "TL4" = "TL4",
            "TL5" = "TL5"
          ),
          selected = "TL2"
        )
      ),
      # Show tables
      mainPanel(
        leafletOutput("map"),
        p(),
        htmlOutput("tables")
      )
    ),
    tabPanel(
      "Compare",
      sidebarPanel(
        h4("This app is a work in progress -
                      use the following for official uses: "),
        a("Azure Experiments",
          href = "https://www.fba.org.uk/FBA/Public/Discover-and-Learn/Projects/RICT%20Application.aspx"
        ),
        p(),
        fileInput("dataset_one", "Choose CSV input file 1",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        fileInput("dataset_two", "Choose CSV input file 2",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        h4("Options"),
        radioButtons(
          "year_type_compare", "Year Type",
          c(
            "Single-year" = "single",
            "Multi-year" = "multi"
          )
        )
      ),
      # Show tables
      mainPanel(
        htmlOutput("compare")
      )
    )
  )
)

# Define server logic ------------------------------------------------------------------
server <- function(input, output) {
  output$tables <- renderUI({
    inFile <- input$dataset
    if (is.null(inFile)) {
      return(HTML('<h1 style="color:lightgrey;">Please choose .csv file...</h1></style>'))
    }
    # Create a Progress object
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating", value = 1)
    data <- read.csv(inFile$datapath, check.names = FALSE)
    validations <- rict_validate(data)
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
      taxa <- rict_predict(data, taxa = TRUE, taxa_list = input$tl)
    }
    taxa_table <- taxa


    indices <- data.frame()
    if (!is.null(predictions) & any(input$include %in% "all_indices")) {
      indices <- rict_predict(data, all_indices = TRUE)
    }
    indices_table <- indices

    output_files <- list(predictions, results, taxa, indices)
    names(output_files) <- c("predictions", "classification", "taxa", "indices")

    output$download_file <- downloadHandler(
      filename = function() {
        paste0("rict-", packageVersion("rict"), "-output.zip")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        for (i in seq_along(output_files)) {
          if (nrow(output_files[[i]] > 0)) {
            path <- paste0(names(output_files)[i], ".csv")
            fs <- c(fs, path)
            write.csv(output_files[[i]], file = path)
          }
        }
        zip(zipfile = fname, files = fs)
      }
    )

    download_data <- renderUI({
      downloadButton("download_file", "Download Outputs")
    })

    map <- leaflet(predictions)
    map <- addTiles(map)
    map <- addMarkers(map, ~LONGITUDE, ~LATITUDE, popup = ~ htmlEscape(SITE))
    output$map <- renderLeaflet(map)

    if (nrow(validations$checks) != 0) {
      validation <- list(h3("Validations"), renderDataTable({
        validations$checks
      }))
    } else {
      validation <- HTML('<h3>Validation</h3><h4 style="color:lightgray;">All input data valid</h1></style>')
    }

    return(list(
      download_data,
      validation,
      h3("Predictions"), renderDataTable({
        predictions_table
      }),
      h3("Classification"), renderDataTable({
        classification_table
      }),
      h3("Taxa"), renderDataTable({
        taxa_table
      }),
      h3("All Indices"), renderDataTable({
        indices_table
      })
    ))
  })

  output$compare <- renderUI({
    inFile_one <- input$dataset_one
    inFile_two <- input$dataset_two
    if (is.null(inFile_one) || is.null(inFile_two)) {
      return(HTML('<h1 style="color:lightgrey;">Please choose .csv file...</h1></style>'))
    }

    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating", value = 1)
    data_one <- read.csv(inFile_one$datapath, check.names = FALSE)
    data_two <- read.csv(inFile_two$datapath, check.names = FALSE)
    data_one <- rict(data_one, store_eqrs = TRUE, year_type = input$year_type_compare)
    data_two <- rict(data_two, store_eqrs = TRUE, year_type = input$year_type_compare)
    compare <- rict_compare(results_a = data_one, results_b = data_two)
    compare <- compare
    return(list(
      h3("Compare"), renderDataTable({
        compare
      })
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
