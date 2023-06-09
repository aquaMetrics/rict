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
    paste("RICT", packageVersion("rict")),
    tabPanel(
      "Predict & Classify",
      sidebarPanel(
        h4("This app is in TESTING"),
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
          "options", "All Indices",
          c(
            "Include" = "all_indices"
          ),
        ),
        checkboxGroupInput(
          "tl", "Predict Taxa Lists",
          c(
            "TL1" = "TL1",
            "TL2" = "TL2",
            "TL3" = "TL3",
            "TL4" = "TL4",
            "TL5" = "TL5"
          )
        ), p("Taxa predictions may take a few minutes to calculate")
      ),
      # Show tables
      mainPanel(
        htmlOutput("message"),
        leafletOutput("map"),
        p(),
        htmlOutput("tables")
      )
    ),
    tabPanel(
      "Compare",
      sidebarPanel(
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
        htmlOutput("message_compare"),
        htmlOutput("compare")
      )
    ),
    tabPanel(
      "Help",
      HTML('<a href="https://aquametrics.github.io/rict/articles/user-guide.html" target="_blank">User Guide</a>')
    ),
  )
)

# Define server logic ----------------------------------------------------------
server <- function(input, output) {
  output$message <- renderUI({
    inFile <- input$dataset
    if (is.null(inFile)) {
      return(HTML(
        '<h3 style="color:grey;">Upload a prepared .CSV input file or use the following:</h3></style>
        <h4 style="color:grey;">Template File</h4></style>
          <p><a href="https://github.com/aquaMetrics/rict/raw/master/inst/extdat/input-file-template.xls" target="_blank">Validation Spreadsheet</a></p>
          <h4 style="color:grey;">Example Input Files</h4></style>
          <p style="color:grey;">Open link, right-click and save as CSV: </p></style>
          <p><a href="https://raw.githubusercontent.com/aquaMetrics/rict/master/inst/extdat/new-input-file-data-to-use-multi-year-1.csv" target="_blank">Great Britain</a></p>
          <p><a href="https://raw.githubusercontent.com/aquaMetrics/rict/master/inst/extdat/ni-model-1-test-data.csv" target="_blank">Northern Ireland</a></p>
          <p><a href="https://raw.githubusercontent.com/aquaMetrics/rict/master/inst/extdat/input-file-data-to-use-multi-year-iom.csv" target="_blank">Isle of Man</a></p>
        <p><a href="https://raw.githubusercontent.com/aquaMetrics/rict/master/inst/extdat/environmental-test-data-model-44-log.csv" target="_blank">GIS variables for GB only (Model 44)</a></p>'
        ))
    }
  })

  output$message_compare <- renderUI({
    compare_two <- input$dataset_two
    if (is.null(compare_two)) {
      return(HTML('
          <h3 style="color:grey;">Upload prepared .CSV input files or use the following:</h3></style>
          <h4 style="color:grey;">Example Upstream/Downstream Input Files</h4></style>
          <p style="color:grey;">Open link, right-click and save as CSV: </p></style>
          <p><a href="https://raw.githubusercontent.com/aquaMetrics/rict/master/inst/extdat/compare-data/rict-input-1-upstream-compare.csv" target="_blank">Upstream</a></p>
          <p><a href="https://raw.githubusercontent.com/aquaMetrics/rict/master/inst/extdat/compare-data/rict-input-2-downstream-compare.csv" target="_blank">Downstream</a></p>'
      ))
    }
  })

  # 'Predict and Classify' tab outputs -----------------------------------------
  output$tables <- renderUI({
    inFile <- input$dataset
    if (is.null(inFile)) {
      return()
    }
    # Create a Progress object
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating", value = 1)
    data <- read.csv(inFile$datapath, check.names = FALSE)
    options(shiny.sanitize.errors = FALSE)
    validations <- rict_validate(data, stop_if_all_fail = FALSE)
    predictions <- rict_predict(data)
    predictions_table <- predictions
    # don't need to display all columns - some columns only used by some models
    predictions_table <- dplyr::select(
      predictions_table,
      SITE,
      WATERBODY,
      YEAR,
      SuitCode,
      SuitText,
      dplyr::everything(),
      -dplyr::contains("LATITUDE"),
      -dplyr::contains("LONGITUDE"),
      -dplyr::contains("LOG.ALTITUDE"),
      -dplyr::contains("LOG.DISTANCE.FROM.SOURCE"),
      -dplyr::contains("LOG.WIDTH"),
      -dplyr::contains("LOG.DEPTH"),
      -dplyr::contains("MEAN.SUBSTRATUM"),
      -dplyr::contains("DISCHARGE.CATEGORY"),
      -dplyr::contains("ALKALINITY"),
      -dplyr::contains("LOG.ALKALINITY"),
      -dplyr::contains("LOG.SLOPE"),
      -dplyr::contains("MEAN.AIR.TEMP"),
      -dplyr::contains("AIR.TEMP.RANGE"),
      -area,
      -dplyr::contains("belongs_to_end_grp"),
      -dplyr::contains("SEASON_ID"),
      -dplyr::starts_with("p")
    )
    predictions_table[colSums(!is.na(predictions_table)) > 0]
    predictions_table <- Filter(function(x) !(all(x=="")), predictions_table)
    predictions_table <- dplyr::mutate(
      predictions_table,
      dplyr::across(
        where(is.numeric),
        round, 2
      )
    )
    output_files <- list(predictions)
    results <- data.frame()
    if (!is.null(predictions) & input$output == "predict_classify") {
      results <- rict_classify(predictions,
        year_type = input$year_type
      )
    }
    classification_table <- results
    classification_table <-  classification_table[colSums(!is.na(classification_table)) > 0]
    classification_table <- Filter(function(x) !(all(x=="")), classification_table)
    classification_table <- dplyr::mutate(
      classification_table,
      dplyr::across(
        where(is.numeric),
        round, 2
      )
    )

    taxa <- data.frame()
    taxa_table <- taxa
    if (!is.null(predictions) & !is.null(input$tl)) {
      if(nrow(data) > 24) {
        stop("To allow good website performance, please limit to fewer than
        25 sites when predicting taxa.")
      }
      taxa <- rict_predict(data, taxa = TRUE, taxa_list = input$tl)
      if (is.null(taxa) && validations$area == "iom") {
        taxa <- data.frame("Note" =
                                   "Taxa predictions not available for Isle of Man model")
        taxa_table <- taxa
        } else {
        taxa$Season_Code <- as.numeric(taxa$Season_Code)
        taxa_table <- dplyr::arrange(taxa, NBN_Name, Season_Code)
        taxa_table <- dplyr::select(
          taxa_table,
          siteName,
          TL,
          Season_Code,
          NBN_Name,
          NBN_Code,
          Average_Numerical_Abundance,
          Prob_Occurrence
        )

        taxa_table <- dplyr::mutate(
          taxa_table,
          dplyr::across(
            where(is.numeric),
            round, 2
          )
        )
      }
    }

    indices <- data.frame()
    if (!is.null(predictions) & any(input$options %in% "all_indices")) {
      indices <- rict_predict(data, all_indices = T)
    }
    indices_table <- indices
    indices_table <-  indices_table[colSums(!is.na(indices_table)) > 0]
    indices_table <- Filter(function(x) !(all(x=="")), indices_table)
    # Don't need to display all columns - some columns only used by some models
    indices_table <- dplyr::select(
      indices_table,
      dplyr::contains("SITE"),
      dplyr::contains("YEAR"),
      dplyr::contains("WATERBODY"),
      dplyr::contains("SEASON"),
      dplyr::everything(),
      -dplyr::contains("SUM"),
      -dplyr::contains("AUT"),
      -dplyr::contains("SPR"),
      -dplyr::contains("LATITUDE"),
      -dplyr::contains("LONGITUDE"),
      -dplyr::contains("LOG.ALTITUDE"),
      -dplyr::contains("LOG.DISTANCE.FROM.SOURCE"),
      -dplyr::contains("LOG.WIDTH"),
      -dplyr::contains("LOG.DEPTH"),
      -dplyr::contains("MEAN.SUBSTRATUM"),
      -dplyr::contains("DISCHARGE.CATEGORY"),
      -dplyr::contains("ALKALINITY"),
      -dplyr::contains("LOG.ALKALINITY"),
      -dplyr::contains("LOG.SLOPE"),
      -dplyr::contains("MEAN.AIR.TEMP"),
      -dplyr::contains("AIR.TEMP.RANGE"),
      -dplyr::contains("belongs_to_end_grp"),
      -dplyr::contains("area"),
      -dplyr::starts_with("p"),
      -dplyr::starts_with("SuitCode")
    )
    indices_table <- dplyr::mutate(
      indices_table,
      dplyr::across(
        where(is.numeric),
        round, 2
      )
    )

    output_files <- list(
      predictions,
      results,
      taxa,
      indices,
      validations$checks
    )
    names(output_files) <- c(
      "predictions",
      "classification",
      "taxa",
      "indices",
      "validations"
    )

    output$download_file <- downloadHandler(
      filename = function() {
        paste0("rict-", packageVersion("rict"), "-output.zip")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        for (i in seq_along(output_files)) {
            path <- paste0(names(output_files)[i], ".csv")
            fs <- c(fs, path)
            write.csv(output_files[[i]], file = path, row.names = FALSE)
        }
        zip(zipfile = fname, files = fs)
      }
    )

    download_data <- renderUI({
      downloadButton("download_file", "Download Outputs")
    })

    # Use validations$data for map as IOM predictions don't have lat/lon
    map <- leaflet(validations$data)
    map <- addTiles(map)
    map <- addMarkers(map, ~LONGITUDE, ~LATITUDE, popup = ~ htmlEscape(SITE))
    output$map <- renderLeaflet(map)

    # Format validations depending if detected ---------------------------------
    if (nrow(validations$checks) != 0) {
      validation <- list(h3("Validations"), renderDataTable({
        validations$checks
      }))
    } else {
      validation <- HTML(
        '<h3>Validation</h3><h4 style="color:gray;">All input data valid <span style="color:green;">✓</span></h1></style>'
      )
    }
    # Format outputs depending on options selected ----------------------------
    if (!is.null(input$tl)) {
      if (validations$area == "iom") {
        taxa_output <- list(h3("Taxa"), h4("❌ Taxa predictions not available for Isle of Man"))
      } else {
        taxa_output <- list(h3("Taxa"), DT::renderDataTable(
          {
            taxa_table
          },
          rownames = FALSE
        ))
      }
    } else {
      taxa_output <- NULL
    }

    # Only display Classification header if option selected
    if (input$output == "predict_classify") {
      classification_ouput <- list(h3("Classification"), DT::renderDataTable(
        {
          classification_table
        },
        rownames = FALSE
      ))
    } else {
      classification_ouput <- NULL
    }

    # Only display Indices header if option selected
    if (any(input$options %in% "all_indices")) {
      indices_output <- list(h3("All Indices"), DT::renderDataTable(
        {
          indices_table
        },
        rownames = FALSE
      ))
    } else {
      indices_output <- NULL
    }

    return(list(
      download_data,
      validation,
      h3("Predictions"), DT::renderDataTable(
        {
          predictions_table
        },
        rownames = FALSE
      ),
      classification_ouput,
      indices_output,
      taxa_output
    ))
  })

  # Compare tab outputs --------------------------------------------------------
  output$compare <- renderUI({
    inFile_one <- input$dataset_one
    inFile_two <- input$dataset_two
    if (is.null(inFile_one) || is.null(inFile_two)) {
      return()
    }

    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating", value = 1)
    data_one <- read.csv(inFile_one$datapath, check.names = FALSE)
    data_two <- read.csv(inFile_two$datapath, check.names = FALSE)
    valid_one <- rict_validate(data_one)
    valid_two <- rict_validate(data_two)
    validations <- dplyr::bind_rows(valid_one$checks, valid_two$checks)
    data_one <- rict(data_one,
      store_eqrs = TRUE,
      year_type = input$year_type_compare
    )
    data_two <- rict(data_two,
      store_eqrs = TRUE,
      year_type = input$year_type_compare
    )
    compare <- rict_compare(results_a = data_one, results_b = data_two)
    compare <- dplyr::mutate(
      compare,
      dplyr::across(
        where(is.numeric),
        round, 2
      )
    )
    data_one <- NULL
    data_two <- NULL

    output_files <- list(
      compare,
      validations
    )
    names(output_files) <- c(
      "compare",
      "validations"
    )

    output$download_file <- downloadHandler(
      filename = function() {
        paste0("rict-", packageVersion("rict"), "-output.zip")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        for (i in seq_along(output_files)) {
            path <- paste0(names(output_files)[i], ".csv")
            fs <- c(fs, path)
            write.csv(output_files[[i]], file = path, row.names = FALSE)
        }
        zip(zipfile = fname, files = fs)
      }
    )

    download_data <- renderUI({
      downloadButton("download_file", "Download Outputs")
    })

    if (nrow(validations) != 0) {
      validation <- list(h3("Validations"), renderDataTable({
        validations
      }))
    } else {
      validation <- HTML(
        '<h3>Validation</h3><h4 style="color:gray;">All input data valid <span style="color:green;">✓</span></h1></style>'
      )
    }

    return(list(
      download_data,
      validation,
      h3("Compare"), renderDataTable({
        compare
      })
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
