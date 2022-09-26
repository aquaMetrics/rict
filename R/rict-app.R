#' Open RICT web app
#'
#' Open RICT as an interactive shiny web app.
#'
#' @details
#' \code{rict_app()} opens RICT as an interactive shiny app. RICT will
#' automatically detect if the input file contains NI or GB grid references and
#' apply the correct model including if GIS / geological predictors are used
#' (currently only available for GB).
#'
#' Using the app:
#' \enumerate{
#'  \item Select your required year type (multi/single) for classification and
#'  type of prediction for instance include taxa or all indices.
#'  \item Click the Browse for .csv file button to select and upload a data file.
#'  \item View and download results.
#'  }
#'
#' @examples
#' \dontrun{
#' rict_app()
#' }
#'
#' @export rict_app

rict_app <- function() {
  message("This app is a work in progress - use the rict experiments on Azure for officialclassification:
          https://www.fba.org.uk/FBA/Public/Discover-and-Learn/Projects/RICT%20Application.aspx")
  appDir <- system.file("shiny_apps", "rictapp", package = "rict")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `rict`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
