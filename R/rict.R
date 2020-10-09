#' Run the River Invertebrate Classification Tool (RICT)
#'
#' The \code{rict} function classifies observed environmental values.
#' @seealso \code{\link{rict_predict}} to run prediction (not classification)
#' @param data Dataframe of observed environmental values for example of input
#'   data see \code{\link{demo_observed_values}} or
#'   \code{\link{demo_gis_values_log}}.
#'  \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'   \item{...}{...}
#'    }
#' @param year_type "single" or "multi" depending if multi-year classification
#'   required - default is "multi".
#' @param store_eqrs TRUE/FALSE if set to TRUE function will return simulate
#'   EQRs.
#' @return Dataframe of classification results (and simulated EQRs if store_eqrs
#'   = TRUE).
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' results <- rict(demo_observed_values)
#' }
rict <- function(data = NULL, year_type = "multi", store_eqrs = FALSE) {
  predictions <- rict_predict(data)
  results <- rict_classify(predictions, year_type, store_eqrs = store_eqrs)
  return(results)
}
