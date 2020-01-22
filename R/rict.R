#' Run the River Invertebrate Classification Tool (RICT)
#'
#' The \code{rict} function classifies observed environmental values.
#' @param data Dataframe of observed environmental values
#' \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'   ...
#' }
#' @param model Model to use either 'gis' or 'physical'
#' @param area Area of UK, either 'ni' (Northern Ireland) or 'gb' (Great Britain)
#' @param year_type "single" or "multi" depending if multi-year classification
#'   required - default is "multi".
#' @return Dataframe of classification results
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' results <- rict(demo_observed_values)
#' }
rict <- function(data = NULL, model = "physical", area = "gb", year_type = "multi") {
  predictions <- rict_predict(data, model = model, area = area)
  results <- rict_classify(predictions, year_type)
  return(results)
}
