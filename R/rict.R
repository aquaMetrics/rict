#' Run the River Invertebrate Classification Tool (RICT)
#'
#' The \code{rict} function classifies observed environmental values.
#' @param data Dataframe of observed environmental values
#'  \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'    }
#' @param model Model to use either 'gis' or 'physical'
#' @param area Area of UK, either 'ni' (Northern Ireland) or 'gb' (Great
#'   Britain)
#' @param year_type "single" or "multi" depending if multi-year classification
#'   required - default is "multi".
#' @param store_eqrs TRUE/FALSE if set to TRUE function will return simulate
#'   EQRs only
#' @return Dataframe of classification results (or simulated EQRs if store_eqrs
#'   = TRUE)
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' results <- rict(demo_observed_values)
#' }
rict <- function(data = NULL, model = "physical", area = "gb",
                 year_type = "multi", store_eqrs = F) {
  predictions <- rict_predict(data, model = model, area = area)
  results <- rict_classify(predictions, year_type, store_eqrs = store_eqrs, area = area)
  return(results)
}
