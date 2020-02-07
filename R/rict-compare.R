#' Compare the Statistical Differences between Classifications Results
#'
#' The Compare function allows the user to assess whether there is a real
#' difference in EQR values and/or status class between a pair of results and/or
#' sites and/or time periods.
#'
#' By default detects if upstream / downstream comparison required. This is
#' indicate by 'a' sites not included in 'b' sites or detects. Else it detects
#' if sites are paired between both datasets. For instance, if comparing season
#' to season or year to year pairs.
#'
#' By default only two metrics will be compared - these will be whatever EQR
#' metrics are in the columns following the 'SITE' and 'RESULT' columns. Edit
#' the file to re-order which EQR metrics to compare. Column three should be
#' ASPT and column four NTAXA.
#'
#' @param results_a Data frame output from `rict(store_eqrs = T)`
#' @param results_b Data frame output from `rict(store_eqrs = T)`
#' @param list of eqr_metrics to compare, default is average year ASPT and NTAXA
#'   metric
#' @return Dataframe of compare results
#' @export
#' @examples
#' \dontrun{
#' results_a <- rict(demo_observed_values[1, ], store_eqrs = T, year_type = "single")
#' results_b <- rict(demo_gis_values[1, ], store_eqrs = T, year_type = "single", model = "gis")
#' compare <- rict_compare(results_a, results_b)
#' }
#'
rict_compare <- function(results_a = NULL, results_b = NULL,
                         eqr_metrics = c("AVG_NTAXA", "AVG_ASPT")) {
  # Create 'result' ID
  results_a$RESULT <- paste(results_a$SITE, results_a$YEAR)
  results_b$RESULT <- paste(results_b$SITE, results_b$YEAR)

  # Arrange datasets so matching sites/results will line up if present
  results_a <- dplyr::arrange(results_a, RESULT)
  results_b <- dplyr::arrange(results_b, RESULT)

  # Convert to character because of potentially different factor levels
  results_a$SITE <- as.character(results_a$SITE)
  results_b$SITE <- as.character(results_b$SITE)

  # Bind first four columns from datasets. By default only two metrics will be
  # compared - these will be whatever EQR metrics are in the columns following
  # the 'SITE' and 'RESULT' columns. Edit the file to re-order which EQR metrics
  # to compare. Column three should be ASPT and column four NTAXA
  if (is.null(eqr_metrics)) {
    # Default to first four columns from each
    data <- dplyr::bind_rows(
      results_a[, 1:4],
      results_b[, 1:4]
    )
  } else {
    columns <- c("SITE", "RESULT", eqr_metrics)
    data <- dplyr::bind_rows(
      results_a[, columns],
      results_b[, columns]
    )
    names(data) <- columns
  }

  ## Select which results to compare -------------------------------------------------
  # Detect if upstream / downstream comparison required. This is indicate by 'a'
  # sites not included in 'b' sites
  if (any(unique(results_a$SITE) != unique(results_b$SITE))) {
    comparisons <- compare(data, a = results_a$RESULT, b = results_b$RESULT)
  }
  # Detect if sites are paired between both datasets. For instance, if comparing
  # season to season or year to year pairs.
  else if (results_a$SITE == results_b$SITE) {
    # Loop through all rows in 'a' comparing to matching result in 'b'
    comparisons <- purrr::map_df(nrow(results_a), function(a) {
      compare(data[c(a, length(a) + a), ],
        a = data$RESULT[a],
        b = data$RESULT[length(results_a$RESULT) + a]
      )
    })
  } else {
    message("You provided two datasets with matching SITE(s)
 but differing number of rows. We expect datasets that share matching
 SITE(S) to have matching number of rows.")
  }
  return(comparisons)
}