#' Compare the Statistical Differences between Classifications Results
#'
#' The Compare function allows the user to assess whether there is a real
#' difference in EQR values and/or status class between a pair of results and/or
#' sites and/or time periods.
#'
#' By default detects if upstream / downstream comparison required. This is
#' indicate by 'a' sites not included in 'b' sites or else it detects
#' if sites are paired between both datasets. For instance, if comparing season
#' to season or year to year pairs.
#'
#'
#' @param results_a Data frame output from `rict(store_eqrs = T)`.
#' @param results_b Data frame output from `rict(store_eqrs = T)`.
#' @param eqr_metrics List of eqr_metrics to compare, default is average year ASPT and NTAXA
#' metric.
#' @return Dataframe of compare results with 44 variables see `technical
#' specifications` for details:
#' \describe{
#'   \item{EQR metric Result A}{EQR metric from Result A which is being compared}
#'   \item{EQR metric Result B}{EQR metric from Result B which is being compared}
#'   \item{Result A}{Concatennated results name of Year and Site}
#'   \item{Result B}{Concatennated results name of Year and Site}
#'   \item{Average EQR for Result A}{Average EQR}
#'   \item{Average EQR for Result B}{Average EQR}
#'   \item{Average EQR: Difference (B - A)}{Difference in EQR}
#'   \item{Standard Deviation of Difference}{Standard Deviation of Difference}
#'   \item{Lower 95\% (L95) of Difference}{Lower 95\% (L95) of Difference}
#'   \item{Upper 95\% (U95) of Difference}{Upper 95\% (U95) of Difference}
#'   \item{2-sided test probability p of No difference in EQR}{p of probability test}
#'   \item{Most Probable Class for Result A}{Most Probable Class for Result A}
#'   \item{Probability of most likely class for Result A}{Probability of most likely class for Result A}
#'   \item{Most Probable Class for Result B}{Most Probable Class for Result B}
#'   \item{Probability of most likely class for Result B}{Probability of most likely class for Result B}
#'   \item{Probability B more than one class Worse than A}{Probability B more than one class Worse than A}
#'   \item{Probability B one class Worse than A}{Probability B one class Worse than A}
#'   \item{Probability B same Class as A}{Probability B same Class as A}
#'   \item{Probability B one class Better than A}{Probability B one class Better than A}
#'   \item{Probability B more than one class Better than A}{Probability B more than one class Better than A}
#'   \item{Probability Result A in High & Result B in High}{Probability...}
#'   \item{Probability Result A in High & Result B in Good}{Probability...}
#'   \item{Probability Result A in High & Result B in Moderate}{Probability...}
#'   \item{Probability Result A in High & Result B in Poor}{Probability...}
#'   \item{Probability Result A in High & Result B in Bad}{Probability...}
#'   \item{Probability Result A in Good & Result B in High}{Probability...}
#'   \item{Probability Result A in Good & Result B in Good}{...}
#'   \item{Probability Result A in Good & Result B in Moderate}{...}
#'   \item{Probability Result A in Good & Result B in Poor}{...}
#'   \item{Probability Result A in Good & Result B in Bad}{...}
#'   \item{Probability Result A in Moderate & Result B in High}{...}
#'   \item{Probability Result A in Moderate & Result B in Good}{...}
#'   \item{Probability Result A in Moderate & Result B in Moderate}{...}
#'   \item{Probability Result A in Moderate & Result B in Poor}{...}
#'   \item{Probability Result A in Moderate & Result B in Bad}{...}
#'   \item{Probability Result A in Poor & Result B in High}{...}
#'   \item{Probability Result A in Poor & Result B in Good}{...}
#'   \item{Probability Result A in Poor & Result B in Moderate}{...}
#'   \item{Probability Result A in Poor & Result B in Poor}{...}
#'   \item{Probability Result A in Poor & Result B in Bad}{...}
#'   \item{Probability Result A in Bad & Result B in High}{...}
#'   \item{Probability Result A in Bad & Result B in Good}{...}
#'   \item{Probability Result A in Bad & Result B in Moderate}{...}
#'   \item{Probability Result A in Bad & Result B in Poor}{...}
#'   \item{Probability Result A in Bad & Result B in Bad}{...}
#' }
#' @export
#' @examples
#' \dontrun{
#' results_a <- rict(demo_observed_values[1:, ], store_eqrs = T, year_type = "single")
#' results_b <- rict(demo_gis_values[1:3, ], store_eqrs = T, year_type = "single", model = "gis")
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

  # Bind columns from a anda b results. If eqr_metrics specified keep only these
  # columns otherwise bind all columns.
  if (is.null(eqr_metrics)) {
    # Default to min number of columns if a and b differ in column number
    min_length <- min(length(results_a), length(results_b))
    data <- dplyr::bind_rows(
      results_a[, min_length],
      results_b[, min_length]
    )
  } else {
    columns <- c("SITE", "YEAR", "RESULT", eqr_metrics)
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
  else if (all(results_a$SITE == results_b$SITE)) {

    # Loop through all rows in 'a' comparing to matching result in 'b'
    comparisons <- purrr::map_df(as.numeric(row.names(results_a)), function(a) {
      compare(data[c(a, (length(data$RESULT) / 2) + a), ],
        a = data$RESULT[a],
        b = data$RESULT[(length(data$RESULT) / 2) + a]
      )
    })
  } else {
    message("You provided two datasets with matching SITE(s)
 but differing number of rows. We expect datasets that share matching
 SITE(S) to have matching number of rows.")
  }
  return(comparisons)
}