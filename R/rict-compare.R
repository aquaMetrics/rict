#' Compare the Statistical Differences between Classifications Results
#'
#' The `rict_compare` function allows the user to assess whether there is a real
#' difference in EQR values and/or status class between a pair of results and/or
#' sites and/or time periods.
#'
#' The `rict_compare` makes use of two lower level functions:
#' \code{\link{compare_test}} and \code{\link{compare_probability}}. Before
#' passing data to these functions it formats the data and removes results for
#' example the 'MINTA' values (as these are not applicable for compare_test()).
#'
#' @param results_a Dataframe output from `rict(store_eqrs = T)`. The
#'   `results_b` and `results_b` input  dataframes must have the same number of
#'   rows
#' @param results_b Dataframe output from `rict(store_eqrs = T)`. The
#'   `results_b` and `results_b` input  dataframes must have the same number of
#'   rows
#' @return Dataframe of compare results with 44 variables see `technical
#' specifications` for details. Returns output of results the same length as
#' the input files.
#' \describe{
#'   \item{EQR metric compared}{EQR metric from Result A which is being compared
#'   ("Vs") the metric in Result B for example SUM_ASPT Vs SUM_ASPT - this row
#'   is comparing Summer ASPT results A to Summer ASPT in results B. Note, if
#'   only one season is calculated, the "AVG_"... EQR metric (average) will have
#'   the same value as the season EQR value}
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
#' results_a <- rict(demo_observed_values[1, ], store_eqrs = TRUE, year_type = "single")
#' results_b <- rict(demo_gis_values_log[2, ], store_eqrs = TRUE, year_type = "single")
#' compare <- rict_compare(results_a, results_b)
#' }
rict_compare <- function(results_a = NULL, results_b = NULL) {
  message("Comparing simulated EQR results...")

  # Stop if results_a and b are not paired/ equal length.
  if (nrow(results_a) != nrow(results_b)) {
    stop("You supplied two datasets to compare with differing number of rows.
        We expect both datasets have the same number of rows.
        HINT - Check your datasets have the same number of rows")
  }

  # Create 'result' ID
  results_a$RESULT <- paste(results_a$SITE, results_a$YEAR)
  results_b$RESULT <- paste(results_b$SITE, results_b$YEAR)

  # Get EQR metric names (sometimes these could be different) for example
  # comparing SPR_NTAXA to AUT_NTAXA
  EQR_Metrics <- paste(
    results_a$`EQR Metrics`,
    "Vs",
    results_b$`EQR Metrics`
  )

  results_a$`EQR Metrics` <- EQR_Metrics
  results_b$`EQR Metrics` <- EQR_Metrics
  ### TODO Filter out ASPT Vs NTAXA Vs Minta - comparison not allowed?  ###

  # Loop through each EQR metrics
  comparisons <- lapply(split(results_a, results_a$`EQR Metrics`), function(eqr_metric) {
    # Special case handling - provide 'EQR' boundaries, labels and cap_eqr paraemters
    # in preparation for compare_probability() function
    if (grepl("minta", unique(eqr_metric$`EQR Metrics`), ignore.case = T)) {
      eqr_bands <- 1:6 # Minta uses class not EQR boundaries
      cap_eqrs <- F
      labels <- 1:5
    }
    if (grepl("aspt", unique(eqr_metric$`EQR Metrics`), ignore.case = T)) {
      eqr_bands <- c(0, 0.59, 0.72, 0.86, 0.97, 1)
      cap_eqrs <- T
      labels <- 5:1
    }
    if (grepl("ntaxa", unique(eqr_metric$`EQR Metrics`), ignore.case = T)) {
      eqr_bands <- c(0, 0.47, 0.56, 0.68, 0.8, 1)
      cap_eqrs <- T
      labels <- 5:1
    }
    # Loop through all unique results in 'a'
    comparison <- lapply(split(eqr_metric, eqr_metric$ID), function(a) {
      # Find matching b results EQRs to compare
      b <- results_b[results_b$ID == unique(a$ID) &
        results_b$`EQR Metrics` == unique(a$`EQR Metrics`), ]

      compare_probability <- compare_probability(
        a = a$EQR,
        b = b$EQR,
        eqr_bands = eqr_bands,
        cap_eqrs = cap_eqrs,
        labels = labels
      )
      compare_test <- compare_test(a = a$EQR, b = b$EQR)

      compare_output <- cbind(
        "EQR metric compared" = unique(a$`EQR Metrics`),
        "Result A" = unique(a$RESULT),
        "Result B" = unique(b$RESULT),
        compare_test,
        compare_probability
      )
      return(compare_output)
    })
    comparisons <- do.call("rbind", comparison)
  })
  results <- do.call("rbind", comparisons)
  # Special case - filter out minta `compare_test()` results as not applicable
  results[grep("MINTA", results$`EQR metric compared`), 4:10] <- NA
  # Tidy messy row.names
  row.names(results) <- seq_len(nrow(results))
  message("Simulated EQR comparison completed")
  return(results)
}
