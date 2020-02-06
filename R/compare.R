#' Compare the statistical differences between classifications results
#'
#' Low-level function to assess whether there is a real
#' difference in EQR values and/or status class between simulated EQR results.
#' What this function does:
#' \enumerate{
#'  \item Run probability of difference
#'  \item Enumerate probability of difference
#' }
#' @param data Data frame containing at least three columns.  A 'RESULT' and 'SITE' column
#'   plus at least one column with simulated eqrs. For
#'   example see `demo_eqr_values` - this is nested but flat dataframe will work too.
#' @param
#' @return Dataframe of compare results
#' @export
#' @examples
#' \dontrun{
#' compare_results <- compare(demo_eqr_values)
#' }
#'
compare <- function(data = NULL, a = NULL, b = NULL) {
  message("Comparing results...")
  results <- data
  # Compare all the RESULT rows (a) with all the other RESULT rows (b)
  if (is.null(a) | is.null(b)) {
    a <- results$RESULT
    b <- results$RESULT
    message("No parameter provided to `a` and/or `b` arguments -
defaulting to comparing all results to all other results in input data!")
  }
  # need eqr metric - ASPT, NTAXA...
  eqrs <- names(results)[!names(results) %in% c("RESULT", "SITE")]

  # Compare probability test
  compare_test <- compare_test(results, eqrs = eqrs, a, b)
  # Compare probability table
  compare_probability <- compare_probability(results, eqrs, a, b)
  # Join output
  compare_output <- merge(compare_test, compare_probability,
                            by.x = c("Result A","Result B"),
                            by.y = c("Result A","Result B"))
  message("Comparison completed!")
  return(compare_output)
}