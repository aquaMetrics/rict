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
compare <- function(data = NULL, a_results = NULL, b_results = NULL) {
  message("Comparing simulated EQR results...")

  results <- data
  # Compare all the RESULT rows (a) with all the other RESULT rows (b)
  if (is.null(a_results) | is.null(b_results)) {
    a_results <- results$RESULT
    b_results <- results$RESULT
    message("No parameter provided to `a` and/or `b` arguments -
defaulting to comparing all results to all other differing results in input data!")
  }
  if (all(a_results == b_results)) {
    message("Nothing to compare - Results A the same as Results B")
    return(NULL)
  }
  # find all EQR metrics to compare - ASPT, NTAXA, SPR_NTAXA...
  eqrs <- names(results)[!names(results) %in% c("RESULT", "SITE", "YEAR")]

  ## Loop through results and run tests to compare results ----------------------------------------------
  combine_compare <- function(results, eqrs, a_results, b_results) {
    # Loop for each eqr type e.g. NTAXA, ASPT etc
    loop_eqr_types <- lapply(eqrs, function(eqr) {
      # Loop for each unique result(s) "a"
      loop_a <- lapply(unique(a_results), function(a_result) {
        # Loop for each unique result(s) "b"
        loop_b <- lapply(unique(b_results), function(b_result) {
          # Check not comparing same result
          if (a_result == b_result) {
            return()
          }
          # Filter results to get relevant EQRs
          a <- unlist(results[results$RESULT == a_result, eqr])
          b <- unlist(results[results$RESULT == b_result, eqr])
          # Compare probability test
          compare_test <- compare_test(a, b)
          # Compare probability table
          compare_probability <- compare_probability(a, b, eqr)
          # Check something got returned else return NULL
          if (!is.null(compare_test)) {
            # Join output
            compare_output <- cbind(
              "EQR metric compared" = eqr,
              "Result A" = a_result,
              "Result B" = b_result,
              compare_test,
              compare_probability
            )
            return(compare_output)
          } else {
            return(NULL)
          }
        })
        comparison <- do.call("rbind", loop_b) # bind all "b" result(s)
      })
      comparisons <- do.call("rbind", loop_a) # bind all "a" results(s)
    })
    comparisons_eqr <- do.call("rbind", loop_eqr_types) # bind all eqr type
  }

  compare_output <- combine_compare(results, eqrs, a_results, b_results)
  message("Simulated EQR comparison completed!")
  return(compare_output)
}