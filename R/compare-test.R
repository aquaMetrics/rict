#' A Statistical Test to Compare Two Results
#'
#' Statistics to compare whether there is a real difference in EQR values of
#' paired results.
#'
#' @param a numeric vector containing results 'a'
#' @param b numeric vector constainng results 'b'
#'
#' @return dataframe containing stats
#' @export
#'
#' @examples
#' \dontrun{
#' test <- compare_test(
#'   a = c(0.3, 0.4, 0.6),
#'   b = c(1, 0.9, 0.6)
#' )
#' }
compare_test <- function(a, b) {
  # Calculate difference between a and b eqrs

  # set seed for reproducible results
  set.seed(42)
  diff <- sample(b) - sample(a)
  # Make dataframe of useful test statistics
  compare <- data.frame(
    "Average EQR for Result A" =
      mean(a),
    "Average EQR for Result B" =
      mean(b),
    "Average EQR: Difference (B - A)" =
      mean(b) - mean(a),
    "Standard Deviation of Difference" =
      stats::sd(diff),
    "Lower 95% (L95) of Difference" = stats::quantile(sort(diff),
      probs = c(2.5, 97.5) / 100
    )[[1]],
    "Upper 95% (U95) of Difference" = stats::quantile(sort(diff),
      probs = c(2.5, 97.5) / 100
    )[[2]],
    "2-sided test probability p of No difference in EQR" =
      2 * min(
        length(diff[diff <= 0]) / length(diff),
        length(diff[diff >= 0]) / length(diff)
      ),
    check.names = F, stringsAsFactors = F
  )
  return(compare)
}
