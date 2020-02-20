#' A Statistical Test to Compare Two Results
#'
#' @param a numeric results 'a'
#' @param b numeric reuslts 'b'
#'
#' @return dataframe containing stats
#' @export
#'
#' @examples
#' \dontrun{
#' test <- compare_test(a = c(0.3,0.4,0.6),
#'                             b = c(1,0.9,0.6))
#' }
compare_test <- function(a, b) {
  # Calculate difference between a and b eqrs
  diff <- b - a
  # Make dataframe of useful test statistics
  compare <- data.frame(
    "Average EQR for Result A" =
      mean(a),
    "Average EQR for Result B" =
      mean(b),
    "Average EQR: Difference (B - A)" =
      mean(b) - mean(a),
    "Standard Deviation of Difference" =
      stats::sd(b - a),
    "Lower 95% (L95) of Difference" = stats::quantile(sort(diff),
      probs = c(2.5, 97.5) / 100
    )[[1]],
    "Upper 95% (U95) of Difference" = stats::quantile(sort(diff),
      probs = c(2.5, 97.5) / 100
    )[[2]],
    "2-sided test probability p of No difference in EQR" =
      2 * min(
        length(diff[diff >= 0]) / length(diff),
        length(diff[diff >= 0]) / length(diff)
      ),
    check.names = F, stringsAsFactors = F
  )
  return(compare)
}
