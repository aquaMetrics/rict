## A statistical test to compare results ----------------------------------------------
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
      sd(b - a),
    "Lower 95% (L95) of Difference" = quantile(sort(diff),
      probs = c(2.5, 97.5) / 100
    )[[1]],
    "Upper 95% (U95) of Difference" = quantile(sort(diff),
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