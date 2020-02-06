test_that("compare probability works", {

  # How to generate Spring EQRs? Not sure - separate seasonal experiments?
  # results <- demo_observed_values
  # results[1:2 , 26:33] <- NA
  # results <- rict(results, year_type = "single")

  # Get demo eqrs provided by Ralph
  results <- demo_eqr_values[c(1:2, 13:14),]

  # Compare all the RESULT rows (a) with all the other RESULT rows (b)
  a <- results$RESULT[grep("R",results$RESULT)]
  b <- results$RESULT[grep("D",results$RESULT)]

  test_results <- rict:::compare_probability(results = results, a = a, b = b, eqrs = "NTAXA EQR")

  # Filter only to compare matching 'R' (real?) with 'D' (artificially downgraded?)
  test_results$A <-  substr(test_results$`Result A`, 1, 6)
  test_results$B <-  substr(test_results$`Result B`, 1, 6)
  test_results <- test_results[test_results$A == test_results$B, ]

  # A dataset containing testing comparison values provided by Ralph
  compare_test_values <- utils::read.csv(
    system.file("extdat",
                "compare-results-for-gb-model-1-test-sites.csv",
                package = "rict"), check.names = F)

  # close enough?
  equal <- all.equal(test_results[1, 3:8 ],
                     compare_test_values[1, 11:16])
})
