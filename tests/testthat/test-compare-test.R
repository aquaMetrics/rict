test_that("expect compare test to work", {

  # Get demo eqrs provided by Ralph
  results <- demo_eqr_values

  # Compare all the RESULT rows (a) with all the other RESULT rows (b)
  a <- results$RESULT[grep("R",results$RESULT)]
  b <- results$RESULT[grep("D",results$RESULT)]

  test_results <- rict:::compare_test(results = results, a = a, b = b, eqrs = "NTAXA EQR")

  # Filter only to compare matching 'R' (real?) with 'D' (artificially downgraded?)
  # This matches the sites compared in Ralph's provided comparison data
  test_results$A <-  substr(test_results$`Result A`, 1, 6)
  test_results$B <-  substr(test_results$`Result B`, 1, 6)
  test_results <- test_results[test_results$A == test_results$B, ]

 # A dataset containing testing comparison values provided by Ralph
 compare_test_values <- utils::read.csv(
  system.file("extdat",
   "compare-results-for-gb-model-1-test-sites.csv",
          package = "rict"), check.names = F)

 # close enough?
 equal <- all.equal(round(test_results[, 4:10],3),
                   compare_test_values[, 4:10])

})
