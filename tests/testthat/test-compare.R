test_that("expect compare function to match with test data", {

  # Get SPR NTAXA
  # This was to try are re-create EQRs from demo data not from Ralph's supplied EQRs
  # demo_observed_values <- demo_observed_values
  # demo_observed_values$Year[1:12] <- 1:12
  # demo_observed_values$Year[13:24] <- 1:12
  # results <- rict(demo_observed_values, year_type = "single", store_eqrs = T)
  # results <- dplyr::select(results, SITE, YEAR, SPR_NTAXA)
  # results$RESULT <- paste(results$SITE, results$YEAR)
  # a <- results[grep("-R", results$RESULT), ]
  # b <- results[grep("-D", results$RESULT), ]
  # test <-  rict_compare(results_a = a, results_b = b, eqr_metrics = c("SPR_NTAXA"))
  # test$check <-  gsub("-D", "-R", test$`Result B`)
  # test <- test[test$`Result A` == test$check, ]
  # test$check <- NULL
  # test_results <- test

  # Get demo eqrs provided by Ralph
  results <- demo_eqr_values
  results$`EQR Metrics` <- "SPR_NTAXA"

  # Compare all the RESULT rows (a) with all the other RESULT rows (b)
  a <- results$RESULT[grep("-R", results$RESULT)]
  b <- results$RESULT[grep("-D", results$RESULT)]

  test_results <- rict:::compare(data = results, a_results = a, b_results = b)

  # Filter only to compare matching 'R' (real?) with 'D' (artificially downgraded?)
  # This matches the sites compared in Ralph's provided comparison data
  test_results$A <- substr(test_results$`Result A`, 1, 6)
  test_results$B <- substr(test_results$`Result B`, 1, 6)
  test_results <- test_results[test_results$A == test_results$B, ]
  test_results$A <- NULL
  test_results$B <- NULL

  # A dataset containing testing comparison values provided by Ralph
  compare_test_values <- utils::read.csv(
    system.file("extdat",
      "compare-results-for-gb-model-1-test-sites.csv",
      package = "rict"
    ),
    check.names = F
  )

  names(compare_test_values)[29] <- "Probability Sample A = class G and Sample B = class B"
  # tidy classes
  compare_test_values <- compare_test_values %>% dplyr::mutate_if(is.factor, as.character)
  compare_test_values <- compare_test_values %>% dplyr::mutate_if(is.integer, as.numeric)

  test_results <- test_results %>% dplyr::mutate_if(is.factor, as.character)

  row.names(test_results) <- 1:nrow(test_results)
  row.names(compare_test_values) <- 1:nrow(compare_test_values)

  # close enough?
  equal <- all.equal(
    test_results,
    compare_test_values
  )

  expect_equal(
    equal[1:8],
    c(
      "Names: 37 string mismatches",
      "Component 1: 12 string mismatches",
      "Component 2: 12 string mismatches",
      "Component 3: 12 string mismatches",
      "Component 4: Mean relative difference: 0.0001960894",
      "Component 5: Mean relative difference: 0.0003590471",
      "Component 6: Mean relative difference: 0.0004592005",
      "Component 7: Mean relative difference: 0.001075709"
    )
  )
})
