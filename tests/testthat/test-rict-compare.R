test_that("rict_compare matches test data", {

  # Test dataset
  test_data <- utils::read.csv(
    system.file("extdat",
                "test-data-model-1.csv",
                package = "rict"
    ),
    check.names = F
  )

  # Test dataset
  test_rict_compare <- utils::read.csv(
    system.file("extdat",
                "test-rict-compare.csv",
                package = "rict"
    ),
    check.names = F
  )

  a <-  rict(test_data[1:12, ], year_type = "single", store_eqrs = T)
  b <-  rict(test_data[13:24, ], year_type = "single", store_eqrs = T)
  test <- rict_compare(a, b)

  test_rict_compare <- test_rict_compare[, 2:45]

  expect_equal(length(all.equal(test, test_rict_compare)), 3)

})
