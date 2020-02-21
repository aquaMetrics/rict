test_that("rict_compare matches test data", {

  # Test dataset
  test_data <- utils::read.csv(
    system.file("extdat",
                "test-data-model-1.csv",
                package = "rict"
    ),
    check.names = F
  )

  results <-  rict(test_data, year_type = "single", store_eqrs = T)
  a <- results[results$ID %in% 1:12, ]
  b <-  results[results$ID %in% 13:24, ]
  b$ID <- as.factor(b$ID)
  levels(b$ID) <- 1:12
  b$ID <- as.integer(b$ID)
  test <- rict_compare(a, b)
})