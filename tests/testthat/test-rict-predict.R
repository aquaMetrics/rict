context("test rict_predict")

test_that("rict_predict for physical variables", {
  predictions <- rict_predict(data = demo_observed_values)

  expected_predictions <- utils::read.csv(system.file("extdat",
    "validation-prediction-multi-year.csv",
    package = "rict"
  ))

  expected_predictions$SuitCode <- as.factor(expected_predictions$SuitCode)
  names(expected_predictions)[17] <- "belongs_to_end_grp"

  equal <- all.equal(
    predictions[, names(predictions) %in% names(expected_predictions)],
    expected_predictions
  )

  expect_true(equal == T)
})

test_that("rict_predict for GIS variables", {
  predictions <- rict_predict(data = demo_gis_values, model = "gis")
  # load unadjusted reference predicted values:
  expected_predictions <- utils::read.csv(system.file("extdat",
    "expected-unadjusted-biotic-scores-model-44.csv",
    package = "rict"
  ), check.names = F)

 expected_end_group <- utils::read.csv(system.file("extdat",
                     "test-expected-end-group-model-44.csv",
                     package = "rict"
  ), check.names = F, header = F)
 expected_end_group <- data.frame(t(expected_end_group))
 names(expected_end_group) <- paste0("p", 1:43)

test <-  predictions[, names(predictions) %in% names(expected_end_group)] -
 expected_end_group[, names(expected_end_group) %in% names(predictions)]
# check end groups don't differ more than 0.00038 - Ralph's rule of thumb
expect_true(max(test) < 0.00038)

  # Note: lat/lon calculation converts NGR slightly different to values and rounding on
  # log values may also cause slight changes. So rounding output to 2 decimal places:
  equal <- all.equal(
    round(predictions[, names(predictions) %in% names(expected_predictions)][2:5], 2),
    round(expected_predictions[, names(expected_predictions) %in% names(predictions)][2:5], 2)
  )

  expect_true(equal == T)
})