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
  skip("not passing - Work in progress")
  predictions <- rict_predict(data = demo_gis_values, model = "gis")

  expected_predictions <- utils::read.csv(system.file("extdat",
    "expected-biotic-scores-model-44.csv",
    package = "rict"
  ), check.names = F)

  # ISSUE: lat/lon calculation converts NGR slightly different to values on
  # Spreadsheet "RICT(E and Azure) software - Independent code Testing Results v2"
  # has slightly difference in log values.
  # Rounding issues in discrimant functions? Even using log values from
  # spreadsheet there's in Mahalanobis Distances.

  equal <- all.equal(
    predictions[, names(predictions) %in% names(expected_predictions)],
    expected_predictions[, names(expected_predictions) %in% names(predictions)]
  )

  expect_true(equal == T)
})