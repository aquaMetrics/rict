context("test calcPredictions")

test_that("calcRict outputs dataframe", {
  predictions <- calcPrediction(observed_values = demo_observed_values)
  expect_equal(class(predictions), "data.frame")


  validation_predictions <- utils::read.csv(system.file("extdat",
                                                        "validation-prediction-multi-year.csv",
                                                        package = "rict"))


  validation_predictions$SuitCode <- as.factor(validation_predictions$SuitCode)
  names(validation_predictions)[17] <- "belongs_to_end_grp"


  equal <- all.equal(predictions[, names(predictions) %in% names(validation_predictions)],
                     validation_predictions)
  expect_true(equal == T)
})
