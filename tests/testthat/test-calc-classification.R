context("test calcClassifcation")

test_that("calcRict outputs dataframe", {
  predictions <- calcPredictions(observed_values = demo_observed_values)
  classification <- calcClassification(predictions)
  expect_equal(class(classification), "data.frame")
})
