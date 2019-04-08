context("test calcPredictions")

test_that("calcRict outputs dataframe", {
  predictions <- calcPredictions(observed_values = demo_observed_values)
  expect_equal(class(predictions), "data.frame")
})
