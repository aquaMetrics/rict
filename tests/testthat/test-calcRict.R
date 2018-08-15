context("test calcRict")


test_that("calcRict outputs dataframe", {

  # Load some test data
  environmentalVariables <- read.csv(file="inst/extdat/fba-test-WHPT.csv")
  # Load some test end groups
  endPointMeans <- read.csv(file="inst/extdat/End_Group_Means.csv")

  output <- calcRict(environmentalVariables=environmentalVariables, endPointMeans=endPointMeans)

  expect_equal(class(output), "data.frame")

})