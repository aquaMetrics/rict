context("test calcRict")

test_that("calcRict outputs dataframe", {
  # fails on travis for some reason maybe related: https://github.com/r-lib/testthat/issues/364
  skip_on_travis()
  # Load some test data
  environmentalVariables <- read.csv(system.file("extdat",
                                                 "fba-test-WHPT.csv",
                                                 package = "rict"),
                                                 stringsAsFactors = F)

  # Load some test end groups
  endPointMeans <- read.csv(system.file("extdat",
                                        "End_Group_Means.csv",
                                        package = "rict"),
                                        stringsAsFactors = F)

  output <- calcRict(environmentalVariables = environmentalVariables, endPointMeans = endPointMeans)

  expect_equal(class(output), "data.frame")
})
