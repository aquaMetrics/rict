context("test calcRict")

test_that("calcRict outputs dataframe", {
  # Load some test data
  environmentalVariables <- read.csv(system.file("extdat",
                                                 "fba-test-whpt.csv",
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
