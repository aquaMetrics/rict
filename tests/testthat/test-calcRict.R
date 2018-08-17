context("test calcRict")

test_that("calcRict outputs dataframe", {
  # Load some test observed values
  observed_values <- read.csv(system.file("extdat",
                                          "fba-test-whpt.csv",
                                          package = "rict"),
                              stringsAsFactors = F)

  # Load some test expected biotic scores
  expected_biotic_scores <- read.csv(system.file("extdat",
                                          "End_Group_Means.csv",
                                          package = "rict"),
                              stringsAsFactors = F)

  output <- calcRict(observed_values = observed_values,
                     expected_biotic_scores = expected_biotic_scores)

  expect_equal(class(output), "data.frame")
})
