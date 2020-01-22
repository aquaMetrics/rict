test_that("sense-checks work", {
  # No data provided
  expect_error(rict_validate())
  # Not dataframe
  expect_error(rict_validate("test"))
  # Wrong variable names
  expect_error(rict_validate(data.frame("test" = 1:10)))
  # Data provided for more than two model i.e. gis and physical
  expect_error(rict_validate(cbind(demo_observed_values, demo_gis_values[,14:17])))
  # Data from GB and NI in input dataset
  test_data <- demo_gis_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "S"
  expect_error(rict_validate(test_data))
  # All variables present
  test_data <- demo_gis_values
  test_data$SITE <- NULL
  expect_error(rict_validate(test_data))
  # Check data types are correct
  test_data <- demo_gis_values
  test_data$Alkalinity <- "test"
  test_data$NGR <- 1
  expect_error(rict_validate(test_data))
  # Check optional columns where one or the other column must be provided
  test_data <- demo_observed_values
  test_data$Velocity <- NA
  test_data$Discharge <- NA
  expect_error(rict_validate(test_data))
  # Warning if both discharge and velocity have values
  test_data <- demo_observed_values
  test_data$Velocity <- 1
  expect_warning(rict_validate(test_data))
  # NGR must all be less than three letters long
  test_data <- demo_observed_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "BIG"
  expect_error(rict_validate(test_data))
})

test_that("temperature override works", {
  test_data <- demo_observed_values
  test_data$TMEAN <- 15
  test_data$TRANGE <- 36
  test <- rict_validate(test_data)
})

test_that("warnings work", {
  test_data <- demo_observed_values
  test_data$Slope[1] <- 1500
  test_data$Slope[2] <- 0
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
})

test_that("failures work", {
  test_data <- demo_observed_values
  test_data$Discharge[1] <- 1500
  test_data$Pebbles_Gravel[1]  <- 90
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
})
