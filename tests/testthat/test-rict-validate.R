test_that("sense-checks work", {
  # No data provided
  expect_error(rict_validate())
  # Not dataframe
  expect_error(rict_validate("test"))
  # Wrong variable names
  expect_error(rict_validate(data.frame("test" = 1:10)))
  # Data provided for more than two model i.e. gis and physical
  expect_error(rict_validate(cbind(demo_observed_values, demo_gis_values_log[, 14:17])))
  # Data from GB and NI in input dataset
  test_data <- demo_gis_values_log
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "S"
  expect_error(rict_validate(test_data))
  # All variables present
  test_data <- demo_gis_values_log
  test_data$SITE <- NULL
  expect_error(rict_validate(test_data))
  # Check NA caught
  test_data <- demo_gis_values_log
  test_data$Alkalinity[1] <- NA
  test_data$SITE[1] <- NA
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
  # Check NA in NGR will fail
  test_data <- demo_gis_values_log
  test_data$NGR[1] <- NA
  expect_error(rict_validate(test_data), "The data provided contains more than one area of the UK.
        Hint: Check your data contains NGR grid letters for either: NI or GB. ")
  # Check lower case NGR work - regex was not detecting lower case - fixed now.
  test_data <- demo_gis_values_log
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "se"
  no_error <- rict_validate(test_data)
  # Check missing Easting and Northing will fail
  test_data <- demo_gis_values_log
  test_data$EASTING[1] <- NA
  expect_error(rict_validate(test_data), "EASTING or NORTHING")
  # Check data types are correct
  test_data <- demo_gis_values_log
  test_data$Alkalinity <- "test"
  test_data$NGR <- 1
  expect_error(
    rict_validate(test_data),
    "You provided column 'ALKALINITY' with class 'character', we expect class 'numeric'. You provided column 'NGR' with class 'numeric', we expect class 'character'. ")
  # Check optional columns where one or the other column must be provided
  test_data <- demo_observed_values
  test_data$Velocity <- NA
  test_data$Discharge <- NA
  expect_error(rict_validate(test_data), "You provided empty VELOCITY and DISCHARGE values,
          we expect values for at least one of these variables. ")
  # Warning if both discharge and velocity have values
  test_data <- demo_observed_values
  test_data$Velocity <- 1
  expect_warning(rict_validate(test_data), "You provided both VELOCITY and DISCHARGE values,
          DISCHARGE will be used by default. ")
  # NGR must all be less than three letters long
  test_data <- demo_observed_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "BIG"
  expect_error(rict_validate(test_data))
})
# ---------------------------------------------------------------------
test_that("alkalinity, hardness, conductivity and calcium calculations work", {
  skip("needs more work")
  test_data <- demo_observed_values
  test_data$Alkalinity[1:2] <- NA
  test_data$Hardness[1] <- 50
  test_data$Calcium[2] <- 50
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 0)
})
# ---------------------------------------------------------------------
test_that("velocity calculation work", {
  skip("needs more work")
  test_data <- demo_observed_values
  test_data$Velocity[1:5] <- 5
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 5)
})

# ---------------------------------------------------------------------
test_that("warnings work", {
  test_data <- demo_observed_values
  test_data$Slope[1] <- 1500
  test_data$Slope[2] <- 0
  test_data$Slope[3] <- 0.1
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
})
# ---------------------------------------------------------------------
test_that("failures work", {
  test_data <- demo_observed_values
  test_data$Discharge[1] <- 1500
  test_data$Pebbles_Gravel[1] <- 90
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
})
# ---------------------------------------------------------------------
test_that("replacement values work if value is less than the ‘overall’ minimum value", {
  test_data <- demo_observed_values
  test_data$Altitude[1] <- 0
  test_data$Dist_from_Source[1] <- 0.01
  test_data$Mean_Width[1] <- 0.01
  test_data$Mean_Depth[1] <- 0.1
  test_data$Discharge[1] <- 0
  test_data$Alkalinity[1] <- 0.001
  test_data$Slope[1] <- 0
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 7)
  expect_equal(test[[1]][1, c("ALTITUDE")], 1)
  expect_equal(test[[1]][1, c("DIST_FROM_SOURCE")], 0.1)
  expect_equal(test[[1]][1, c("MEAN_WIDTH")], 0.1)
  expect_equal(test[[1]][1, c("MEAN_DEPTH")], 1)
  expect_equal(test[[1]][1, c("DISCHARGE")], 1)
  expect_equal(test[[1]][1, c("ALKALINITY")], 0.1)
  expect_equal(test[[1]][1, c("SLOPE")], 0.1)
})
# ---------------------------------------------------------------------
test_that("user supplied temperatures override calculate temperatures", {
  test_data <- demo_observed_values
  test_data$MEAN.AIR.TEMP <- 15
  test_data$AIR.TEMP.RANGE <- 36
  data <- rict_validate(test_data)
  expect_equal(is.na(length(data[["checks"]]$WARN)), FALSE)
})
