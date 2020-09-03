test_that("outright fails stop process and create error message", {
  # No data provided
  expect_error(rict_validate())
  # Not dataframe
  expect_error(rict_validate("test"))
  # Wrong variable names
  expect_error(rict_validate(data.frame("test" = 1:10)))
  # Data provided for more than two model i.e. gis and physical
  expect_error(rict_validate(cbind(demo_observed_values, demo_gis_values_log[, 14:17])))
  # Some shared columns (site, year etc) but nothing to suggest which model
  expect_error(rict_validate(demo_gis_values_log[, 1:3]))
  # Data from GB and NI in input dataset
  test_data <- demo_observed_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "S"
  expect_error(rict_validate(test_data))
  # All variables present
  test_data <- demo_gis_values_log
  test_data$SITE <- NULL
  expect_error(rict_validate(test_data))
  # Test data types are correct
  test_data <- demo_gis_values_log
  test_data$Alkalinity <- "test"
  expect_error(rict_validate(test_data),
               "You provided column 'ALKALINITY' with class 'character', we expect class 'numeric'.")
  # Test optional columns where one or the other column must be provided
  test_data <- demo_observed_values
  test_data$Velocity <- NA
  test_data$Discharge <- NA
  expect_error(rict_validate(test_data), "You provided empty VELOCITY and DISCHARGE values,
          we expect values for at least one of these variables. ")
  # Empty ALKALINITY, HARDNESS, CONDUCT and CALCIUM values
  test_data <- demo_observed_values
  test_data$Alkalinity <- NA
  expect_error(rict_validate(test_data))
  # Test NA in NGR will fail
  test_data <- demo_observed_values
  test_data$NGR[1] <- NA
  expect_error(rict_validate(test_data), "The data provided contains more than one area of the UK.
        Hint: Check your data contains NGR grid letters for either: NI or GB. ")
  # NGR must all be less than three letters long
  # This stops process because NGR are processed in a batch (not individually for each site)
  # Therefore one wrong NGR in column stops whole process
  test_data <- demo_observed_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "BIG"
  expect_error(rict_validate(test_data))
  # Even a single missing Northing and Easting required
  test_data <- demo_observed_values
  test_data$Northing[1] <- NA
  expect_error(rict_validate(test_data))
  test_data <- demo_observed_values
  test_data$Easting[1] <- NA
  expect_error(rict_validate(test_data))
  # Test if all values in column fail
  test_data <- demo_observed_values
  test_data$Discharge <- 14
  expect_error(rict_validate(test_data))
  # Test fail if all substrate missing
  test_data <- demo_observed_values
  test_data$Sand <- NA
  test_data$Silt_Clay <- NA
  test_data$Boulder_Cobbles <- NA
  test_data$Pebbles_Gravel <- NA
  expect_error(rict_validate(test_data))
  # missing WHPT scores
  test_data <- demo_observed_values
  test_data[, grep("DistFam", names(test_data))] <- NA
  test <- rict(test_data)
})

# ---------------------------------------------------------------------
test_that("fails on some rows create fail messages (but process continues of validate data)", {
  # Check NA caught
  test_data <- demo_gis_values_log
  test_data$Alkalinity[1] <- NA
  test_data$SITE[1] <- NA
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
  # Test missing substrate
  test_data <- demo_observed_values
  test_data$Sand[1] <- NA
  test_data$Boulder_Cobbles[1] <- NA
  test_data$Pebbles_Gravel[1] <- NA
  test_data$Silt_Clay[1] <- NA
  expect_equal(class(rict_validate(test_data)), "list")
  # test fail values
  test_data <- demo_observed_values
  test_data$Discharge[1] <- 1500
  test_data$Pebbles_Gravel[1] <- 90
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
  # test temperature fails if outside temperature grid
  test_data <- demo_observed_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "NZ"
  test_data$Easting[1] <- "57900"
  test_data$Northing[1] <- "59000"
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 2)
})

# ---------------------------------------------------------------------
test_that("warnings work", {
  test_data <- demo_observed_values
  test_data$Slope[1] <- 1500
  test_data$Slope[2] <- 0
  test_data$Slope[3] <- 0.1
  test_data$Year[1] <- 1989
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 3)
  # Test user supplied temperatures override calculate temperatures
  test_data <- demo_observed_values
  test_data$MEAN.AIR.TEMP <- 15
  test_data$AIR.TEMP.RANGE <- 36
  data <- rict_validate(test_data)
  expect_equal(is.na(length(data[["checks"]]$WARNING)), FALSE)
  # Warning if both discharge and velocity have values
  test_data <- demo_observed_values
  test_data$Velocity <- 1
  expect_warning(rict_validate(test_data), "You provided both VELOCITY and DISCHARGE values,
          DISCHARGE will be used by default. ")
})

# ---------------------------------------------------------------------
test_that("replacement values work if value is less than the ‘overall’ minimum value (to avoid dividing by zero)", {
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
test_that("alkalinity, hardness, conductivity and calcium calculations work", {
  test_data <- demo_observed_values
  test_data$Alkalinity[1:2] <- NA
  test_data$Hardness[1] <- 50
  test_data$Calcium[2] <- 50
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 0)
})

# ---------------------------------------------------------------------
test_that("velocity calculation work", {
  test_data <- demo_observed_values
  test_data$Velocity[1:5] <- 5
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 0)
})

# ---------------------------------------------------------------------
test_that("changes/formatting that shouldn't impact calculations", {
  # Test adding NGR column  in demo_gis_values_log doesn't interfere
  # (SX/SY columns provide location for GIS/model 44 data)
  test_data <- demo_gis_values_log
  # Doesn't matter is GIS has NGR or not - not used in calculations:
  test_data$NGR <- "S"
  expect_equal(nrow(rict_validate(test_data)[["checks"]]), 0)
  # Test lower case NGR work - regex was not detecting lower case - fixed now.
  test_data <- demo_observed_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "se"
  no_error <- rict_validate(test_data)
  # Site ID treated as character even if numeric
  test_data <- demo_observed_values
  test_data$SITE <- seq_len(24)
  no_error <- rict_validate(test_data)
  # Short NGR - add leading zeros (because of issue with Excel cropping them)
  test_data <- demo_observed_values
  test_data$Easting <- "354"
  test_data$Northing <- "9200"
  test <- rict_validate(test_data)[[1]]
  expect_equal(as.character(unique(test$EASTING)), "00354")
  expect_equal(as.character(unique(test$NORTHING)), "09200")
})
