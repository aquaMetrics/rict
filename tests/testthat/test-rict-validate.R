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
  expect_error(
    rict_validate(test_data),
    "You provided column 'ALKALINITY' with class 'character', we expect class 'numeric'."
  )
  # Test optional columns where one or the other column must be provided
  test_data <- rict::demo_observed_values
  test_data$Velocity <- NA
  test_data$Discharge <- NA
  expect_error(rict_validate(test_data))
  # Velocity over limits
  test_data <- demo_observed_values
  test_data$Velocity <- 6
  test_data$Velocity[1] <- 0
  test_data$Discharge <- NA
  expect_error(rict_validate(test_data))
  # Empty ALKALINITY, HARDNESS, CONDUCT and CALCIUM values
  test_data <- demo_observed_values
  test_data$Alkalinity <- NA
  expect_error(rict_validate(test_data))
  # Test NAs in NGR will fail
  test_data <- demo_observed_values
  test_data$NGR[1] <- NA
  expect_error(rict_validate(test_data), "You provided data with one or more NGR values missing,
       Hint: Check your NGR variable has letters. ")
  # Test if all NGR values NA will fail
  test_data <- demo_observed_values
  test_data$NGR <- NA
  expect_error(rict_validate(test_data), "You provided data with all NGR values missing,
       Hint: Check your NGR variable has letters. ")
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
  # Missing WHPT scores
  test_data <- demo_observed_values
  test_data[, grep("DistFam", names(test_data))] <- NA
  test <- rict(test_data)
  # Missing SX or SY for GIS
  test_data <- demo_gis_values_log
  test_data$SX <- NULL
  test_data$SY <- NULL
  expect_error(rict_validate(test_data))
})

# ---------------------------------------------------------------------
test_that("fails on some rows create fail messages (but process continues of validate data)", {
  # check if stop_if_all_fail = FALSE works (produces empty validate dataframe)
  data <- demo_observed_values
  data$Altitude <- 2000
  test <- rict_validate(data, stop_if_all_fail = FALSE)
  expect_equal(nrow(test$data), 0)
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
  test_data <- rict::demo_observed_values
  test_data$Discharge[1] <- 11
  test_data$Discharge[2] <- 0
  test_data$Velocity[3] <- 6
  test_data$Discharge[3] <- NA
  test_data$Pebbles_Gravel[1] <- 90
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 4)
  # test temperature fails if outside temperature grid
  test_data <- demo_observed_values
  test_data$NGR <- as.character(test_data$NGR)
  test_data$NGR[1] <- "NZ"
  test_data$Easting[1] <- "57900"
  test_data$Northing[1] <- "59000"
  test <- rict_validate(test_data)
  test <- test$checks
  expect_equal(length(test$FAIL[test$FAIL != "---"]), 2)
  # Test optional columns where one or the other column must be provided
  test_data <- demo_observed_values
  test_data$Velocity[1] <- NA
  test_data$Discharge[1] <- NA
  test <- rict_validate(test_data)
  test <- test$checks
  expect_equal(length(test$FAIL[test$FAIL != "---"]), 5)
  # Test GIS values also value
  test_data <- rict::demo_gis_values_log
  test_data$disch_cat[1] <- NA
  test_data$disch_cat[2] <- 0
  test_data$disch_cat[3] <- 11
  test_data$Altitude[1] <- -2
  test <- rict_validate(test_data)
  test <- test$checks
  expect_equal(length(test$FAIL[test$FAIL != "---"]), 5)
})

# ---------------------------------------------------------------------
test_that("warnings work", {
  test_data <- demo_observed_values
  test_data$Slope[1] <- 1500
  test_data$Slope[2] <- 0
  test_data$Slope[3] <- 0.1
  test_data$Year[1] <- 1989
  test_data$Velocity[2] <- 6 # Stored in velo_dummy column??!!
  test_data$Discharge[2] <- NA
  test_data$Discharge[1] <- 10
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 5)

  test_data <- demo_ni_observed_values
  test_data$Discharge[1] <- 9
  test <- rict_validate(test_data[1, ])
  expect_equal(length(test[[2]][, 1]), 1)
  # Test user supplied temperatures override calculate temperatures
  test_data <- demo_observed_values
  test_data$MEAN.AIR.TEMP <- 15
  test_data$AIR.TEMP.RANGE <- 36
  data <- rict_validate(test_data)
  expect_equal(is.na(length(data[["checks"]]$WARNING)), FALSE)
  # Warning if both discharge and velocity have values
  test_data <- demo_observed_values
  test_data$Velocity <- 1
  expect_warning(rict_validate(test_data))
  # GIS model warnings work
  test_data <- rict::demo_gis_values_log
  test_data$disch_cat[1] <- 10
  test_data$d_f_source[1] <- 246000
  test <- rict_validate(test_data)
  test <- test$checks
  expect_equal(length(test$FAIL[test$WARN != "---"]), 2)
})

# ---------------------------------------------------------------------
test_that("replacement values work if value is less than the ‘overall’ minimum value (to avoid dividing by zero)", {
  test_data <- demo_observed_values
  test_data$Altitude[1] <- 0
  test_data$Dist_from_Source[1] <- 0.01
  test_data$Mean_Width[1] <- 0.01
  test_data$Mean_Depth[1] <- 0.1
  test_data$Alkalinity[1] <- 0.001
  test_data$Slope[1] <- 0
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 6)
  expect_equal(test[[1]][1, c("ALTITUDE")], 1)
  expect_equal(test[[1]][1, c("DIST_FROM_SOURCE")], 0.1)
  expect_equal(test[[1]][1, c("MEAN_WIDTH")], 0.1)
  expect_equal(test[[1]][1, c("MEAN_DEPTH")], 1)
  expect_equal(test[[1]][1, c("ALKALINITY")], 0.1)
  expect_equal(test[[1]][1, c("SLOPE")], 0.1)

  test_data <- demo_gis_values_log
  test_data$Altitude[1] <- 0
  test_data$d_f_source[1] <- 90
  test_data$Alkalinity[1] <- 0.1
  test_data$slope[1] <- 0
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 4)
  expect_equal(test[[1]][1, c("ALTITUDE")], 1)
  expect_equal(test[[1]][1, c("D_F_SOURCE")], 0.1) # Now converted to Km from metres
  expect_equal(test[[1]][1, c("ALKALINITY")], 0.1)
  expect_equal(test[[1]][1, c("SLOPE")], 0.1)
})

# ---------------------------------------------------------------------
test_that("alkalinity, hardness, conductivity and calcium calculations work", {
  test_data <- demo_observed_values[1:3, ]
  test_data$Alkalinity[1:3] <- NA
  test_data$Hardness[1] <- 50
  test_data$Calcium[2] <- 50
  test_data$Conductivity[3] <- 50
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 0)
  # Also works for GIS input:
  test_data <- demo_gis_values_log[1:4, ]
  test_data$Alkalinity[1:3] <- NA
  test_data$hardness[4] <- 50
  # Hardness, calcium, conductivity is order of preference
  test_data$hardness[1] <- 50 # hardness preferred
  test_data$calcium[1] <- 50
  test_data$Conductivity[1] <- 50
  test_data$calcium[2] <- 50 # calcium preferred
  test_data$Conductivity[2] <- 50
  test_data$Conductivity[3] <- 50 # conductivity preferred (only option)
  test <- rict_validate(test_data)
  messages <- capture_messages(rict_validate(test_data))
  expect_equal(
    messages[2],
    "Using Hardness value to calculate Alkalinity at TST-GB-01-R - 2019. \n"
  )
  expect_equal(
    messages[3],
    "Using Calcium value to calculate Alkalinity at TST-GB-02-R - 2019. \n"
  )
  expect_equal(
    messages[4],
    "Using Conductivity value to calculate Alkalinity at TST-GB-03-R - 2019. \n"
  )
  expect_equal(length(test[[2]][, 1]), 0)
  expect_equal(test$data$ALKALINITY, c(36.6420, 102.5820, 7.9457, 164.0000))
  # Alkalinity value used by default over hardness etc
  test_data <- demo_observed_values[1:3, ]
  test_data$Hardness[1] <- 50
  test_data$Calcium[2] <- 50
  test_data$Conductivity[3] <- 50
  test <- rict_validate(test_data)
  test_data <- demo_observed_values[1:3, ]
  expect <- rict_validate(test_data)
  expect_equal(expect$data$ALKALINITY, test$data$ALKALINITY)
})

# ---------------------------------------------------------------------
test_that("velocity calculation work", {
  test_data <- rict::demo_observed_values
  test_data$Velocity[1:5] <- 5
  test <- rict_validate(test_data)
  expect_equal(length(test[[2]][, 1]), 0)

  # Test discharge_value of exactly 0.31 returns discharge value of 2. The
  # discharge_value is calculated in `get_discharge` function.
  test_data <- rict::demo_observed_values[1, ]
  test_data$Mean_Width <- 6.2
  test_data$Mean_Depth <- 100
  test_data$Velocity <- 1
  test_data$Discharge <- NA
  test <- rict_validate(test_data)
  expect_equal(test$data$DISCHARGE, 2)
  # Test discharge_value of less than 0.31 returns discharge value of 2. The
  # discharge_value is calculated in `get_discharge` function.
  test_data <- rict::demo_observed_values[1, ]
  test_data$Mean_Width <- 6
  test_data$Mean_Depth <- 100
  test_data$Velocity <- 1
  test_data$Discharge <- NA
  test <- rict_validate(test_data)
  expect_equal(test$data$DISCHARGE, 1)
  # Test discharge_value of less than 80 returns discharge value of 9. The
  # discharge_value is calculated in `get_discharge` function. NOTE- cannot get
  # discharge_value to return exactly 80 part have manually tested that value of
  # 80 returns 9 and greater than 80 returns 10.
  test_data <- rict::demo_observed_values[1, ]
  test_data$Mean_Width <- 53.333333
  test_data$Mean_Depth <- 100
  test_data$Velocity <- 5
  test_data$Discharge <- NA
  test <- rict_validate(test_data)
  expect_equal(test$data$DISCHARGE, 9)
  # Test discharge_value of greater than 80 returns discharge value of 10. The
  # discharge_value is calculated in `get_discharge` function.
  test_data <- rict::demo_observed_values[1, ]
  test_data$Mean_Width <- 53.34
  test_data$Mean_Depth <- 100
  test_data$Velocity <- 5
  test_data$Discharge <- NA
  test <- rict_validate(test_data)
  expect_equal(test$data$DISCHARGE, 10)
})

# ---------------------------------------------------------------------
test_that("changes/formatting that shouldn't impact calculations", {
  # Test adding NGR column  in demo_gis_values_log doesn't interfere
  # (SX/SY columns provide location for GIS/model 44 data)
  test_data <- demo_gis_values_log
  # Doesn't matter if GIS has NGR or not - not used in calculations:
  test_data$NGR <- "S"
  expect_equal(nrow(rict_validate(test_data)[["checks"]]), 0)
  # Doesn't matter if NI has optional leading 'I' or not
  test_data <- demo_ni_observed_values
  test_data$NGR <- paste0("I", test_data$NGR)
  expect_equal(rict_validate(test_data)[["area"]], "ni")
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
  # test row = TRUE (used to match checks to input data)
  data <- rict::demo_observed_values
  data$Slope[2] <- 151
  data$Altitude[2] <- 700
  data$Slope[8] <- 151
  data$Altitude[8] <- 700
  data$ROW <- seq_len(nrow(data))
  test <- rict_validate(data, row = TRUE)
  checks_data <- merge(data, test$checks, by.x = "ROW", by.y = "ROW")
  expect_equal(nrow(checks_data), 4)
  # data is tibble (if using readr package etc)
  data <- rict::demo_observed_values
  data <- tibble::tibble(data)
  data <- data[1, ]
  test <- rict_validate(data)
  expect_equal(length(test), 4)
  # iom area & model detection
  test <- rict_validate(demo_iom_observed_values)
  expect_equal(test$area, "iom")
  expect_equal(test$model[[1]], "physical")
  # iom NGR change to GB instead
  data <- demo_iom_observed_values[1,]
  data$Easting <- 55001
  data$Northing <- 10001
  expect_error(rict_validate(data))
  # iom NGR change to GB instead
  data <- demo_iom_observed_values[4,]
  data$Easting <- 55001
  data$Northing <- 99999
  expect_error(rict_validate(data))
  # Additional columns added to IOM data will be ignored
  data <- demo_iom_observed_values
  data$silt <- 1
  data$Discharge <- NA
  data$Velocity <- NA
  test <- rict_validate(data)
  expect_equal(test$area, "iom")
})
