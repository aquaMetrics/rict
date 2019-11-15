# Validate observed values
# Returns list of dataframes containing fails, warnings and observed values
validate_observed <- function(observed_values) {
  # load air temp grid - required for validation
  air_temp_grid <- utils::read.csv(system.file("extdat", "air-temp-grid.csv", package = "rict"))
  # upper columns - repeated here just incase this function is being used on it's own
  names(observed_values) <- toupper(names(observed_values))
  # 1. MEAN_WIDTH, lower_bound=0.4, upper_bound=117
  # head(getValidEnvInput(observed_values$MEAN_WIDTH[10], 0.4, 117, "MEAN_WIDTH"),5)
  valid_mean_width <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_mean_width <- rbind(valid_mean_width, getValidEnvInput(observed_values$MEAN_WIDTH[i],
                                                                 0.4,
                                                                 117,
                                                                 "MEAN_WIDTH"))
  }

  # Change column names to suit env variable name, and cbind to original dataset
  colnames(valid_mean_width) <- paste0("mn_width_", noquote(colnames(valid_mean_width)))
  observed_values <- cbind(observed_values, valid_mean_width)

  # # Data validation
  # # 2. MEAN_DEPTH, lower_bound=1.7, upper_bound=300
  valid_mean_depth <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_mean_depth <- rbind(valid_mean_depth, getValidEnvInput(observed_values$MEAN_DEPTH [i],
                                                                 1.7,
                                                                 300,
                                                                 "MEAN_DEPTH"))
  }
  colnames(valid_mean_depth) <- paste0("mn_depth_", noquote(colnames(valid_mean_depth)))
  observed_values <- cbind(observed_values, valid_mean_depth)

  # Data validation
  # 3. SLOPE, lower_bound=0.1, upper_bound=150
  valid_slope <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_slope <- rbind(valid_slope, getValidEnvInput(observed_values$SLOPE[i],
                                                       0.1,
                                                       150,
                                                       "SLOPE"))
  }
  colnames(valid_slope) <- paste0("vld_slope_", noquote(colnames(valid_slope))) # vld = valid
  observed_values <- cbind(observed_values, valid_slope)

  # Data validation
  # 4. DIST_FROM_SOURCE, lower_bound=0.1, upper_bound=202.8
  valid_dist_src <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_dist_src <- rbind(valid_dist_src, getValidEnvInput(observed_values$DIST_FROM_SOURCE[i],
                                                             0.1,
                                                             202.8,
                                                             "DIST_FROM_SOURCE"))
  }
  colnames(valid_dist_src) <- paste0("vld_dist_src_", noquote(colnames(valid_dist_src))) # vld = valid
  observed_values <- cbind(observed_values, valid_dist_src)

  # Data validation
  # 5. ALTITUDE, has two sets of bounds, lower_bound=1, upper_bound=590, lower_low_bound=0, upper_up_bound = 1345
  # [0,1345] are hard coded, could be parameterised QED
  valid_altitude <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_altitude <- rbind(valid_altitude, getAltitude(observed_values$ALTITUDE[i],
                                                        1,
                                                        590))
  }
  colnames(valid_altitude) <- paste0("vld_alt_src_", noquote(colnames(valid_altitude))) # vld = valid
  observed_values <- cbind(observed_values, valid_altitude)
  #
  #
  # # Data validation
  # # 6. ALKALINITY, has bounds, lower_bound=1.2, upper_bound=366
  # # getLogAlkalinity <- function (hardness, calcium, conduct, alkal, lower_b, upper_b)
  #
  valid_alkalinity <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_alkalinity <- rbind(valid_alkalinity, getLogAlkalinity(observed_values$HARDNESS[i],
                                                                 observed_values$CALCIUM[i],
                                                                 observed_values$CONDUCTIVITY[i],
                                                                 observed_values$ALKALINITY[i],
                                                                 1.2,
                                                                 366))
  }
  colnames(valid_alkalinity) <- paste0("vld_alkal_", noquote(colnames(valid_alkalinity))) # vld = valid
  observed_values <- cbind(observed_values, valid_alkalinity)

  # Data validation
  # 7. Validate SUBSTRATUM for sum of values "TOTSUB" in interval [97,103] exclussive,and MSUBSTR in
  # interval [-8, 8]. Write to a file if errors found
  # Remove the site or records with such errors, and continue the prediction
  # Note that we don't use log for calculation of substrate
  valid_substrate <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_substrate <- rbind(valid_substrate, getSubstrate(observed_values$BOULDER_COBBLES[i],
                                                           observed_values$PEBBLES_GRAVEL[i],
                                                           observed_values$SAND[i],
                                                           observed_values$SILT_CLAY[i],
                                                           97,
                                                           103))
  }
  colnames(valid_substrate) <- paste0("vld_substr_", noquote(colnames(valid_substrate))) # vld = valid
  observed_values <- cbind(observed_values, valid_substrate)

  # Data validation and conversion
  # 8. Discharge category, bounds [0, 10]. Discharge calculated from velocity if not provided using width, depth
  valid_discharge <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_discharge <- rbind(valid_discharge, getLogDischarge(observed_values$MEAN_DEPTH[i],
                                                              observed_values$MEAN_WIDTH[i],
                                                              observed_values$DISCHARGE [i],
                                                              observed_values$VELOCITY[i],
                                                              0,
                                                              10))
  }
  colnames(valid_discharge) <- paste0("disch_", noquote(colnames(valid_discharge)))
  observed_values <- cbind(observed_values, valid_discharge)
  # Data validation and conversion
  # 9. Calculation of Lat/Long, and validation of LAT, LONG
  # Calculation of Lat/Long using bng (British National Grids)
  # Use function getLatLong
  lat_long <- with(observed_values, getLatLong(NGR, EASTING, NORTHING, "WGS84"))

  ## Calculate Longitude ##
  observed_values$LONGITUDE <- lat_long$lon
  valid_longitude <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_longitude <- rbind(valid_longitude, getLongitude(observed_values$LONGITUDE[i],
                                                           -8,
                                                           1.4))
  }
  colnames(valid_longitude) <- paste0("vld_long_src_", noquote(colnames(valid_longitude))) # vld = valid
  observed_values <- cbind(observed_values, valid_longitude)

  ## Calculate Latitude ##
  observed_values$LATITUDE <- lat_long$lat
  valid_latitude <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)){
    valid_latitude <- rbind(valid_latitude, getLatitude(observed_values$LATITUDE[i],
                                                        50.8,
                                                        52))
  }
  colnames(valid_latitude) <- paste0("vld_lat_src_", noquote(colnames(valid_latitude))) # vld = valid
  observed_values <- cbind(observed_values, valid_latitude)

  # Data validation and conversion
  # 10. Calculation of mean temperature (TMEAN), range temperature (TRANGE), using
  # function calcTemps() from package "rnfra"
  # Use function getbng
  bng <- with(observed_values, getBNG(NGR, EASTING, NORTHING, "BNG"))

  # Lat long used for temperature lookups, using mean-air-temp-range.R helper function
  my_temperatures <- calcTemps(coordinates = data.frame(
    Site_ID = observed_values$SITE,
    Easting4 = bng$easting / 100,
    Northing4 = bng$northing / 100,
    stringsAsFactors = FALSE),
    air_temp_grid)
  # Assign to variables as appropriate
  observed_values$TMEAN <- my_temperatures$TMEAN
  observed_values$TRANGE <- my_temperatures$TRANGE

  # Data validation and conversion
  # 12. Write to file all Warnings and Failrures: SITE, MSG, iterate through the list of all variables with vld
  # WRITE TO LOG FILES all Warnings and Errors
  # 1. Warnings to log file :1
  #
  # Deal with all warnings, save them in a file
  # Same as above, but using pipes, and using all the variables
  msg_columns <- names(dplyr::select(observed_values, dplyr::ends_with("_msg")))
  this_warning <- observed_values
  this_warning <- dplyr::filter(this_warning,
            substr(.data$vld_alt_src_msg, 1, 5) == "Warn:"    | substr(.data$mn_width_msg, 1, 5) == "Warn:"
            | substr(.data$mn_depth_msg, 1, 5) == "Warn:"     | substr(.data$vld_alkal_msg, 1, 5) == "Warn:"
            | substr(.data$disch_msg, 1, 5) == "Warn:"        | substr(.data$vld_substr_msg, 1, 5) == "Warn:"
            | substr(.data$vld_dist_src_msg, 1, 5) == "Warn:" | substr(.data$vld_slope_msg, 1, 5) == "Warn:"
            | substr(.data$vld_lat_src_msg, 1, 5) == "Warn:"  | substr(.data$vld_long_src_msg, 1, 5) == "Warn:")
  #  select("SITE","YEAR",msg_columns) # Select some columns
  # write.csv(this_warning, file = paste0(path,"/Warnings_file_data.csv"))
  # which rows are these
  # observed_values[which(this_warning[1,1] %in% observed_values[,c("SITE")]),]

  # 2. Failings to log file
  # Deal with all failings, save them in a file
  this_failing <- observed_values
  this_failing <- dplyr::filter(this_failing,
            substr(.data$vld_alt_src_msg, 1, 5) == "Fail:"  | substr(.data$mn_width_msg, 1, 5) == "Fail:"
            | substr(.data$mn_depth_msg, 1, 5) == "Fail:"     | substr(.data$vld_alkal_msg, 1, 5) == "Fail:"
            | substr(.data$disch_msg, 1, 5) == "Fail:"        | substr(.data$vld_substr_msg, 1, 5) == "Fail:"
            | substr(.data$vld_dist_src_msg, 1, 5) == "Fail:" | substr(.data$vld_slope_msg, 1, 5) == "Fail:"
            | substr(.data$vld_lat_src_msg, 1, 5) == "Fail:"  | substr(.data$vld_long_src_msg, 1, 5) == "Fail:")
  # select("SITE","YEAR",msg_columns) # Select some columns
  # # write.csv(this_failing, file = paste0(path,"/Failings_file_data.csv"))
  # Put warnings and failures in a file of warnings_failings
  warnings_failings <- rbind(this_warning, this_failing)
  return(list(warnings_failings, this_failing, observed_values))
}
