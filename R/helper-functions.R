# Helper functions for calculating, formatting, validating predictor variables
# for RIVPACS III+

# get_alkalinity: This function takes in the input dataframe to rict_validate
# and calculates alkalinity from hardness, calcium or conductivity when
# supplied, else returns the input dataframe with the alkalinity provided.
get_alkalinity <- function(data) {
  names(data) <- toupper(names(data))
  if (all(is.na(data$HARDNESS)) &
      all(is.na(data$CALCIUM)) &
      all(is.na(data$CONDUCTIVITY)) &
      all(is.na(data$ALKALINITY))
  ) {
    stop("You provided empty ALKALINITY, HARDNESS, CONDUCTIVITY and CALCIUM values,
       we expect values for at least one of these variables. ", call. = FALSE)
  } else { # loop through rows and calculate Alkalinity

  alkalinity <- lapply(split(data, paste(data$SITE, data$YEAR)), function(data_row) {
    if (!any(is.null(data_row$HARDNESS)) && !any(is.na(data_row$HARDNESS))) {
      data_row$ALKALINITY <- 4.677 + 0.6393 * data_row$HARDNESS
      message(paste0(
        "Using Hardness value to calculate Alkalinity at ",
        data_row$SITE, " - ", data_row$YEAR, ". "
      ))
    }
    else
      if (!any(is.null(data_row$CALCIUM)) && !any(is.na(data_row$CALCIUM))) {
        data_row$ALKALINITY <- 14.552 + 1.7606 * data_row$CALCIUM
        message(paste0(
          "Using Calcium value to calculate Alkalinity at ",
          data_row$SITE, " - ", data_row$YEAR, ". "
        ))
      }
    else
      if (!any(is.null(data_row$CONDUCTIVITY)) && !any(is.na(data_row$CONDUCTIVITY))) {
        data_row$ALKALINITY <- 0.3201 * data_row$CONDUCTIVITY - 8.0593
        message(paste0(
          "Using Conductivity value to calculate Alkalinity at ",
          data_row$SITE, " - ", data_row$YEAR, ". "
        ))
      }
    return(data_row)
  })
  alkalinity <- dplyr::bind_rows(alkalinity)
  # Keep order and row.names the same as original input data for consistent output
  data <- alkalinity[order(match(alkalinity[, "SITE"], data[, "SITE"])), ]
  row.names(data) <- seq_len(nrow(data))
  }
  return(data)
}

# Calculate total substrate and phi grain size scale
get_substrate <- function(data) {
  data$TOTSUB <- rowSums(data[, c("BOULDER_COBBLES", "PEBBLES_GRAVEL", "SILT_CLAY", "SAND")])
  data$MSUBST <- ((-7.75 * data$BOULDER_COBBLES) - (3.25 * data$PEBBLES_GRAVEL) +
                    (2 * data$SAND) + (8 * data$SILT_CLAY)) / data$TOTSUB
  # re-assign substrate variable to match with prediction function requirements
  data$vld_substr_log <- data$MSUBST
  return(data)
}

# Get latLong, use package rnrfa for function osg_parse
getLatLong <- function(nat_grid_ref, easting, northing, coordsys_latlon, area) {
  lat_long <- NA
  concat_bng <- paste(nat_grid_ref, substr(easting, 1, 3), substr(northing, 1, 3), sep = "")
  if (area == "ni") {
    # Add "I" for NOrthern Ireland
    lat_long <- osg_parse(paste0("I", concat_bng), coord_system = coordsys_latlon)
  } else {
    lat_long <- osg_parse(concat_bng, coord_system = coordsys_latlon)
  }
  return(lat_long)
}

getLatLong_NI <- function(x, y) {
  # Declare the lat_long
  lat_long_all <- NULL
  # It is assumed the East/North have 5 digits, just add a ZERO at the end
  xx <- as.numeric(paste0(x, 0))
  yy <- as.numeric(paste0(y, 0))

  # Loop through the codes to extract the Easting and Northing
  for (i in 1:length(x)) {
    xy <- data.frame(easting_x = xx[i], northing_y = yy[i]) # Edited, just to give site identifier
    # 1. create sf object in Irish National Grid (CRS 29903)
    irish.ng <- sf::st_as_sf(xy, coords = c("easting_x", "northing_y"), crs = 29903)
    lat_long <- sf::st_transform(irish.ng, crs = 4326)
    results <- c(lat_long$geometry[[1]][[2]], lat_long$geometry[[1]][1])
    lat_long_all <- rbind(lat_long_all, results)
  }
  # Remove row names
  rownames(lat_long_all) <- c()
  # names columns appropriately as "Easting",and "Northing"
  colnames(lat_long_all) <- c("Latitude", "Longitude")
  return(as.data.frame(lat_long_all))
}

# Note :
# Can view all Grid references with "SE..." from  allStations <- catalogue(), using the rNRFA package
# this <- filter(allStations, grepl("SE", gridReference, fixed = TRUE))$gridReference
# sort(this, decresing  = TRUE)

# Get BNG
getBNG <- function(nat_grid_ref, easting, northing, coordsys_bng) {
  lat_long <- NA
  concat_bng <- paste(nat_grid_ref, substr(easting, 1, 3), substr(northing, 1, 3), sep = "")
  new_bng <- osg_parse(grid_refs = concat_bng, coord_system = coordsys_bng)
  return(new_bng)
}

# Calculate discharge from velocity and river width if discharge category not supplied
get_discharge <- function(data) {
  # Calculate discharge category from velocity and width if required
  discharge_categories <- c(0.31, 0.62, 1.25, 2.5, 5.0, 10.0, 20.0, 40.0, 80.0, 1000000)
  velocity_categories <- c(5.0, 17.5, 37.5, 75.0, 150.0, 1000000)

  discharge <- lapply(split(data, paste(data$SITE, data$YEAR)), function(data_row) {
    if (!any(is.null(data_row$VELOCITY)) && !any(is.na(data_row$VELOCITY))) {
      discharge_value <- data_row$MEAN_DEPTH / 100 *
        data_row$MEAN_WIDTH *
        velocity_categories[data_row$VELOCITY] / 100
      data_row$DISCHARGE <- min(which(discharge_categories > discharge_value))
      message("Using velocity, width and depth to calculate discharge category")
    }
    # hack - to avoid errors if some VELOCITY rows are NA - but avoids velocity validation rules..
    data_row$VELOCITY <- NULL
    return(data_row)
  })
  discharge <- dplyr::bind_rows(discharge)
  # Keep order and row.names the same as original input data for consistent output
  data <- discharge[order(match(discharge[, "SITE"], data[, "SITE"])), ]
  row.names(data) <- seq_len(nrow(data))
  return(data)
}

