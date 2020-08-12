#' Validate observed values
#'
#' Low level function to validate input `data`. Returns list of dataframes
#' containing fails/warnings and passing observed values. The function detects
#' which model (gis or physical) and area (GB & NI) and applies validation rules
#' as required. Here is a summary of checks:
#' \enumerate{
#'  \item Input 'data' exists.
#'  \item Input is dataframe.
#'  \item Required columns are present.
#'  \item Columns have correct class.
#'  \item Conditional columns if present are correct class.
#'  \item Where multiple classes are allowed, convert columns to
#'  standardised class for example integer to numeric.
#'  \item Additional columns/variables calculated for example mean temperate.
#'  \item Logs failures and warnings applied using `validation_rules` table.
#'  \item Replace values if value is less than the ‘overall’ minimum value.
#'  \item Returns dataframe with passing values, warnings, fails.
#' }
#' See `validation` vignette for full details.
#'
#' @param data dataframe of observed environmental variables
#' \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'   ...
#' }
#'
#'
#' @return List of three dataframes: 1. warnings_failings, 2. this_failing 3.
#'   valid 'data'.
#' @export
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' validations <- rict_validate(demo_observed_values)
#' }
rict_validate <- function(data = NULL) {
  ### Sense checks --------------------------------------------------------------------
  # check data object provided  -
  if (is.null(data)) {
    stop("Can't find 'data' object.
       We expected 'data' with environmental values.", call. = FALSE)
  }
  # Check data object is a dataframe
  if (class(data) != "data.frame") {
    stop("You provided 'data' object with class '", class(data), "'.
       We expect 'data' to have class 'data.frame'
       Hint: Does your 'data' object contain your observed environmental
       values? ", call. = FALSE)
  }
  # Load validation rules
  validation_rules <-
    utils::read.csv(system.file("extdat", "validation-rules.csv", package = "rict"),
      stringsAsFactors = F
    )
  # Standardise all column names to uppercase
  names(data) <- toupper(names(data))
  names(validation_rules$variable) <- toupper(validation_rules$variable)

  # Check data contains at least some required column names
  if (dplyr::filter(validation_rules, variable %in% names(data)) %>% nrow() < 1) {
    stop("The data provided contains none of the required column names
          Hint: Double-check your data  contains correct column names. ", call. = FALSE)
  }
  # Check if data contains variable names for more than one model type
  models <- c("gis", "physical")
  check_models <- lapply(models, function(model) {
    ifelse(any(names(data) %in% validation_rules$variable[validation_rules$models == model]),
      TRUE, FALSE
    )
  })
  # Predictor variables provided for more than one model? --------------------------------------
  # For example, input data has columns for GIS and Physical models
  # check if variables contain values or are empty
  if (length(check_models[check_models == T]) > 1) {
    data_present <- lapply(models, function(model) {
      model_variables <- validation_rules$variable[
        validation_rules$models %in% model &
          validation_rules$source == "input" &
          validation_rules$optional == F &
          validation_rules$shared == F
      ]

      model_data <- suppressWarnings(dplyr::select(data, dplyr::one_of(toupper(model_variables))))

      ifelse(nrow(model_data %>% na.omit()) > 0 & ncol(model_data %>% na.omit()) > 0, TRUE, FALSE)
    })
    # If values provided for more than one model then stop.
    if (length(data_present[data_present == T]) > 1) {
      stop("The data provided contains values for more than one model
        Hint: Check your data contains values for a single model type only: ",
        paste(c(models), collapse = " or "), ". ",
        call. = FALSE
      )
    }
  }
  # Create variable for data model detected  --------------------------------------------
  model <- data.frame(cbind(models, data_present))
  model <- model$models[model$data_present == T]

  # Display which model type has been detected
  message("Variables for the '", model, "' model detected - applying relevant checks. ")

  # Detect NI / GB grid references -----------------------------------------------------
  areas <- unique(ifelse(grepl(pattern = "^.[A-Z]", toupper(data$NGR)), "gb", "ni"))
  if (length(areas) > 1) {
    stop("The data provided contains more than one area of the UK.
        Hint: Check your data contains NGR grid letters for either: ",
      paste(c(toupper(areas)), collapse = " or "), ". ",
      call. = FALSE
    )
  } else {
    area <- areas
  }

  # Display which model area has been detected
  message("Grid reference values detected for '", toupper(area), "' - applying relevant checks.")

  # Re-assigning area due to issue with filtering column and variable sharing same name
  area_selected <- area

  # Filter rules based on which model and area selected ---------------------------
  validation_rules <-
    dplyr::filter(validation_rules, area %in% c(area_selected, "all")) %>%
    dplyr::filter(models %in% c(model, "all"))

  # Check column names correct -----------------------------------------------------
  # Note: additional columns provided by user are allowed
  if (all(validation_rules$variable[validation_rules$source == "input"] %in%
    names(data)) == FALSE) {
    stop(
      "Can't find these columns in data: ",
      paste("\n", validation_rules$variable[!validation_rules$variable %in%
        names(data) & validation_rules$source == "input"]),
      call. = FALSE
    )
  }
  # Convert to character as required by specification
  data$SITE <- as.character(data$SITE)
  data$WATERBODY <- as.character(data$WATERBODY)

  # Check class of each column is correct ----------------------------------------
  # Loop through each 'variable' in rules dataframe
  fails <- lapply(
    split(
      validation_rules[validation_rules$source == "input", ],
      validation_rules$variable[validation_rules$source == "input"]
    ),
    function(rule) {
      # find matching column/variable in data
      values <- data[, rule$variable]
      # skip column if contains only NA values - e.g. HARDNESS - this
      # column will be class logical and not as expected by validation rules
      if (!all(is.na(values))) {
        # test class is what is expected
        if (!class(values) %in% c(rule$type, rule$fall_back_type)) {
          return(paste0(
            "You provided column '", rule$variable,
            "' with class '", class(values),
            "', we expect class '", rule$type, "'. "
          ))
        }
      }
    }
  )
  # Stop process is any incorrect classes found
  fails <- Filter(Negate(is.null), fails)
  if (length(fails) != 0) {
    stop(fails, call. = FALSE)
  }
  # Check columns that may or may not be provided --------------------------------------
  if (model == "physical") {
    if (all(is.na(data$DISCHARGE)) &
      all(is.na(data$VELOCITY))) {
      stop("You provided empty VELOCITY and DISCHARGE values,
          we expect values for at least one of these variables. ", call. = FALSE)
    }

    if (all(!is.na(data$DISCHARGE)) &
      all(!is.na(data$VELOCITY))) {
      warning("You provided both VELOCITY and DISCHARGE values,
          DISCHARGE will be used by default. ", call. = FALSE)
      # remove VELOCITY from validation rules so no rule will be applied
      validation_rules <- validation_rules[validation_rules$variable != "VELOCITY", ]
      # remove VELOCITY from input data
      data$VELOCITY <- NULL
    }
  }
  ### Add calculated variables based on input data ------------------------------------
  # Check alkalinity related columns and calculate if necessary
  if (all(is.na(data$HARDNESS)) &
    all(is.na(data$CALCIUM)) &
    all(is.na(data$CONDUCT)) &
    all(is.na(data$ALKALINITY))
  ) {
    stop("You provided empty ALKALINITY, HARDNESS, CONDUCT and CALCIUM values,
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

  # Calculate discharge category from velocity and width if required
  discharge_categories <- c(0.31, 0.62, 1.25, 2.5, 5.0, 10.0, 20.0, 40.0, 80.0, 1000000)
  velocity_categories <- c(5.0, 17.5, 37.5, 75.0, 150.0, 1000000)

  discharge <- lapply(split(data, paste(data$SITE, data$YEAR)), function(data_row) {
    if (!any(is.null(data_row$VELOCITY)) && !any(is.na(data_row$VELOCITY))) {
      discharge_value <- data_row$MEAN_DEPTH / 100 * data_row$MEAN_WIDTH * velocity_categories[data_row$VELOCITY] / 100
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
  # If model GIS - then NGR easting and northing maybe not be provided. So only calculate
  # Easting and Northings for physical data.
  if (model == "physical") {
  # Convert to character as required by specification
  data$EASTING <- as.character(data$EASTING)
  data$NORTHING <- as.character(data$NORTHING)

  # Check NGR length
  data$NGR <- as.character(data$NGR)
  data$NGR_LENGTH <- nchar(data$NGR)
  if (any(data$NGR_LENGTH > 2)) {
    stop("You provided an NGR with more than two letters,
       Hint: Check your NGR variables have less than 3 three letters. ", call. = FALSE)
  }
  # Convert to numeric in order to help validate them as numbers
  data$EASTING <- as.numeric(data$EASTING)
  data$NORTHING <- as.numeric(data$NORTHING)
  # Check for length <5, add a "0" to get proper Easting/Northing 5 digit codes
  # data$EASTING_LENGTH <- nchar(data$EASTING)
  # data$NORTHING_LENGTH <- nchar(data$NORTHING)
  if (any(is.na(data$EASTING)) | any(is.na(data$NORTHING))) {
    stop("EASTING or NORTHING value(s) have not been supplied, we expect
       all rows to have Easting and Northing values.
       Hint: Check all rows of input data have Easting and Northing values. ", call. = FALSE)
  } else {
    data$EASTING <- as.character(formatC(round(data$EASTING), width = 5, format = "d", flag = "0"))
    data$NORTHING <- as.character(formatC(round(data$NORTHING), width = 5, format = "d", flag = "0"))
  }
  }
  # Calculate Longitude & Latitude
  if (area == "gb" & model == "physical") {
    lat_long <- with(data, getLatLong(NGR, EASTING, NORTHING, "WGS84", area))
    data$LONGITUDE <- lat_long$lon
    data$LATITUDE <- lat_long$lat
  }
  if (area == "ni" & model == "physical") {
    lat_long <- with(data, getLatLong_NI(EASTING, NORTHING))
    data$LONGITUDE <- lat_long$Longitude
    data$LATITUDE <- lat_long$Latitude
  }

  if (area == "gb" & model == "gis") {
    coords <- sf::st_as_sf(data[, c("SX", "SY")], coords = c("SX", "SY"), crs = 27700)
    coords <- sf::st_transform(coords, crs = 4326)
    data$LATITUDE <-  unlist(purrr::map(coords$geometry, 2))
    data$LONGITUDE <-   unlist(purrr::map(coords$geometry, 1))
  }

  if (area == "ni" & model == "gis") {
    coords <- sf::st_as_sf(data[, c("SX", "SY")], coords = c("SX", "SY"), crs = 29903)
    coords <- sf::st_transform(coords, crs = 4326)
    data$LATITUDE <- unlist(purrr::map(coords$geometry, 2))
    data$LONGITUDE <- unlist(purrr::map(coords$geometry, 1))
  }

  if (area == "gb") {
    # Calculate Lat/Long using bng (British National Grid) - temperate lookup needs BNG
    if (model == "physical") {
    bng <- with(data, getBNG(NGR, EASTING, NORTHING, "BNG"))
    }
    if (model == "gis") {
      bng <- data.frame(
        "easting" = data$SX,
        "northing" = data$SY
      )
    }
    # Calculate mean temperature (TMEAN), range temperature (TRANGE) only if
    # users have not provided temperatures e.g. could be studying climate change etc...
    if ((is.null(data$MEAN.AIR.TEMP) | is.null(data$AIR.TEMP.RANGE)) ||
      (any(is.na(data$MEAN.AIR.TEMP)) | any(is.na(data$AIR.TEMP.RANGE)))) {
      my_temperatures <- calcTemps(data.frame(
        Site_ID = as.character(data$SITE),
        Easting4 = bng$easting / 100,
        Northing4 = bng$northing / 100,
        stringsAsFactors = FALSE
      ))

      # Add temp variables to data
      data <- dplyr::bind_cols(data, my_temperatures[, c("TMEAN", "TRANGE")])
    } else {
      data$TMEAN <- data$MEAN.AIR.TEMP
      data$TRANGE <- data$AIR.TEMP.RANGE
      warning("Your input data file includes mean temperature and/or range (MEAN.AIR.TEMP & AIR.TEMP.RANGE).
These values will be used instead of calculating them from Grid Reference values. ")
    }
  }
  # Total substrate
  if (model == "physical") {
    data$TOTSUB <- rowSums(data[, c("BOULDER_COBBLES", "PEBBLES_GRAVEL", "SILT_CLAY", "SAND")])
    data$MSUBST <- ((-7.75 * data$BOULDER_COBBLES) - (3.25 * data$PEBBLES_GRAVEL) +
      (2 * data$SAND) + (8 * data$SILT_CLAY)) / data$TOTSUB
    # re-assign substrate variable to match with prediction function requirements
    data$vld_substr_log <- data$MSUBST
  }

  # convert metres to km in Distance from source GIS attribute
  if (model == "gis") {
    data$D_F_SOURCE <- data$D_F_SOURCE / 1000
  }

  # Add log10 values where required
  log_rules <- validation_rules[validation_rules$log == TRUE, ]
  # loop through variables and add log10 variable if required
  columns <- lapply(
    split(log_rules, row.names(log_rules)),
    function(variable) {
      log_col_name <- variable$log_col_name
      data[, log_col_name] <- log10(data[, variable$variable])
      column <- data.frame(data[, log_col_name])
      names(column) <- log_col_name
      return(column)
    }
  )
  # bind log10 variables to input data
  columns <- dplyr::bind_cols(columns)
  data <- dplyr::bind_cols(data, columns)

  ### Check values pass validation rules ----------------------------------------------
  # Loop through each variable in validation rules dataframe
  checks <- lapply(split(
    validation_rules[validation_rules$variable %in% names(data), ],
    validation_rules$variable[validation_rules$variable %in% names(data)]
  ), function(rule) {
    # find matching column in input data to validation rule
    values <- data[, c(rule$variable, "SITE", "YEAR")]
    # skip column if contains only NA values - e.g. HARDNESS - this
    # column will be class logical and not as expected by validation rules
    if (!all(is.na(values[, rule$variable]))) {
      # loop through all values in column
      checks <- lapply(split(values, row.names(values)), function(value) {
        # make dataframe to hold checks
        check <- data.frame(
          "SITE" = "",
          "YEAR" = "",
          "FAIL" = "",
          "WARNING" = "",
          "REPLACEMENT" = "",
          stringsAsFactors = F
        )
        # if value not NA check for less than fails
        fails <- ""
        if (is.na(rule$less_than_fail) == FALSE) {
          if (is.na(value[, rule$variable]) || value[, rule$variable] < rule$less_than_fail) {
            fails <- c(
              fails,
              paste0(
                "You provided ", names(value)[1], ": ", value[, rule$variable],
                ", expected min value: ", rule$less_than_fail
              )
            )
          }
        }
        # Check for greater than fails
        if (is.na(rule$greater_than_fail) == FALSE) {
          if (is.na(value[, rule$variable]) || value[, rule$variable] > rule$greater_than_fail) {
            fails <- c(
              fails,
              paste0(
                "You provided ", names(value)[1], ": ", value[, rule$variable],
                ", expected max value: ", rule$greater_than_fail, ". "
              )
            )
          }
        }
        # if value not NA, then check for less than warnings
        warns <- ""
        if (is.na(rule$less_than_warn) == FALSE) {
          if (!is.na(value[, rule$variable]) & value[, rule$variable] < rule$less_than_warn) {
            warns <- c(
              warns,
              paste0(
                "You provided ", names(value)[1], ": ", value[, rule$variable],
                ", min value used to train model: ", rule$less_than_warn, ". "
              )
            )
          }
        }
        # Check for greater than warnings
        if (is.na(rule$greater_than_warn) == FALSE) {
          if (!is.na(value[, rule$variable]) & value[, rule$variable] > rule$greater_than_warn) {
            warns <- c(
              warns,
              paste0(
                "You provided ", names(value)[1], ": ", value[, rule$variable],
                ", max value used to train model: ", rule$greater_than_warn, ". "
              )
            )
          }
        }
        # check for replacement

        replacem <- ""
        if (is.na(rule$replacement) == FALSE) {
          if (!is.na(value[, rule$variable]) & rule$replacement_cond == "lessthan" &
              value[, rule$variable] <= rule$replacement_limit) {
            replacem <- c(
              replacem,
              paste0(
                "You provided ", names(value)[1], ": ", value[, rule$variable],
                ", value replaced with: ", rule$replacement_val
              )
            )
          }
          if (!is.na(value[, rule$variable]) & rule$replacement_cond == "equals" &
              value[, rule$variable] == rule$replacement_limit) {
            replacem <- c(
              replacem,
              paste0(
                "You provided ", names(value)[1], ": ", value[, rule$variable],
                ", value replaced: ", rule$replacement_val
              )
            )
          }

        }

        # bind all checks and warnings into data frame
        checks <- dplyr::bind_rows(
          check,
          test <- data.frame(
            "SITE" = value$SITE,
            "YEAR" = as.character(value$YEAR),
            "FAIL" = fails,
            "WARNING" = warns,
            "REPLACEMENT" = replacem,
            stringsAsFactors = F
          )
        )
        return(checks)
      })
      return(dplyr::bind_rows(checks))
    }
  })

  # Replace values if value is less than the ‘overall’ minimum value ---------------
  validation_rules_input <- validation_rules[validation_rules$source == "input", ]
  ALT_LIM <- validation_rules_input[validation_rules_input$variable == "ALTITUDE", "replacement_limit"]
  ALT_VAL <- validation_rules_input[validation_rules_input$variable == "ALTITUDE", "replacement_val"]
  if (any(data$ALTITUDE[!is.na(data$ALTITUDE)] == ALT_LIM)) {
    data$ALTITUDE[data$ALTITUDE == ALT_LIM] <- ALT_VAL
  }
  DFS_LIM <- validation_rules_input[validation_rules_input$variable %in%
                                      c("DIST_FROM_SOURCE", "D_F_SOURCE"), "replacement_limit"]
  if (any(data$DIST_FROM_SOURCE[!is.na(data$DIST_FROM_SOURCE)] < DFS_LIM)) {
    data$DIST_FROM_SOURCE[data$DIST_FROM_SOURCE < DFS_LIM] <- DFS_LIM
  }
  MNW_LIM <- validation_rules_input[validation_rules_input$variable == "MEAN_WIDTH", "replacement_limit"]
  if (any(data$MEAN_WIDTH[!is.na(data$MEAN_WIDTH)] < MNW_LIM)) {
    data$MEAN_WIDTH[data$MEAN_WIDTH < MNW_LIM] <- MNW_LIM
  }
  MND_LIM <- validation_rules_input[validation_rules_input$variable == "MEAN_DEPTH", "replacement_limit"]
  if (any(data$MEAN_DEPTH[!is.na(data$MEAN_DEPTH)] < MND_LIM)) {
    data$MEAN_DEPTH[data$MEAN_DEPTH < MND_LIM] <- MND_LIM
  }
  DIS_LIM <- validation_rules_input[validation_rules_input$variable %in%
                                      c("DISCHARGE", "DISCH_CAT"), "replacement_limit"]
  DIS_VAL <- validation_rules_input[validation_rules_input$variable %in%
                                      c("DISCHARGE", "DISCH_CAT"), "replacement_val"]
  if (any(data$DISCHARGE[!is.na(data$DISCHARGE)] == DIS_LIM))  {
    data$DISCHARGE[data$DISCHARGE == DIS_LIM] <- DIS_VAL
  }
  ALK_LIM <- validation_rules_input[validation_rules_input$variable == "ALKALINITY", "replacement_limit"]
  if (any(data$ALKALINITY[!is.na(data$ALKALINITY)] < ALK_LIM)) {
    data$ALKALINITY[data$ALKALINITY < ALK_LIM] <- ALK_LIM
  }
  SLP_LIM <- validation_rules_input[validation_rules_input$variable == "SLOPE", "replacement_limit"]
  SLP_VAL <- validation_rules_input[validation_rules_input$variable == "SLOPE", "replacement_val"]
  if (any(data$SLOPE[!is.na(data$SLOPE)] == SLP_LIM)) {
    data$SLOPE[data$SLOPE == SLP_LIM] <- SLP_VAL
  }

  # Bind and format checks into data frame
  checks <- dplyr::bind_rows(checks)
  checks <- checks[checks$FAIL != "" | checks$WARNING != "" | checks$REPLACEMENT != "", ]
  # if both fail and warn - then only return fail
  checks$WARNING[checks$FAIL != "" & checks$WARNING != ""] <- "---"
  checks$WARNING[checks$WARNING == ""] <- "---"
  checks$REPLACEMENT[checks$REPLACEMENT == ""] <- "---"
  checks$FAIL[checks$FAIL == ""] <- "---"
  # Print warnings and failures
  if (nrow(checks) > 0) {
    test <- checks
    row.names(test) <- NULL
    print(test)
  } else {
    message("Success, all validation checks passed!")
  }

  return(list("data" = data, "checks" = checks, "model" = model, "area" = area))
}
