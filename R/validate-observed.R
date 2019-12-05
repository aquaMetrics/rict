#' Validate observed values
#'
#' Low level function to validate input `data`. Returns list of dataframes
#' containing fails, warnings and passing observed values. The validation rules
#' adjust based on the `model` and `area` arguments. Here is a summary of
#' checks:
#' \enumerate{
#'  \item Input 'data' exists
#'  \item Input is dataframe
#'  \item Required columns are present
#'  \item Columns have correct class
#'  \item Conditional columns if present are correct class
#'  \item Where multiple classes are allowed, convert columns to
#'  standardised class for example integer to numeric.
#'  \item Suspected input errors are corrected for example `0` slope.
#'  \item Additional columns/variables calculated for example mean temperate.
#'  \item Logs failures and warnings applied using `validation_rules` table.
#'  \item
#'  \item Replacements made to very low values for example
#'  \item Returns dataframe with passing values, warnings, fails
#' }
#' See `validation` vignette for full details.
#'
#' @param data dataframe of observed envornmental variables
#' \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'   ...
#' }
#'
#' @param model Specify model to apply correct validation rules, default is
#'   "physical".
#' @param area Specify area of UK to apply localised validation rules, default
#'   is "gb". Accepts "ni" (Northern Ireland) or "gb" (Great Britain).
#'
#' @return List of three dataframes: 1. warnings_failings, 2. this_failing 3.
#'   valid 'data'.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' validations <- rict_validate(data = rict::demo_data)
#' }
rict_validate <- function(data = NULL, model = "physical", area = "gb") {
  # check params -------------------------------------------------------
  if (is.null(model) | !model %in% c("physical", "gis")) {
    stop("The 'model' argument is '", model, "'
       We expected 'model' to be 'physical' or 'gis'.", call. = FALSE)
  }
  if (is.null(area) | !area %in% c("gb", "ni")) {
    stop("The 'area' argument is '", model, "'
       We expected 'area' to be 'gb' or 'ni'.", call. = FALSE)
  }
  if (is.null(data)) {
    stop("Can't find 'data' object.
       We expected 'data' with environmental values.", call. = FALSE)
  }
  # Check input is dataframe ---------------------------------------------
  if (class(data) != "data.frame") {
    stop("You provided 'data' object with class '", class(data), "'.
       We expect 'data' to have class 'data.frame'
       Hint: Does your 'data' object contain your observed environmental
       values?", call. = FALSE)
  }
  # State which parameters are being checked for clarity --------------------------------------
  message("Validating data using rules for '", model, "' model...")

  # Load validation rules
  area_selected <- area # re-assigning due to issue with filter column and varibale sharing same name
  validation_rules <-
    utils::read.csv(system.file("extdat", "validation-rules.csv", package = "rict"),
      stringsAsFactors = F
    ) %>%
    # filter rules based on which model and area selected
    dplyr::filter(area %in% c(area_selected, "all")) %>%
    dplyr::filter(models %in% c(model, "all"))

  # Standardise all column names to uppercase -------------------------------------------------
  names(data) <- toupper(names(data))
  names(validation_rules$variable) <- toupper(validation_rules$variable)

  ## Check column names correct (additional columns are allowed) -------------------------------
  if (all(validation_rules$variable[validation_rules$source == "input"] %in%
    names(data)) == FALSE) {
    stop(
      "Can't find these columns in data: ",
      paste("\n", validation_rules$variable[!validation_rules$variable %in%
        names(data)]),
      call. = FALSE
    )
  }

  ## Check class of each column is correct
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
            "', we expect class '", rule$type, "'"
          ))
        }
      }
    }
  )
  # stop process is any incorrect classes found
  fails <- Filter(Negate(is.null), fails)
  if (length(fails) != 0) {
    stop(fails, call. = FALSE)
  }
  # ------------------------------------------------------------
  # Check columns that may or may not be provided but at  -
  if (model == "physical") {
    if (all(c(is.null(data$DISCHARGE), is.null(data$VELOCITY))) == TRUE) {
      stop("You have not provided VELOCITY and DISCHARGE columns,
          we expect both these columns")
    }

    if (all(is.na(data$DISCHARGE)) &
      all(is.na(data$VELOCITY))) {
      stop("You provided empty VELOCITY and DISCHARGE values,
          we expect values for at least one of these variables")
    }

    if (all(!is.na(data$DISCHARGE)) &
      all(!is.na(data$VELOCITY))) {
      warning("You provided both VELOCITY and DISCHARGE values,
          DISCHARGE will be used by default")
      data$VELOCITY <- NULL
    }
  }

  # Add calculated columns ------------------------------------------------------------
  # 1. TEMPM
  # 2. TEMPR
  #  etc...

  # Convert to character as required by specification ---------------------------------------
  data$SITE <- as.character(data$SITE)
  data$EASTING <- as.character(data$EASTING)
  data$NORTHING <- as.character(data$NORTHING)

  # check for length <5, add a "0" to get proper Easting/Northing 5 digit codes --------------
  data$EASTING <- getCorrectCodes(data$EASTING)
  data$NORTHING <- getCorrectCodes(data$NORTHING)

  ## Check each value in observed values passes validation rules -----------------------------
  # Loop through each variable in rules dataframe
  message("Applying data validation rules for '", toupper(area), "' area...")
  ################## remove "input" filter once ALL calculated columns are added above ! ################
  validation_rules <- validation_rules[validation_rules$source == "input", ]
  checks <- lapply(split(
    validation_rules,
    validation_rules$variable
  ), function(rule) {
    # find matching column in data
    values <- data[, c(rule$variable, "SITE", "YEAR")]
    # skip column if contains only NA values - e.g. HARDNESS - this
    # column will be class logical and not as expected by validation rules
    if (!all(is.na(values[, rule$variable]))) {
      # loop through all values in column
      checks <- lapply(split(values, row.names(values)), function(value) {
        # make dataframe to hold checks
        checks <- data.frame(
          "SITE" = "",
          "YEAR" = "",
          "fail" = "",
          stringsAsFactors = F
        )
        # check for less than fails
        if (is.na(rule$less_than_fail) == FALSE) {
          if (value[, rule$variable] < rule$less_than_fail) {
            checks <- dplyr::bind_rows(
              checks,
              test <- data.frame(
                "SITE" = value$SITE,
                "YEAR" = as.character(value$YEAR),
                "fail" = paste0(
                  "Fail, you provided ", names(value)[1], ": ", value[, rule$variable],
                  ", expected value greater than ", rule$less_than_fail
                ),
                stringsAsFactors = F
              )
            )
          }
        }
        # check for greater than fails
        if (is.na(rule$greater_than_fail) == FALSE) {
          if (value[, rule$variable] > rule$greater_than_fail) {
            checks <- dplyr::bind_rows(
              checks,
              data.frame(
                "SITE" = as.character(value$SITE),
                "YEAR" = as.character(value$YEAR),
                "fail" =
                  paste0(
                    "Fail, you provided ", names(value)[1], ": ", value[, rule$variable],
                    ", expected value less than ", rule$greater_than_fail
                  ),
                stringsAsFactors = F
              )
            )
          }
        }


        return(checks)
      })
      return(dplyr::bind_rows(checks))
    }
  })

  # log10 values
  log_rules <- validation_rules[validation_rules$log == TRUE, ]
  columns <- lapply(split(log_rules, row.names(log_rules)),
               function(variable) {
                 log_col_name <- variable$log_col_name
               data[, log_col_name] <- log10(data[, variable$variable])
              column <- data.frame(data[, log_col_name])
              names(column) <- log_col_name
      return(column)
  })

  columns <- dplyr::bind_cols(columns)
  data <- dplyr::bind_cols(data, columns)
  checks <- dplyr::bind_rows(checks[[1]])
  if (length(checks$fail[checks$fail != ""]) > 0) {
    message(paste(checks$SITE, checks$YEAR, checks$fail, "\n"))
  }
  if (model == "physical") {
    # 1. MEAN_WIDTH, lower_bound=0.4, upper_bound=117
    # head(getValidEnvInput(data$MEAN_WIDTH[10], 0.4, 117, "MEAN_WIDTH"),5)
    valid_mean_width <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_mean_width <- rbind(valid_mean_width, getValidEnvInput(
        data$MEAN_WIDTH[i],
        0.4,
        117,
        "MEAN_WIDTH"
      ))
    }

    # Change column names to suit env variable name, and cbind to original dataset
    colnames(valid_mean_width) <- paste0("mn_width_", noquote(colnames(valid_mean_width)))
    data <- cbind(data, valid_mean_width)

    # # Data validation
    # # 2. MEAN_DEPTH, lower_bound=1.7, upper_bound=300
    valid_mean_depth <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_mean_depth <- rbind(valid_mean_depth, getValidEnvInput(
        data$MEAN_DEPTH [i],
        1.7,
        300,
        "MEAN_DEPTH"
      ))
    }
    colnames(valid_mean_depth) <- paste0("mn_depth_", noquote(colnames(valid_mean_depth)))
    data <- cbind(data, valid_mean_depth)

    # Data validation
    # 3. SLOPE, lower_bound=0.1, upper_bound=150
    valid_slope <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_slope <- rbind(valid_slope, getValidEnvInput(
        data$SLOPE[i],
        0.1,
        150,
        "SLOPE"
      ))
    }
    colnames(valid_slope) <- paste0("vld_slope_", noquote(colnames(valid_slope))) # vld = valid
    data <- cbind(data, valid_slope)

    # Data validation
    # 4. DIST_FROM_SOURCE, lower_bound=0.1, upper_bound=202.8
    valid_dist_src <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_dist_src <- rbind(valid_dist_src, getValidEnvInput(
        data$DIST_FROM_SOURCE[i],
        0.1,
        202.8,
        "DIST_FROM_SOURCE"
      ))
    }
    colnames(valid_dist_src) <- paste0("vld_dist_src_", noquote(colnames(valid_dist_src))) # vld = valid
    data <- cbind(data, valid_dist_src)

    # Data validation
    # 5. ALTITUDE, has two sets of bounds, lower_bound=1, upper_bound=590, lower_low_bound=0, upper_up_bound = 1345
    # [0,1345] are hard coded, could be parameterised QED
    valid_altitude <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_altitude <- rbind(valid_altitude, getAltitude(
        data$ALTITUDE[i],
        1,
        590
      ))
    }
    colnames(valid_altitude) <- paste0("vld_alt_src_", noquote(colnames(valid_altitude))) # vld = valid
    data <- cbind(data, valid_altitude)
    #
    #
    # # Data validation
    # # 6. ALKALINITY, has bounds, lower_bound=1.2, upper_bound=366
    # # getLogAlkalinity <- function (hardness, calcium, conduct, alkal, lower_b, upper_b)
    #
    valid_alkalinity <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_alkalinity <- rbind(valid_alkalinity, getLogAlkalinity(
        data$HARDNESS[i],
        data$CALCIUM[i],
        data$CONDUCTIVITY[i],
        data$ALKALINITY[i],
        1.2,
        366
      ))
    }
    colnames(valid_alkalinity) <- paste0("vld_alkal_", noquote(colnames(valid_alkalinity))) # vld = valid
    data <- cbind(data, valid_alkalinity)

    # Data validation
    # 7. Validate SUBSTRATUM for sum of values "TOTSUB" in interval [97,103] exclussive,and MSUBSTR in
    # interval [-8, 8]. Write to a file if errors found
    # Remove the site or records with such errors, and continue the prediction
    # Note that we don't use log for calculation of substrate
    valid_substrate <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_substrate <- rbind(valid_substrate, getSubstrate(
        data$BOULDER_COBBLES[i],
        data$PEBBLES_GRAVEL[i],
        data$SAND[i],
        data$SILT_CLAY[i],
        97,
        103
      ))
    }
    colnames(valid_substrate) <- paste0("vld_substr_", noquote(colnames(valid_substrate))) # vld = valid
    data <- cbind(data, valid_substrate)

    # Data validation and conversion
    # 8. Discharge category, bounds [0, 10]. Discharge calculated from velocity if not provided using width, depth
    valid_discharge <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_discharge <- rbind(valid_discharge, getLogDischarge(
        data$MEAN_DEPTH[i],
        data$MEAN_WIDTH[i],
        data$DISCHARGE [i],
        data$VELOCITY[i],
        0,
        10
      ))
    }
    colnames(valid_discharge) <- paste0("disch_", noquote(colnames(valid_discharge)))
    data <- cbind(data, valid_discharge)
  }
  browser()
    # Data validation and conversion
    # 9. Calculation of Lat/Long, and validation of LAT, LONG
    # Calculation of Lat/Long using bng (British National Grids)
    # Use function getLatLong
    lat_long <- with(data, getLatLong(NGR, EASTING, NORTHING, "WGS84"))

    ## Calculate Longitude ##
    data$LONGITUDE <- lat_long$lon
    valid_longitude <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_longitude <- rbind(valid_longitude, getLongitude(
        data$LONGITUDE[i],
        -8,
        1.4
      ))
    }
    colnames(valid_longitude) <- paste0("vld_long_src_", noquote(colnames(valid_longitude))) # vld = valid
    data <- cbind(data, valid_longitude)

    ## Calculate Latitude ##
    data$LATITUDE <- lat_long$lat
    valid_latitude <- data.frame(log = as.numeric(), msg = as.character())
    for (i in seq_len(nrow(data))) {
      valid_latitude <- rbind(valid_latitude, getLatitude(
        data$LATITUDE[i],
        50.8,
        52
      ))
    }
    colnames(valid_latitude) <- paste0("vld_lat_src_", noquote(colnames(valid_latitude))) # vld = valid
    data <- cbind(data, valid_latitude)

    # Data validation and conversion
    # 10. Calculation of mean temperature (TMEAN), range temperature (TRANGE), using
    # function calcTemps() from package "rnfra"
    # Use function getbng
    bng <- with(data, getBNG(NGR, EASTING, NORTHING, "BNG"))

    # Lat long used for temperature lookups, using mean-air-temp-range.R helper function
    my_temperatures <- calcTemps(coordinates = data.frame(
      Site_ID = data$SITE,
      Easting4 = bng$easting / 100,
      Northing4 = bng$northing / 100,
      stringsAsFactors = FALSE
    ))
    # Assign to variables as appropriate
    data$TMEAN <- my_temperatures$TMEAN
    data$TRANGE <- my_temperatures$TRANGE

    if (model == "gis"){
      return(list(data.frame(),data.frame(), data))
    }

    # Data validation and conversion
    # 12. Write to file all Warnings and Failrures: SITE, MSG, iterate through the list of all variables with vld
    # WRITE TO LOG FILES all Warnings and Errors
    # 1. Warnings to log file :1
    #
    # Deal with all warnings, save them in a file
    # Same as above, but using pipes, and using all the variables
    msg_columns <- names(dplyr::select(data, dplyr::ends_with("_msg")))
    this_warning <- data
    this_warning <- dplyr::filter(
      this_warning,
      substr(.data$vld_alt_src_msg, 1, 5) == "Warn:" | substr(.data$mn_width_msg, 1, 5) == "Warn:"
      | substr(.data$mn_depth_msg, 1, 5) == "Warn:" | substr(.data$vld_alkal_msg, 1, 5) == "Warn:"
      | substr(.data$disch_msg, 1, 5) == "Warn:" | substr(.data$vld_substr_msg, 1, 5) == "Warn:"
      | substr(.data$vld_dist_src_msg, 1, 5) == "Warn:" | substr(.data$vld_slope_msg, 1, 5) == "Warn:"
      | substr(.data$vld_lat_src_msg, 1, 5) == "Warn:" | substr(.data$vld_long_src_msg, 1, 5) == "Warn:"
    )
    #  select("SITE","YEAR",msg_columns) # Select some columns
    # write.csv(this_warning, file = paste0(path,"/Warnings_file_data.csv"))
    # which rows are these
    # data[which(this_warning[1,1] %in% data[,c("SITE")]),]

    # 2. Failings to log file
    # Deal with all failings, save them in a file
    this_failing <- data
    this_failing <- dplyr::filter(
      this_failing,
      substr(.data$vld_alt_src_msg, 1, 5) == "Fail:" | substr(.data$mn_width_msg, 1, 5) == "Fail:"
      | substr(.data$mn_depth_msg, 1, 5) == "Fail:" | substr(.data$vld_alkal_msg, 1, 5) == "Fail:"
      | substr(.data$disch_msg, 1, 5) == "Fail:" | substr(.data$vld_substr_msg, 1, 5) == "Fail:"
      | substr(.data$vld_dist_src_msg, 1, 5) == "Fail:" | substr(.data$vld_slope_msg, 1, 5) == "Fail:"
      | substr(.data$vld_lat_src_msg, 1, 5) == "Fail:" | substr(.data$vld_long_src_msg, 1, 5) == "Fail:"
    )
    # select("SITE","YEAR",msg_columns) # Select some columns
    # # write.csv(this_failing, file = paste0(path,"/Failings_file_data.csv"))
    # Put warnings and failures in a file of warnings_failings
    warnings_failings <- rbind(this_warning, this_failing)
    # message(knitr::kable(warnings_failings, widetable = TRUE))
    if (length(warnings_failings) == 0) {
      message("Success, all validations passed!", all. = FALSE)
    }
    else if (length(this_warning) > 0) {
      #  options(warn = warning(knitr::kable(warnings_failings, widetable = TRUE)))
      # warnings <- function() warning(knitr::kable(warnings_failings, widetable = TRUE))
      warning("Some values outside expected, see warnings()", call. = FALSE)
    } else {
      warning("Some values failed validation, see warnings()", call. = FALSE)
    }
    return(list(warnings_failings, this_failing, data))

}
