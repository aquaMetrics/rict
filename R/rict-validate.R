#' Validate observed values
#'
#' Low level function to validate input `data`. Returns list of dataframes
#' containing fails/warnings/replacements and passing observed values. The
#' function detects which model (gis or physical) and area (GB & NI) and applies
#' validation rules as required. Here is a summary of checks:
#' \enumerate{
#'  \item Input 'data' exists.
#'  \item Input is dataframe.
#'  \item Required columns are present.
#'  \item Columns have correct class.
#'  \item Conditional columns if present are correct class.
#'  \item Where multiple classes are allowed, convert columns to
#'  standardised class for example integer to numeric.
#'  \item Assess if model 'gis' (model 44) or 'physical' (model 1) based on
#'  input columns.
#'  \item Assess if input 'gb' or 'ni' based grid reference.
#'  \item Additional columns/variables calculated for example mean temperate.
#'  \item Logs failures and warnings applied using `validation_rules` table.
#'  \item Replace values if input values is zero (or close to zero) to avoid
#'  log10(0) and related errors.
#'  \item Returns dataframe with passing values, notes on warnings, fails,
#'  replacements. And model and area parameters.
#' }
#'
#' @param data dataframe of observed environmental variables
#' \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'   ...
#' }
#' @param row Boolean - if set to `TRUE` returns the row number from the input file
#'   (data) for each check. This makes linking checks (fails/warns etc) to the
#'   associated row in the input data easier. This is more relevant if multiple
#'   samples from the same site and year are input as separate row to the
#'   `rict_validate`. In this case, SITE and YEAR are not enough to link
#'   validation checks to specific rows in the input data.
#'
#' @param stop_if_all_fail Boolean - if set to `FALSE` the validation function
#'   will return empty dataframe for valid `data`. This is useful if you want to
#'   run validation checks without stopping process.
#' @param area Area is by detected by default from the NGR, but you can provide
#'   the area parameter either 'iom', 'gb, 'ni' for testing purposes.
#' @param crs optionally set crs to `29903` for Irish projection system.
#' @return List of dataframes and other parameters:
#' \describe{
#'   \item{data}{Dataframe of input data that passes validation rules}
#'   \item{checks}{Dataframe listing fails, warnings and replacements}
#'   \item{model}{Returns model detected based on columns in input file}
#'   \item{area}{Returns area detected base don Grid Reference in input file}
#'    }
#' @export
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom magrittr "%>%"
#' @importFrom purrr map
#' @importFrom sf st_as_sf st_transform
#'
#' @examples
#' \dontrun{
#' validations <- rict_validate(demo_observed_values, row = TRUE)
#' }
rict_validate <- function(data = NULL,
                          row = FALSE,
                          stop_if_all_fail = TRUE,
                          area = NULL,
                          crs = NULL) {
  ### Sense checks --------------------------------------------------------------------
  # Check data object provided
  if (is.null(data)) {
    stop("Can't find 'data' object.
       We expected 'data' with environmental values.", call. = FALSE)
  }
  # Check data object is a dataframe
  if (!any(class(data) %in% "data.frame")) {
    stop("You provided 'data' object with class '", class(data), "'.
       We expect 'data' to have class 'data.frame'
       Hint: Does your 'data' object contain your observed environmental
       values? ", call. = FALSE)
  }
  # Ensure only data.frame class (could also be tibble, tbl etc at this point).
  # Unique behaviour of data.frame required later (selecting single column will
  # create a list not a data.frame)
  data <- data.frame(data, check.names = FALSE)

  # Load validation rules
  validation_rules <-
    utils::read.csv(system.file("extdat", "validation-rules.csv", package = "rict"),
      stringsAsFactors = FALSE
    )
  # Standardise all column names to uppercase
  names(data) <- toupper(names(data))
  names(validation_rules$variable) <- toupper(validation_rules$variable)

  # Check data contains at least some required column names
  if (dplyr::filter(validation_rules, .data$variable %in% names(data)) %>% nrow() < 1) {
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
  ### Predictor variables provided for more than one model? ----------------------------------------
  # For example, input data has columns for GIS and Physical models
  if (length(check_models[check_models == TRUE]) > 1) {
    data_present <- lapply(models, function(model) {
      model_variables <- validation_rules$variable[
        validation_rules$models %in% model &
          validation_rules$source == "input" &
          validation_rules$optional == FALSE &
          validation_rules$shared == FALSE
      ]

      model_data <- suppressWarnings(dplyr::select(data, dplyr::one_of(toupper(model_variables))))

      ifelse(nrow(model_data) > 0 & ncol(model_data) > 0, TRUE, FALSE)
    })
    # If values provided for more than one model then stop.
    if (length(data_present[data_present == TRUE]) > 1) {
      stop("The data provided contains values for more than one model
        Hint: Check your data contains values for a single model type only: ",
        paste(c(models), collapse = " or "), ". ",
        call. = FALSE
      )
    }
  } else {
    data_present <- FALSE
  }

  ### Create variable for data model detected  ------------------------------------------------
  model <- data.frame(cbind(models, data_present))
  model <- model$models[model$data_present == TRUE]

  # If model not detected
  if (length(model) == 0) {
    stop("You provided data without all required columns for either Model 1 or Model 44:", paste("\n", names(data)),
      paste("\n", "Hint: Check input dataset match required for either Model 1 or Model 44 input variables"),
      call. = FALSE
    )
  }
  # Display which model type has been detected
  message("Variables for the '", model, "' model detected - applying relevant checks. ")
  if (model == "physical" && is.null(area)) {
    ### Detect NI / GB grid references --------------------------------------------------------
     # Check is NGR has two letters.
     areas <- unique(ifelse(grepl(pattern = "^.[A-Z]", toupper(data$NGR)),
                            "gb", "ni"))
     # If NGR has two letters check if any letters 'I' - indicates Irish NGR.
     if(any(areas == "gb")) {
     areas <- unique(ifelse(grepl(pattern = "^[I]", toupper(data$NGR)),
                            "ni", "gb"))
     }
     # Remove optional 'I' - in Irish NGR - this is legacy of old RICT1 but some
     # users may still add this out of habit
     if(any(areas == "ni")) {
       data$NGR <- gsub("I", "", toupper(data$NGR))
     }

    if (length(areas) > 1) {
      stop("The data provided contains more than one area of the UK.
        Hint: Check your data contains NGR grid letters for either: ",
        paste(c(toupper(areas)), collapse = " or "), ". ",
        call. = FALSE
      )
    } else {
      area <- areas
    }
  } else {
    if(is.null(area)) {
    area <- "gb" # model 44 currently always "gb" but could change in future
    }
  }

  if(model == "physical") {
  # Convert to numeric in order to help validate them as numbers
  data$EASTING <- as.numeric(data$EASTING)
  data$NORTHING <- as.numeric(data$NORTHING)

  # Find Isle of Man NGRs and to apply IOM model/area
  sc_data <- dplyr::filter(data, toupper(.data$NGR) == "SC")
  sc_data <- dplyr::filter(sc_data, .data$NORTHING <= 99999)
  sc_data <- dplyr::filter(sc_data, .data$EASTING <= 55000)
  if(nrow(sc_data) > 0) {
    message("Site location detected in the Isle of Man -
              applying IOM model for all input data")
    area <- "iom"
  }

  nx_data <- dplyr::filter(data, toupper(.data$NGR) == "NX")
  nx_data <- dplyr::filter(nx_data, .data$NORTHING <= 10000)
  nx_data <- dplyr::filter(nx_data, .data$EASTING <= 55000)
  if(area != "iom" && nrow(nx_data) > 0) {
    message("Site location detected in the Isle of Man -
              applying IOM model for all input data")
    area <- "iom"
  }
  }
  # Re-assigning area due to issue with filtering column and variable sharing same name
  area_selected <- area

  ### Filter rules based on which model and area selected -------------------------------------------
  if(area != "iom") {
    validation_rules <-
    dplyr::filter(validation_rules, area %in% c(area_selected, "all")) %>%
    dplyr::filter(models %in% c(model, "all"))
  }

  if(area == "iom") {
    validation_rules <-
      dplyr::filter(validation_rules, .data$variable %in% c("SITE",
                                                      "YEAR",
                                                      "WATERBODY",
                                                      "NGR",
                                                      "EASTING",
                                                      "NORTHING",
                                                      "DIST_FROM_SOURCE",
                                                      "ALTITUDE",
                                                      "SLOPE",
                                                      "ALKALINITY"))

        validation_rules <-
                      dplyr::filter(validation_rules, area %in% c("all","iom"))


        validation_rules <-
          dplyr::filter(validation_rules, models != "gis")

  }
  ### Check column names correct ------------------------------------------------------------------
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

  # If model GIS - then NGR easting and northing maybe not be provided. So only
  # convert to character if present in data.
  if (sum(names(data) %in% c("EASTING", "NORTHING")) == 2) {
    # Convert to character as required by specification
    data$EASTING <- as.character(data$EASTING)
    data$NORTHING <- as.character(data$NORTHING)
  }
  ### Check class of each column is correct ------------------------------------
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
  ### Check columns that may or may not be provided -----------------------------------------
  if (model == "physical" && area != "iom") {
    if (all(!is.na(data$DISCHARGE)) &&
      all(!is.na(data$VELOCITY))) {
      warning("You provided both VELOCITY and DISCHARGE values,
          DISCHARGE will be used by default. ", call. = FALSE)
      # Store VELOCITY in VELO_TRUE column (this will be replaced after
      # validation checks)
      data$VELO_TRUE <- data$VELOCITY
      # And remove VELOCITY column so doesn't go through validation rules, it
      # will be replaced afterwards
      data$VELOCITY <- NA
    }
  }
  ### Add calculated variables based on input data -----------------------------------------

  # Check alkalinity related columns and calculate if necessary
  data <- get_alkalinity(data)

  # Calculate discharge category from velocity and width if required
  data <- get_discharge(data)

  # If model GIS - then NGR easting and northing maybe not be provided. So only calculate
  # Easting and Northings for physical data.
  if (model == "physical") {

    # Check NGR length
    data$NGR <- as.character(data$NGR)
    data$NGR_LENGTH <- nchar(data$NGR)
    if (all(is.na(data$NGR))) {
      stop("You provided data with all NGR values missing,
       Hint: Check your NGR variable has letters. ", call. = FALSE)
    }
    if (any(is.na(data$NGR))) {
      stop("You provided data with one or more NGR values missing,
       Hint: Check your NGR variable has letters. ", call. = FALSE)
    }
    if (any(data$NGR_LENGTH > 2)) {
      stop("You provided an NGR with more than two letters,
       Hint: Check your NGR variables have less than 3 three letters. ", call. = FALSE)
    }
    data$NGR_LENGTH <- NULL


    # Check for length <5, add a leading zeros "0" to get proper Easting/Northing 5 digit codes
    if (any(is.na(data$EASTING)) || any(is.na(data$NORTHING))) {
      stop("EASTING or NORTHING value(s) have not been supplied, we expect
       all rows to have Easting and Northing values.
       Hint: Check all rows of input data have Easting and Northing values. ", call. = FALSE)
    } else {
      data$EASTING <- as.character(formatC(round(as.numeric(data$EASTING)), width = 5, format = "d", flag = "0"))
      data$NORTHING <- as.character(formatC(round(as.numeric(data$NORTHING)), width = 5, format = "d", flag = "0"))
    }
  }

  # Display which model area has been detected
  message("Grid reference values detected for '", toupper(area), "' - applying relevant checks.")

  # Calculate Longitude & Latitude
  if (area %in% c("gb","iom","ni") && model == "physical") {
    # suppress warning: In showSRID(uprojargs, format = "PROJ", multiline = "NO"):
    # Discarded datum OSGB_1936 in CRS definition
    lat_long <- with(data, suppressWarnings(getLatLong(NGR, EASTING, NORTHING, "WGS84", area)))
    data$LONGITUDE <- lat_long$lon
    data$LATITUDE <- lat_long$lat
  }

  if (!is.null(crs) && as.numeric(crs) == 29903 && model == "physical") {
    # suppress warning: In showSRID(uprojargs, format = "PROJ", multiline = "NO"):
    # Discarded datum OSGB_1936 in CRS definition
    lat_long <- with(data, suppressWarnings(getLatLong_NI(EASTING, NORTHING)))
    data$LONGITUDE <- lat_long$Longitude
    data$LATITUDE <- lat_long$Latitude
  }
  if (model == "gis") {
    coords <- st_as_sf(data[, c("SX", "SY")], coords = c("SX", "SY"), crs = 27700)
    coords <- st_transform(coords, crs = 4326)
    data$LATITUDE <- unlist(map(coords$geometry, 2))
    data$LONGITUDE <- unlist(map(coords$geometry, 1))
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
    if ((is.null(data$MEAN.AIR.TEMP) || is.null(data$AIR.TEMP.RANGE)) ||
      (any(is.na(data$MEAN.AIR.TEMP)) || any(is.na(data$AIR.TEMP.RANGE)))) {
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
  # Rename variables to match functional specifications
  data$TEMPM <- data$TMEAN
  data$TEMPR <- data$TRANGE
  data$TMEAN <- NULL
  data$TRANGE <- NULL

  # Calculate total substrate and phi grain size scale
  if (model == "physical" && area != "iom") {
    data <- get_substrate(data)
  }

  ### Check values pass validation rules ----------------------------------------------------------
  # Add row variables to link data input rows to row in the validation check dataframe
  data$ROW <- seq_len(nrow(data))
  # Loop through each variable in validation rules dataframe
  checks <- lapply(split(
    validation_rules[validation_rules$variable %in% names(data), ],
    validation_rules$variable[validation_rules$variable %in% names(data)]
  ), function(rule) {
    # find matching column in input data to validation rule
    values <- data[, c(rule$variable, "ROW", "SITE", "YEAR")]
    # skip optional column if contains only NA values - e.g. HARDNESS - this
    # column will be class logical and not as expected by validation rules
    if (!all(is.na(values[, rule$variable]) & rule$optional == TRUE)) {
      # loop through all values in column
      checks <- lapply(split(values, row.names(values)), function(value) {
        # make dataframe to hold checks
        check <- data.frame(
          "ROW" = "",
          "SITE" = "",
          "YEAR" = "",
          "FAIL" = "",
          "WARNING" = "",
          "REPLACEMENT" = "",
          stringsAsFactors = FALSE
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
        # If value not NA, then check for less than warnings
        warns <- ""
        if (is.na(rule$less_than_warn) == FALSE) {
          if (!is.na(value[, rule$variable]) & value[, rule$variable] < rule$less_than_warn) {
            warns <- c(
              warns,
              paste0(
                "You provided ", names(value)[1], ": ", value[, rule$variable],
                ", ", rule$min_warning_message, " ", rule$less_than_warn, ". "
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
            "ROW" = as.character(value$ROW),
            "SITE" = value$SITE,
            "YEAR" = as.character(value$YEAR),
            "FAIL" = fails,
            "WARNING" = warns,
            "REPLACEMENT" = replacem,
            stringsAsFactors = FALSE
          )
        )
        return(checks)
      })
      return(dplyr::bind_rows(checks))
    }
  })
  # Bind checks into data frame
  checks <- dplyr::bind_rows(checks)
  ### Add discharge and velocity columns missing data fails ---------------------------
  if(area != "iom") {
  discharge_velocity_fails <- data[is.na(data$DISCHARGE) & is.na(data$VELO_TRUE), ]
  discharge_velocity_fails <- data.frame(
    "ROW" = discharge_velocity_fails$ROW,
    "SITE" = discharge_velocity_fails$SITE,
    "YEAR" = discharge_velocity_fails$YEAR,
    stringsAsFactors = FALSE
  )

  if (nrow(discharge_velocity_fails) > 0) {
    discharge_velocity_fails$FAIL <-
      "You provided empty VELOCITY and DISCHARGE values, we expect values for at least one of these variables."
    discharge_velocity_fails$WARNING <- ""
    discharge_velocity_fails$REPLACEMENT <- ""
    # Add discharge and velocity fails
    checks <- rbind(checks, discharge_velocity_fails)
  }
  }
  ### Replace values if value is less than the ‘overall’ minimum value ------------------------------
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
  if (any(data$D_F_SOURCE[!is.na(data$D_F_SOURCE)] < DFS_LIM)) {
    data$D_F_SOURCE[data$D_F_SOURCE < DFS_LIM] <- DFS_LIM
  }
  MNW_LIM <- validation_rules_input[validation_rules_input$variable == "MEAN_WIDTH", "replacement_limit"]
  if (any(data$MEAN_WIDTH[!is.na(data$MEAN_WIDTH)] < MNW_LIM)) {
    data$MEAN_WIDTH[data$MEAN_WIDTH < MNW_LIM] <- MNW_LIM
  }
  MND_LIM <- validation_rules_input[validation_rules_input$variable == "MEAN_DEPTH", "replacement_limit"]
  if (any(data$MEAN_DEPTH[!is.na(data$MEAN_DEPTH)] < MND_LIM)) {
    data$MEAN_DEPTH[data$MEAN_DEPTH < MND_LIM] <- MND_LIM
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

  # Replace dummy velocity values (where NAs were entered) with real values (i.e. NA values)
  # The NAs would have failed validation checks so were replaced with '1'.
  if (!is.null(data$VELO_TRUE)) {
    data$VELOCITY <- data$VELO_TRUE
  }
  ### Format checks and print check messages  --------------------------------------------------
  # Remove empty checks
  checks <- checks[checks$FAIL != "" | checks$WARNING != "" | checks$REPLACEMENT != "", ]
  # If both fail and warn - then only return fail
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

  ### Pass checks and passing rows into list of dataframes ready for rict_predict() --------------------------
  # Find failing rows and store separately
  this_failing <- checks[checks$FAIL != "---", ]
  # Subset the 'passing' instances to run in prediction by removing "this_failing"
  data <- data[!data$SITE %in% this_failing$SITE, ]
  if (nrow(data) == 0 && stop_if_all_fail == TRUE) {
    stop(paste(list("You provided data that has failure(s) on every row.
       We expect at least one row without any fails to proceed.
       HINT: Check fail messages, fix errors and re-try: ",
                    paste(utils::capture.output(print(checks)),
              collapse = "\n")), collapse = "\n"),
         call. = FALSE)
  }

  if (row == FALSE) {
    checks$ROW <- NULL
  }

  return(list("data" = data, "checks" = checks, "model" = model, "area" = area))
}
