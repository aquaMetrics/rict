#' Calculate River Invertebrate Classification Tool (RICT) predictions
#'
#' @seealso \code{\link{rict_classify}} to run classification using rict_predict
#'   outputs
#' @param data Dataframe of predictive environmental values - variables
#'   dependent on input for model 44, model 1, NI or GB
#' \describe{
#'   \item{SITE}{Site reference number}
#'   \item{Waterbody}{Water body identifier}
#'   \item{LATITUDE}{Location point}
#'   \item{LONGITUDE}{Location point}
#'   \item{LOG.ALTITUDE}{Logarithmic value for Altitude}
#'   \item{LOG.DISTANCE.FROM.SOURCE}{Logarithmic value for Distance from source}
#'   \item{LOG.WIDTH}{Logarithmic value for Stream Width}
#'   \item{LOG.DEPTH}{Logarithmic value for Stream Depth}
#'   \item{MEAN.SUBSTRATUM}{Calculated mean substratum}
#'   \item{DISCHARGE.CATEGORY}{Discharge category}
#'   \item{ALKALINITY}{Alkalinity}
#'   \item{LOG.ALKALINITY}{Logarithmic value for Alkalinity}
#'   \item{LOG.SLOPE}{Logarithmic value for Slope}
#'   \item{MEAN.AIR.TEMP}{Calculated Mean Air Temperature}
#'   \item{AIR.TEMP.RANGE}{Calculated Air Temperature Range}
#'   \item{p1 – p43}{Calculated probability of end group membership}
#'   \item{SuitCode}{Calculated Suitability code for test site}
#'   \item{BelongsTo_endGrp}{Predicted probable end group the test site belongs to}
#'   \item{All Indices...}{...Predicted index value for All Indices}
#'   \item{SPR_SEASON_ID}{ID number for season}
#'   \item{SPR_TL2_WHPT_ASPT (ABW,DISTFAM)}{Observed  value for WHPT NTAXA for spring}
#'   \item{SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)}{Observed value for WHPT ASPT for spring}
#'   \item{SPR_NTAXA_BIAS}{Bias value used}
#'   \item{SUM_SEASON_ID}{Summary ID}
#'   \item{SUM_TL2_WHPT_ASPT (ABW,DISTFAM)}{Observed value for WHPT ASPT for summer}
#'   \item{SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)}{Observed value for WHPT NTAXA for summer}
#'   \item{SUM_NTAXA_BIAS}{Bias value used}
#'   \item{AUT_SEASON_ID}{Aut ID}
#'   \item{AUT_TL2_WHPT_ASPT (ABW,DISTFAM)}{Observed value for WHPT ASPT for autumn}
#'   \item{AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)}{Observed value for WHPT NTAXA for autumn}
#'   \item{AUT_NTAXA_BIAS}{Bias value used}
#'   \item{Area_CEH}{}
#'   \item{AltBar_CEH}{}
#'   \item{Alt_CEH}{}
#'   \item{DFS_CEH}{}
#'   \item{Slope_CEH}{}
#'   \item{QCat_CEH}{}
#'   \item{Peat_CEH}{}
#'   \item{Chalk_O1_CEH}{}
#'   \item{Clay_O1_CEH}{}
#'   \item{Hardrock_O1_CEH}{}
#'   \item{Limestone_O1_CEH}{}
#'   \item{area}{ni or gb}
#' }
#' @param all_indices Boolean - Return all indices in output (default only
#'   returns WHPT indices).
#' @param taxa Boolean - Return taxa predictions (default  returns indices).
#' @param taxa_list Vector of taxa lists to predict default all lists i.e.
#'   c("TL1", "TL2", "TL3", "TL4", "TL5").
#' @param rows Number (integer) of rows (one site per row) to predict taxa.
#'   Default is all rows.
#' @param area Area is by detected by default from the NGR, but you can provide
#'   the area parameter either 'iom', 'gb, 'ni' for testing purposes.
#' @param crs optionally set crs to `29903` for Irish projection system.
#' @return Dataframe of predicted biotic scores and probability of observed
#'   values falling into each statistical grouping of rivers.
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr mutate n group_by filter select
#' @importFrom stats complete.cases
#'
#' @examples
#' \dontrun{
#' predictions <- rict_predict(demo_observed_values)
#' }
rict_predict <- function(data = NULL,
                         all_indices = FALSE,
                         taxa = FALSE,
                         taxa_list = c("TL1", "TL2", "TL3", "TL4", "TL5"),
                         rows = NULL,
                         area = NULL,
                         crs = NULL) {
  # Validate predictive input data
  all_validation <- rict_validate(data, area = area, crs = crs)
  model <- all_validation[["model"]] # returns model based on input headers
  area <- all_validation[["area"]] # returns area based on NGR
  # Change all column names to uppercase
  names(data) <- toupper(names(data))

  ## load supporting tables ---------------------------------------------------
  if (taxa == TRUE) {
    taxa.input.data <- utils::read.csv(system.file("extdat",
      "TAXA_AB_APR_PRAB.csv",
      package = "rict"
    ),
    stringsAsFactors = TRUE,
    skip = 1
    )
    model_type <- ifelse(area == "gb", 1, 2)
    taxa.input.data <- filter(taxa.input.data, .data$Model == model_type)
  }

  end_group_index <- utils::read.csv(system.file("extdat",
    "x-103-end-group-means-formatted-jdb-17-dec-2019.csv",
    package = "rict"
  ),
  check.names = FALSE
  )
  end_group_index <- rename_end_group_means(end_group_index)
  if (area == "iom") {
    end_group_index <- utils::read.csv(system.file("extdat",
      "x-103-end-group-means-formatted-iom.csv",
      package = "rict"
    ),
    check.names = FALSE
    )

    end_group_index$`TL2 33 Group ARMI NTaxa` <- NA
    end_group_index$`TL2 33 Group ARMI Score` <- NA
    end_group_index$`TL2 33 Group Flow  Silt NTaxa` <- NA
    end_group_index$`TL2 33 Group Flow  Silt Score` <- NA

    end_group_index <- rename_end_group_means(end_group_index)
  }

  nr_efg_groups <- utils::read.csv(system.file("extdat",
    "end-grp-assess-scores.csv",
    package = "rict"
  ))

  if (model == "physical" && area == "gb") {
    df_mean_gb685 <-
      utils::read.delim(
        system.file("extdat", "df-mean-gb-685.DAT", package = "rict"),
        header = FALSE,
        sep = "",
        as.is = TRUE
      )
    df_coeff_gb685 <-
      utils::read.delim(
        system.file("extdat", "df-coeff-gb-685.DAT", package = "rict"),
        header = FALSE,
        sep = "",
        as.is = TRUE
      )
  }
  if (area == "ni") {
    df_mean_gb685 <-
      utils::read.delim(system.file("extdat", "DFMEAN_NI_RALPH.DAT",
        package = "rict"),
        header = FALSE, sep = "", as.is = TRUE
      )
    df_coeff_gb685 <-
      utils::read.delim(system.file("extdat", "DFCOEFF_NI.DAT",
                                    package = "rict"),
        header = FALSE, sep = "", as.is = TRUE
      )
    nr_efg_groups <-
      utils::read.csv(system.file("extdat", "EndGrp_AssessScoresNI.csv",
        package = "rict"
      ))
  }
  if (model == "gis") {
    df_mean_gb685 <-
      utils::read.csv(
        system.file("extdat",
                    "end-group-means-discriminant-scores-model-44.csv",
          package = "rict"
        )
      )
    df_mean_gb685 <- df_mean_gb685[, 3:19]
    df_coeff_gb685 <-
      utils::read.csv(
        system.file("extdat", "discriminant-function-coefficients-model-44.csv",
          package = "rict"
        )
      )
  }

  if (area == "iom") {
    df_mean_gb685 <-
      utils::read.delim(system.file("extdat", "df-mean-iom.DAT",
                                    package = "rict"),
        header = FALSE, sep = "", as.is = TRUE
      )
    df_coeff_gb685 <-
      utils::read.delim(system.file("extdat", "df-coeff-iom.DAT",
                                    package = "rict"),
        header = FALSE, sep = "", as.is = TRUE
      )
    nr_efg_groups <-
      utils::read.csv(system.file("extdat", "end-group-assess-scores-iom.csv",
        package = "rict"
      ))
  }

  # check season provided#
  seasons_to_run <-
    length(na.omit(c(data$SPR_SEASON_ID[1],
             data$AUT_SEASON_ID[1],
             data$SUM_SEASON_ID[1])))
  if(seasons_to_run < 1) {
    warning("No '...SEASON_ID' provided, predicting all seasons",
            call. = FALSE
    ) # or run all seasons if not provided
    seasons_to_run <- 1:3
  } else {
  seasons_to_run <- 1:3
  }
  data <- all_validation[[1]]
  if (model == "gis") {
    # remove TEST-SITE_CODE column - not required and causes issues later on!
    data$`TEST SITECODE` <- NULL
  }

  # Final Data for classification e.g. Linear discriminant Analysis (LDA)
  # classifier/predictor
  if (model == "physical" && area == "gb") {
    final_predictors <- data.frame(
      "SITE" = data$SITE,
      "LATITUDE" = data$LATITUDE,
      "LONGITUDE" = data$LONGITUDE,
      "LOG.ALTITUDE" = data$vld_alt_src_log,
      "LOG.DISTANCE.FROM.SOURCE" = data$vld_dist_src_log,
      "LOG.WIDTH" = data$mn_width_log,
      "LOG.DEPTH" = data$mn_depth_log,
      "MEAN.SUBSTRATUM" = data$vld_substr_log,
      "DISCHARGE.CATEGORY" = data$DISCHARGE, # data$disch_log,
      "ALKALINITY" = data$ALKALINITY,
      "LOG.ALKALINITY" = data$vld_alkal_log,
      "LOG.SLOPE" = data$vld_slope_log,
      "MEAN.AIR.TEMP" = data$TEMPM,
      "AIR.TEMP.RANGE" = data$TEMPR
    )
  }

  if (area == "iom") {
    final_predictors <- data.frame(
      "SITE" = data$SITE,
      "LOG.ALTITUDE" = data$vld_alt_src_log,
      "LOG.DISTANCE.FROM.SOURCE" = data$vld_dist_src_log,
      "LOG.ALKALINITY" = data$vld_alkal_log,
      "LOG.SLOPE" = data$vld_slope_log
    )
  }

  if (area == "ni") {
    final_predictors <- data.frame(
      "SITE" = data$SITE,
      "LATITUDE" = data$LATITUDE,
      "LONGITUDE" = data$LONGITUDE,
      "LOG.ALTITUDE" = data$vld_alt_src_log,
      "LOG.DISTANCE.FROM.SOURCE" = data$vld_dist_src_log,
      "LOG.WIDTH" = data$mn_width_log,
      "LOG.DEPTH" = data$mn_depth_log,
      "MEAN.SUBSTRATUM" = data$vld_substr_log,
      "DISCHARGE.CATEGORY" = data$DISCHARGE, # data$disch_log,
      "ALKALINITY" = data$ALKALINITY,
      "LOG.ALKALINITY" = data$vld_alkal_log,
      "LOG.SLOPE" = data$vld_slope_log
    )
  }

  if (model == "gis") {
    final_predictors <- data.frame(
      "SITE" = data$SITE,
      "LATITUDE" = data$LATITUDE,
      "LONGITUDE" = data$LONGITUDE,
      "TEMPM" = data$TEMPM,
      "TEMPR" = data$TEMPR,
      "ALKALINITY" = data$ALKALINITY,
      "LgAlk" = data$LgAlk,
      "LgArea_CEH" = data$LOG_AREA,
      "LgAltBar_CEH" = data$LOGALTBAR,
      "LgAlt_CEH" = data$LgAlt_CEH,
      "LgDFS_CEH" = data$LgDFS_CEH,
      "LgSlope_CEH" = data$LgSlope_CEH,
      "QCat_CEH" = data$DISCH_CAT,
      "Peat_CEH" = data$PEAT,
      "Chalk_O1_CEH" = data$CHALK,
      "Clay_O1_CEH" = data$CLAY,
      "Hardrock_O1_CEH" = data$HARDROCK,
      "Limestone_O1_CEH" = data$LIMESTONE
    )
    # browser("check lat/lon conversion using st_transform Vs parse_osg?
    #          - check this is the problem first?")
    # convert proportions to percentage for geology variables
    final_predictors[, 14:18] <- final_predictors[, 14:18] * 100
  }

  # Prediction Settings
  # 1. Enter
  # Prediction Settings
  # 2. Find the df_scores of each row using one line of coefficients
  # Note: df_coeff_gb685[1,-1] # removes the first column
  # NRefg equals number of reference sites in end group g , for GB = 685, for NI
  # = 11
  NRefg_all <- rowSums(nr_efg_groups[, -1])
  # #DFScore_g <- DFCoef1 * Env1 + ... + DFCoefn * Envn ; remove "SITE" col=1
  # from final_predictors, and  remove col=1 from df_coeff_gb685
  df_scores <- as.data.frame(getDFScores(
    EnvValues = final_predictors,
    DFCoeff = df_coeff_gb685
  ))
  # Calculate the Mahanalobis distance of point x from site g for all reference
  # sites
  MahDist_g <- getMahDist(DFscore = df_scores, meanvalues = df_mean_gb685)

  # Make column names
  DistNames <- paste0("p", seq_len(ncol(MahDist_g))) # reused later
  MahDistNames <- gsub("p", "Mah", DistNames)
  colnames(MahDist_g) <- MahDistNames

  # Calculate the minimum Mahanalobis distance of point x from site g
  MahDist_min <- getMahDist_min(df_scores, df_mean_gb685)
  # Calculate the probability distribution
  PDist_g <- PDist(NRefg_all, MahDist_g)
  # Calculate probabilities of sites belonging to the endgroups, prob_g, l,as
  # last column 44 contains the total "PGdistTot"
  # ALL probabilities p1..pn,  rowsums() add to 1, except when last row which it
  # "total" is removed i.e. rowSums(PDistTot[,-ncol(PDistTot)])=1
  PDistTot <- as.data.frame(PDistTotal(PDist_g))
  # Rename the columns to probabilities with "Total" at the end.
  colnames(PDistTot) <- c(DistNames, "Total")

  final_predictors_try1 <- cbind(final_predictors, PDistTot[, -ncol(PDistTot)])

  # 3. Use chisquare to find suitability codes.
  chi_square <-
    utils::read.csv(
      system.file("extdat", "chi_square.csv",
                  package = "rict"
      )
    )
  suit_codes <- getSuitabilityCode(MahDist_min, chi_square, area, model)
  # Add suitability codes to the final data, using cbind
  final_predictors_try2 <- cbind(final_predictors_try1, suit_codes)
  # Find max class group belongs to by getting the column names
  group_probabilities <- final_predictors_try2[, DistNames]
  belongs_to_end_grp <- lapply(1:nrow(group_probabilities), function(n) {
    return(which.max(group_probabilities[n, ]))
  })

  # Replace p with EndGr
  belongs_to_end_grp <- gsub("p", "EndGr", names(unlist(belongs_to_end_grp)))
  final_predictors_try3 <- cbind(final_predictors_try2, belongs_to_end_grp)

  # 4 Prediction: WE1.5 Algorithms for prediction of expected values of any index based on probability of end group
  # membership and average values of the index amongst reference sites in each end group.
  # We predict WHPT NTAXA, and WHPT ASP
  endgroup_index_frame <- end_group_index[grep(area, end_group_index$RIVPACS_Model, ignore.case = TRUE), ]
  endgroup_index_frame <- dplyr::select(endgroup_index_frame, -.data$RIVPACS_Model)
  endgroup_index_frame <- dplyr::rename(endgroup_index_frame,
    EndGrp = .data$End_Group,
    SeasonCode = .data$Season_Code
  )

  # Sort by the columns "EndGrp", "SeasonCode"
  endgroup_index_frame <- dplyr::arrange(endgroup_index_frame, .data$EndGrp, .data$SeasonCode)

  # Prepare what you want to run - seasons, indices, and subset the data with the seasonCodes
  endgroup_index_frame <- filter(endgroup_index_frame, .data$SeasonCode %in% seasons_to_run)

  # Write a function that extracts user input columns and converts them to the values in c("") below :: USER INPUT
  indices_to_run <- c(
    "TL2_WHPT_NTAXA_AbW_DistFam", "TL2_WHPT_ASPT_AbW_DistFam",
    "TL2_WHPT_NTAXA_AbW_CompFam", "TL2_WHPT_ASPT_AbW_CompFam"
  )

  # Run the index Scores
  # seasons_to_run <- seasons_to_run[!is.na(seasons_to_run)]

  if (taxa == TRUE && area != "iom") { # This block predicts and returns taxa predictions
    # Declare a variable where we append all sites
    taxa_pred <- list()
    taxa_predictions <- data.frame()
    # Use complete cases removing null values
    taxa.input.data <- taxa.input.data[complete.cases(taxa.input.data), ]
    nsites <- nrow(final_predictors_try2)
    if (is.null(rows)) {
      chooseSite <- seq_len(nsites)
    } else {
      chooseSite <- seq_len(rows)
    }
    taxa.input.data$EndGrp_Probs <- taxa.input.data$End.Group
    taxa.input.data <- taxa.input.data[taxa.input.data$TL %in% taxa_list, ]
    ## Loop for each SITE
    for (i in chooseSite) {
      message("Processing input row: ", i)
      site1 <- taxa.input.data
      siteIndex <- ifelse(i < 10, paste0("0", i), i)
      measuredColumns <- site1[, c(
        "Average_Numerical_Abundance",
        "Average_Log10_Abundance", "Prob_Occurrence",
        "Prob_Log1", "Prob_Log2", "Prob_Log3", "Prob_Log4", "Prob_Log5"
      )]
      names_measuredColumns <- names(measuredColumns)
      site1$EndGrp_Probs <- EndGrpProb_Replacement(
        site1$EndGrp_Probs,
        final_predictors_try3,
        which(chooseSite == i)
      )
      b1 <- site1 %>%
        group_by(.data$Season_Code, .data$TL, .data$Furse_Code) %>%
        mutate(count = n(), mlist_endGrps = list(.data$EndGrp_Probs)) # `End Group`
      # gives 3,522 rows.or unique b1 Multiply by 12 sites gives us 3,532x12 = 42,384 groups in TOTAL.
      allUniqueSites <- unique(b1[, c(4, 3, 5, 6, 7, 20, 21)])

      for (k in seq_len(nrow(allUniqueSites))) { ## loop over these unique rows per SITE
        sitex <- groupSitesFunction(allUniqueSites, k, siteIndex, b1)
        sitex$siteName <- final_predictors_try2$SITE[i]
        taxa_pred[[k]] <- sitex
      } # for k
      taxa_preds <- data.frame(do.call("rbind", taxa_pred))
      taxa_predictions <- rbind(taxa_predictions, taxa_preds)
    } # for i

    # Remove the "End.Group" column
    taxa_predictions$End.Group <- NULL
    # Arrange the sites by siteName, TL, Season_Code, Furse_code
    dplyr::arrange(
      taxa_predictions,
      .data$siteName,
      .data$TL,
      .data$Season_Code,
      .data$Furse_Code
    )
    # In multi-year predictions there can be multiple rows with the same
    # name/predictors. Therefore processing by row creates dups. Think about
    # re-factoring this to only process unique rows.
    taxa_predictions <- taxa_predictions[!duplicated(taxa_predictions), ]
    return(taxa_predictions)
  }
  if(taxa == TRUE && area == "iom") {
    message("Isle of Man model cannot predict taxa")
    taxa_predictions <- NULL
    return(taxa_predictions)
  }

  # Predict indices scores
  indices_predictions <- getSeasonIndexScores(
    data_to_bindTo = final_predictors_try3,
    season_to_run = seasons_to_run,
    index_id = indices_to_run,
    end_group_IndexDFrame = endgroup_index_frame,
    DistNames = DistNames,
    all_indices = all_indices
  )
  # Append the biological data to the main output dataframe
  # Get all the bioligical data
  names_biological <- c(
    "SITE",
    "WATERBODY",
    "YEAR",
    "SPR_SEASON_ID",
    "SPR_TL2_WHPT_ASPT (ABW,DISTFAM)",
    "SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)",
    "SPR_NTAXA_BIAS",
    "SUM_SEASON_ID",
    "SUM_TL2_WHPT_ASPT (ABW,DISTFAM)",
    "SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)",
    "SUM_NTAXA_BIAS",
    "AUT_SEASON_ID",
    "AUT_TL2_WHPT_ASPT (ABW,DISTFAM)",
    "AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)",
    "AUT_NTAXA_BIAS"
  )
  # Check user input data contains biological values
  if (any(names_biological %in% names(data))) {
    biological_data <- data[, names(data) %in% names_biological]
    if(length(biological_data[names(biological_data) %in%
              c("SITE", "WATERBODY", "YEAR")]) < 3) {
      stop("You provided data missing either SITE, WATERBODY or YEAR, we expect
           all three columns to be present")
    }
    # remove column "SITE", the first one of columns
    biological_data <- select(biological_data, -.data$SITE)
    # Check at least one season's worth of data present
    spring <- length(grep("SPR", names(biological_data)))
    summer <- length(grep("SUM", names(biological_data)))
    autumn <- length(grep("AUT", names(biological_data)))
    # If no season / observation present - then prediction only.
    if(all(c(spring, summer, autumn) == 0)) {
      spring <- 4
      summer <- 4
      autumn <- 4
    }
    if(!any(c(spring, summer, autumn) == 4)) {
     stop("At least one season must have SEASON_ID, ASPT, NTAXA and
          BIAS columns present.")
    }
    indices_predictions <- cbind(indices_predictions, biological_data)
    indices_predictions$area <- area #  needed for classify function
  }


  else {
    stop(paste0("You provided a data with no observation columns provided,
                We expect at least one season's worth of observation and
                SITE, WATERBODY, YEAR: ",
               paste((names_biological), collapse = ",")))
  }
  # Add in missing columns to allow classification to run
  # - will return empty values for missing seasons etc
  missing_columns <-
    names_biological[!names_biological %in% names(data)]
  if(length(missing_columns) > 0) {
  missing <- data.frame()
  missing[missing_columns] <- list(numeric(0))
  missing[1:nrow(indices_predictions), missing_columns] <- NA
  indices_predictions <- cbind(indices_predictions, missing)
  }
  return(indices_predictions)
}
