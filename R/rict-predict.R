#' Calculate River Invertebrate Classification Tool (RICT) predictions
#'
#' @param data Dataframe of predictive environmental values
#' \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'   \item{SPR_SEASON_ID}{Spring ID}
#'   \item{SUM_SEASON_ID}{Summary ID}
#'   \item{AUT_SEASON_ID}{Aut ID}
#'   \item{...}{etc}
#' }
#' @param model Validate observed values based on rules required for model selected.
#' @param area Area of UK, either 'ni' (Northern Ireland) or 'gb' (Great Britain)
#'
#' @return Dataframe of predicted biotic scores and probability of observed values
#' falling into each statistical grouping of rivers.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' predictions <- rict_predict(demo_observed_values)
#' }
rict_predict <- function(data = NULL, model = "physical", area = "gb") {
  # Validate predictive input data
  all_validation <- rict_validate(data)
  # Change all column names to uppercase
  names(data) <- toupper(names(data))
  # load supporting tables
  taxa_average_abundance <-
    utils::read.csv(system.file("extdat", "taxxaab.csv", package = "rict"))
  if (model == "physical") {
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
    ) }
  if (model == "gis") {
      df_mean_gb685 <-
      utils::read.csv(
        system.file("extdat", "end-group-means-discriminant-scores-model-44.csv",
                    package = "rict")
      )
    df_mean_gb685 <- df_mean_gb685[, 3:19]
    df_coeff_gb685 <-
      utils::read.csv(
        system.file("extdat", "discriminant-function-coefficients-model-44.csv",
                    package = "rict")
      )
  }

  end_points <- utils::read.csv(system.file("extdat", "test-data-end-point-means.csv", package = "rict"))
  end_group_index <- utils::read.csv(system.file("extdat", "x-103-end-group-means.csv", package = "rict"))
  nr_efg_groups <- utils::read.csv(system.file("extdat", "end-grp-assess-scores.csv", package = "rict"))
  # end_group_means_discriminant_scores_model_44 <-
  #   utils::read.csv(system.file("extdat", "end-group-means-discriminant-scores-model-44.csv",
  #                               package = "rict"))

  # check season provided
  if (all(!is.null(data$SPR_SEASON_ID[1]),
         !is.null(data$AUT_SEASON_ID[1]),
         !is.null(data$SUM_SEASON_ID[1]))) {
  seasons_to_run <- c(data$SPR_SEASON_ID[1],
                      data$AUT_SEASON_ID[1],
                      data$SUM_SEASON_ID[1])  # Choose the seasons to run e.g. spring and autumn
  } else {

    warning("No '...SEASON_ID' provided, predicting all seasons",
            call. = FALSE)  # or run all seasons if not provided
    seasons_to_run <- 1:3
  }
  # extract fails, warnings and values from list of dataframes returned from rict_validate function:
  # warning_failings <- all_validation[[1]]
  this_failing <- all_validation[[2]]
  this_failing <- this_failing[this_failing$fail != "---", ]
  data <- all_validation[[1]]
 if (model == "gis") {
     data$`TEST SITECODE`  <- NULL
   } # remove TEST-SITE_CODE column - not required and causes issues later on?!
  # Data validation and conversion
  # 13.2 subset the instances to run in prediction by removing "this_failing", use anti-join
  # i.e."Return all rows from x where there are no matching values in y, keeping just columns from x.
  # This is a filtering join"
  # final_predictors_one <- anti_join(data, this_failing, by="SITE") # This works in R Studio,
  # but not in ML AZURE
  final_predictors_one <- data[is.na(match(data$SITE, this_failing$SITE)), ]
  # DONT SORT, if you do, don't use the SORTED array for prediction. it duplicates the results ******
  # final_predictors_one <- final_predictors_one[order(final_predictors_one$SITE),]
  # Print to see where the sorting is taking place  #
  # Generate data for classification
  # Final Data for classification e.g. Linear discriminant Analysis (LDA) classifier/predictor
  if (model == "physical") {
    final_predictors <- data.frame(
     "SITE"                      =  final_predictors_one$SITE,
     "LATITUDE"                  =  final_predictors_one$LATITUDE,
     "LONGITUDE"                 =  final_predictors_one$LONGITUDE,
     "LOG.ALTITUDE"              =  final_predictors_one$vld_alt_src_log,
     "LOG.DISTANCE.FROM.SOURCE"  =  final_predictors_one$vld_dist_src_log,
     "LOG.WIDTH"                 =  final_predictors_one$mn_width_log,
     "LOG.DEPTH"                 =  final_predictors_one$mn_depth_log,
     "MEAN.SUBSTRATUM"           =  final_predictors_one$vld_substr_log,
     "DISCHARGE.CATEGORY"        =  final_predictors_one$DISCHARGE,    #data$disch_log,
     "ALKALINITY"                =  final_predictors_one$ALKALINITY,
     "LOG.ALKALINITY"            =  final_predictors_one$vld_alkal_log,
     "LOG.SLOPE"                 =  final_predictors_one$vld_slope_log,
     "MEAN.AIR.TEMP"             =  final_predictors_one$TMEAN,
     "AIR.TEMP.RANGE"            =  final_predictors_one$TRANGE
    )
  }

  if (model == "gis") {
    final_predictors <- data.frame(
      "SITE"                     =  final_predictors_one$SITE,
      "LATITUDE"                 =  final_predictors_one$LATITUDE,
      "LONGITUDE"                =  final_predictors_one$LONGITUDE,
      "TEMPM"                    =  final_predictors_one$TMEAN,
      "TEMPR"                    =  final_predictors_one$TRANGE,
      "ALKALINITY"               =  final_predictors_one$ALKALINITY,
      "LgAlk"                    =  final_predictors_one$LgAlk,
      "LgArea_CEH"               =  final_predictors_one$LgArea_CEH,
      "LgAltBar_CEH"             =  final_predictors_one$LgAltBar_CEH,
      "LgAlt_CEH"                =  final_predictors_one$LgAlt_CEH,
      "LgDFS_CEH"                =  final_predictors_one$LgDFS_CEH,
      "LgSlope_CEH"              =  final_predictors_one$LgSlope_CEH,
      "QCat_CEH"                 =  final_predictors_one$QCAT_CEH,
      "Peat_CEH"                 =  final_predictors_one$`%PEAT_CEH`,
      "Chalk_O1_CEH"             =  final_predictors_one$CHALK_O1_CEH,
      "Clay_O1_CEH"              =  final_predictors_one$CLAY_O1_CEH,
      "Hardrock_O1_CEH"          =  final_predictors_one$HARDROCK_O1_CEH,
      "Limestone_O1_CEH"         =  final_predictors_one$LIMESTONE_O1_CEH
    )
  }

  # Prediction Settings
  # 1. Enter
  # Prediction Settings
  # 2. Find the df_scores of each row using one line of coefficients
  # Note: df_coeff_gb685[1,-1] # removes the first column
  # NRefg equals number of reference sites in end group g , for GB = 685, for NI = 11
  NRefg_all <- rowSums(nr_efg_groups[, -1])
  # #DFScore_g <- DFCoef1 * Env1 + ... + DFCoefn * Envn ; remove "SITE" col=1 from final_predictors,
  # and  remove col=1 from df_coeff_gb685

  df_scores <- as.data.frame(getDFScores(EnvValues = final_predictors,
                                         DFCoeff = df_coeff_gb685))
  # Calculate the Mahanalobis disance of point x from site g for all reference sites
  MahDist_g <- getMahDist(DFscore = df_scores, meanvalues = df_mean_gb685)
  MahDistNames <- paste0("p", 1:43)
  MahDistNames <- gsub("p", "Mah", MahDistNames)
  colnames(MahDist_g) <- MahDistNames

  # Calculate the minimum Mahanalobis disance of point x from site g
  MahDist_min <- getMahDist_min(df_scores, df_mean_gb685)
  # Calculate the probability distribution
  PDist_g <- PDist(NRefg_all, MahDist_g)
  # Main dataframe needed:: Calculate probabilities of sites belonging to the endgroups,
  # prob_g, l,as last column 44 contrains the total "PGdistTot
  PDistTot <- as.data.frame(PDistTotal(PDist_g)) ## ALL probabilities p1..pn,  rowsums() add to 1,
  # except when last row which it "total" is removed i.e. rowSums(PDistTot[,-ncol(PDistTot)])=1
  # Rename the columns to probabilities p1,p2,...,p43
  colnames(PDistTot) <- c(paste0("p", 1:43), "Total")

  # #  final_predictors <- cbind(final_predictors, PDist_g[,-ncol(PDist_g)]) # This is the line we need
  # sum(final_predictors_try[1,-c(1:14)]) should give 1
  final_predictors_try1 <- cbind(final_predictors, PDistTot[, -ncol(PDistTot)])

  # head(final_predictors_try1,7)
  #
  # #3.Use chisquare to find suitability codes. Start for Britain GB, # # Could use a file for these chisquare values
  # # 1 = GB 21.02606 24.05393 26.21696 32.90923
  # # 2 = NI 18.30700 21.16080 23.20930 29.58830
  chiSquare_vals <- data.frame(CQ1 = c(21.02606, 18.30700),
                               CQ2 = c(24.05393, 21.16080),
                               CQ3 = c(26.21696, 23.20930),
                               CQ4 = c(32.90923, 29.58830))
  suit_codes <- getSuitabilityCode(MahDist_min, chiSquare_vals)
  # # add suitab ility codes to the final data, using cbind
  final_predictors_try2 <- cbind(final_predictors_try1, suit_codes)
  # Find max class group belongs to by getting the column name: use
  # belongs_to_end_grp <- colnames(final_predictors_try2[,15:57])[apply(final_predictors_try2[,15:57], 1, which.max)]
  # This sometimes returns a list, use unlist below to repair this
  belongs_to_end_grp <- colnames(final_predictors_try2[, paste0("p", 1:43)])[apply(
    data.frame(matrix(unlist(final_predictors_try2[, paste0("p", 1:43)]),
                      nrow = nrow(final_predictors_try2[, paste0("p", 1:43)]),
                      byrow = T),
                stringsAsFactors = FALSE), 1, which.max)]

  # Replace p with EndGr
  belongs_to_end_grp <- gsub("p", "EndGr", belongs_to_end_grp)
  final_predictors_try3 <- cbind(final_predictors_try2, belongs_to_end_grp)

  # head(final_predictors_try3,7)
  # #4 Prediction: WE1.5 Algorithms for prediction of expected values of any index based on probability of end group
  # # membership and average values of the index amongst reference sites in each end group.
  # We predict WHPT NTAXA, and WHPT ASP

  getEndGroupMeansColsNeeded <- function(dframe) {
      # Don't select RIVAPCSMODEL since we know model what we are processing
     filtered_dframe <-  dplyr::filter(dframe, .data$RIVPACS.Model == "RIVPACS IV GB")
      dplyr::select(filtered_dframe, .data$`End.Group`, .data$`Season.Code`,
                    .data$`Season`,
                    .data$`TL2.WHPT.NTAXA..AbW.DistFam.`,
                    .data$`TL2.WHPT.ASPT..AbW.DistFam.`,
                    .data$`TL2.WHPT.NTAXA..AbW.CompFam.`,
                    .data$`TL2.WHPT.ASPT..AbW.CompFam.`)
  }

  endgroup_index_frame <- getEndGroupMeansColsNeeded(end_group_index)
  colnames(endgroup_index_frame) <- c("EndGrp", "SeasonCode", "Season",
                                      "TL2_WHPT_NTAXA_AbW_DistFam", "TL2_WHPT_ASPT_AbW_DistFam",
                                      "TL2_WHPT_NTAXA_AbW_CompFam", "TL2_WHPT_ASPT_AbW_CompFam")
  # Sort by the columns "EndGrp", "SeasonCode"
  endgroup_index_frame <- dplyr::arrange(endgroup_index_frame, .data$EndGrp, .data$SeasonCode)

  # Prepare what you want to run - seasons, indices, and subset the data with the seasonCodes
  # seasons_to_run <- c(1,3) # add more seasons, :: USER INPUT
  # indices_to_run_old <- c(111,112,114, 115) # add more indices., TL2 WHPT NTAXA (AbW,DistFam),
  # index id = 111, TL2 WHPT ASPT (AbW,DistFam), index id = 112
  endgroup_index_frame <- dplyr::filter(endgroup_index_frame, .data$SeasonCode %in% seasons_to_run)

  # Write a function that extracts user input columns and converts them to the values in c("") below :: USER INPUT
  indices_to_run <- c("TL2_WHPT_NTAXA_AbW_DistFam", "TL2_WHPT_ASPT_AbW_DistFam",
                      "TL2_WHPT_NTAXA_AbW_CompFam", "TL2_WHPT_ASPT_AbW_CompFam")

  # Run the index Scores
  seasons_to_run <- seasons_to_run[!is.na(seasons_to_run)]
  # data_to_bindTo, season_to_run, index_id, end_group_IndexDFrame
  mainData <- getSeasonIndexScores(data_to_bindTo = final_predictors_try3,
                                          season_to_run = seasons_to_run,
                                          index_id = indices_to_run,
                                          end_group_IndexDFrame = endgroup_index_frame)

  # Append the biological data to the main output dataframe
  # Get all the bioligical data
  names_biological <- c(colnames(data)[1],
                        colnames(data)[2],
                        colnames(data)[3],
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
                        "AUT_NTAXA_BIAS")
  # Check predictions data contains biological values
  if (all(names_biological %in% names(data))) {
    biological_data <- data[, names_biological]
    # Remove failing sites from biological_data
    biological_data <- biological_data[!(biological_data$SITE %in% this_failing$SITE), ]
    # remove column "SITE", the first one of columns
    biological_data <- biological_data[, -1]
    mainData <- cbind(mainData, biological_data)
  }
  return(mainData)
}
