#' Calculate River Invertebrate Classification Tool (RICT) predictions
#'
#' @param observed_values Dataframe of observed environmental values
#' @return Dataframe of predicted biotic scores and probility of observed values
#' falling into each statistical grouping of rivers.
#' @export
#'
#' @examples
#' \dontrun{
#' predictions <- calcPredictions(observed_values = rict::demo_observed_values)
#' }
calcPredictions <- function(observed_values) {

  # install.packages("src/rnrfa_1.4.0.zip", lib = ".", repos = NULL, verbose = TRUE, dependencies = TRUE)
  # install.packages("src/validationpredictor_functions.zip", lib = ".",
  # repos = NULL, verbose = TRUE, dependencies = TRUE)
  # library(rnrfa, lib.loc=".", verbose=TRUE)

  taxa_average_abundance <-
    read.csv(system.file("extdat", "taxxaab.csv", package = "rict"))

  df_mean_gb685 <-
    read.delim(
      system.file("extdat", "df-mean-gb-685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  df_coeff_gb685 <-
    read.delim(
      system.file("extdat", "df-coeff-gb-685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  # source("inst/extdat/prediction-functions-v1.R")
  # source("src/Helperfunctionsv1.R")
  # source("src/MeanAirTempAirTempRangeASFunction.R")

  end_points <- read.csv(system.file("extdat", "test-data-end-point-means.csv", package = "rict"))
  air_temp_grid <- read.csv(system.file("extdat", "air-temp-grid.csv", package = "rict"))
  end_group_index <- read.csv(system.file("extdat", "x-103-end-group-means.csv", package = "rict"))
  nr_efg_groups <- read.csv(system.file("extdat", "end-grp-assess-scores.csv", package = "rict"))

  #air_temp_grid <- read.csv("src/air-temp-grid.csv")
  #endroup_IndexDFrame <- read.csv("src/x103EndGroupMeans(FORMATTED).csv") # replaces END_GROUP_INDEX.csv
  #nr_efg_groups <- read.csv("src/EndGrp_AssessScores.csv")

  # "SITE" convert to character as required by specification
  observed_values$SITE <- as.character(observed_values$SITE)

  # Change to character
  observed_values$Easting  <- as.character(observed_values$Easting)
  observed_values$Northing <- as.character(observed_values$Northing)

  # check for length <5, add a "0" to get proper Easting/Northing 5 digit codes
  observed_values$Easting <- getCorrectCodes(observed_values$Easting)
  observed_values$Northing <- getCorrectCodes(observed_values$Northing)

  # Change all column names to uppercase
  names(observed_values) <- toupper(names(observed_values))

  # Get all the bioligical data
  names_biological <- c(colnames(observed_values)[1],
                       colnames(observed_values)[2],
                       colnames(observed_values)[3],
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

  biological_data <- observed_values[, names_biological]

  # Choose the seasons to run i.e. spring and autumn
  seasons_to_run <- c(observed_values$SPR_SEASON_ID[1],
                      observed_values$AUT_SEASON_ID[1],
                      observed_values$SUM_SEASON_ID[1])

  # Data validation
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
  msg_columns <- names(dplyr::select(observed_values, ends_with("_msg")))
  this_warning <- observed_values %>%
    dplyr::filter(substr(vld_alt_src_msg, 1, 5) == "Warn:"    | substr(mn_width_msg, 1, 5) == "Warn:"
           | substr(mn_depth_msg, 1, 5) == "Warn:"     | substr(vld_alkal_msg, 1, 5) == "Warn:"
           | substr(disch_msg, 1, 5) == "Warn:"        | substr(vld_substr_msg, 1, 5) == "Warn:"
           | substr(vld_dist_src_msg, 1, 5) == "Warn:" | substr(vld_slope_msg, 1, 5) == "Warn:")
  #  select("SITE","YEAR",msg_columns) # Select some columns
  # write.csv(this_warning, file = paste0(path,"/Warnings_file_data.csv"))
  # which rows are these
  # observed_values[which(this_warning[1,1] %in% observed_values[,c("SITE")]),]

  # 2. Failings to log file
  # Deal with all failings, save them in a file
  this_failing <- observed_values %>%
    dplyr:: filter(substr(vld_alt_src_msg, 1, 5) == "Fail:"    | substr(mn_width_msg, 1, 5) == "Fail:"
           | substr(mn_depth_msg, 1, 5) == "Fail:"     | substr(vld_alkal_msg, 1, 5) == "Fail:"
           | substr(disch_msg, 1, 5) == "Fail:"        | substr(vld_substr_msg, 1, 5) == "Fail:"
           | substr(vld_dist_src_msg, 1, 5) == "Fail:" | substr(vld_slope_msg, 1, 5) == "Fail:")
  # select("SITE","YEAR",msg_columns) # Select some columns
  # # write.csv(this_failing, file = paste0(path,"/Failings_file_data.csv"))
  # Put warnings and failures in a file of warnings_failings
  warnings_failings <- rbind(this_warning, this_failing)

  # Add all fails and warnings
  # Check if dataframe of warnings is empty, if not write to file
  if (nrow(warnings_failings) > 0){
    newdf <- data.frame(warnings_failings)
    pdf("Fails_Warnings2.pdf", height = 11, width = 18.5)
    # Output the pdf file
    data.frame(grid.table(newdf))
    print(newdf)
  }

  # Data validation and conversion
  # 13.2 subset the instances to run in prediction by removing "this_failing", use anti-join
  # i.e."Return all rows from x where there are no matching values in y, keeping just columns from x.
  # This is a filtering join"

  # final_predictors_one <- anti_join(observed_values, this_failing, by="SITE") # This works in R Studio,
  # but not in ML AZURE
  final_predictors_one <- observed_values[is.na(match(observed_values$SITE, this_failing$SITE)), ]
  # DONT SORT, if you do, don't use the SORTED array for prediction. it duplicates the results ******
  # final_predictors_one <- final_predictors_one[order(final_predictors_one$SITE),]
  # Print to see where the sorting is taking place
  #
  # Generate data for classification
  # Final Data for classification e.g. Linear discriminant Analysis (LDA) classifier/predictor
  #
  final_predictors <- data.frame(
    SITE                     <-  final_predictors_one$SITE,
    LATITUDE                 <-  final_predictors_one$LATITUDE,
    LONGITUDE                <-  final_predictors_one$LONGITUDE,
    LOG.ALTITUDE             <-  final_predictors_one$vld_alt_src_log,
    LOG.DISTANCE.FROM.SOURCE <-  final_predictors_one$vld_dist_src_log,
    LOG.WIDTH                <-  final_predictors_one$mn_width_log,
    LOG.DEPTH                <-  final_predictors_one$mn_depth_log,
    MEAN.SUBSTRATUM          <-  final_predictors_one$vld_substr_log,
    DISCHARGE.CATEGORY       <-  final_predictors_one$DISCHARGE,    #observed_values$disch_log,
    ALKALINITY               <-  final_predictors_one$ALKALINITY,
    LOG.ALKALINITY           <-  final_predictors_one$vld_alkal_log,
    LOG.SLOPE                <-  final_predictors_one$vld_slope_log,
    MEAN.AIR.TEMP            <-  final_predictors_one$TMEAN,
    AIR.TEMP.RANGE           <-  final_predictors_one$TRANGE
  )
  colnames(final_predictors) <- c("SITE",
                                  "LATITUDE",
                                  "LONGITUDE",
                                  "LOG.ALTITUDE",
                                  "LOG.DISTANCE.FROM.SOURCE",
                                  "LOG.WIDTH",
                                  "LOG.DEPTH",
                                  "MEAN.SUBSTRATUM",
                                  "DISCHARGE.CATEGORY",
                                  "ALKALINITY",
                                  "LOG.ALKALINITY",
                                  "LOG.SLOPE",
                                  "MEAN.AIR.TEMP",
                                  "AIR.TEMP.RANGE")

  #
  # #  Prediction Settings
  # 1. Enter
  # ## Uncomment: end_points <- read.csv(paste0(path,"/Test_Data_End_Point_Means_Copy.csv"))
  # ## Uncoment: DFMean_gd <- read.delim(paste0(path,"/df_mean_gb685.DAT"), header = FALSE, sep="", as.is=TRUE)
  # ## Uncoment::df_coeff_gb685 <- read.delim(paste0(path,"/df_coeff_gb685.DAT"), header = FALSE, sep="", as.is=TRUE)
  #
  # Prediction Settings
  # 2. Find the df_scores of each row using one line of coefficients df_coeff_gb685[1,-1] # removes the first column
  #
  # NRefg equals number of reference sites in end group g , for GB = 685, for NI = 11
  NRefg_all <- rowSums(nr_efg_groups[, -1])
  #
  # #DFScore_g <- DFCoef1 * Env1 + ... + DFCoefn * Envn ; remove "SITE" col=1 from final_predictors,
  # and  remove col=1 from df_coeff_gb685
  df_scores <- as.data.frame(getDFScores(final_predictors, df_coeff_gb685))
  #
  # Calculate the Mahanalobis disance of point x from site g for all referene sites
  MahDist_g <- getMahDist(df_scores, df_mean_gb685)
  MahDistNames <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14",
                    "p15", "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23", "p24", "p25", "p26", "p27", "p28",
                    "p29", "p30", "p31", "p32", "p33", "p34", "p35", "p36", "p37", "p38", "p39", "p40", "p41", "p42",
                    "p43")
  MahDistNames <- gsub("p", "Mah", MahDistNames)
  colnames(MahDist_g) <- MahDistNames

  # Calculate the minimum Mahanalobis disance of point x from site g
  MahDist_min <- getMahDist_min(df_scores, df_mean_gb685)
  # #Calculate the probability distribution
  PDist_g <- PDist(NRefg_all, MahDist_g)
  # Main dataframe needed:: Calculate probabilities of sites belonging to the endgroups,
  # prob_g, l,as last column 44 contrains the total "PGdistTot
  PDistTot <- as.data.frame(PDistTotal(PDist_g)) ## ALL probabilities p1..pn,  rowsums() add to 1,
  # except when last row which it "total" is removed i.e. rowSums(PDistTot[,-ncol(PDistTot)])=1
  # Rename the columns to probabilities p1,p2,...,p43
  colnames(PDistTot) <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14",
                          "p15", "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23", "p24", "p25", "p26", "p27",
                          "p28", "p29", "p30", "p31", "p32", "p33", "p34", "p35", "p36", "p37", "p38", "p39", "p40",
                          "p41", "p42", "p43", "Total")

  # #  final_predictors <- cbind(final_predictors, PDist_g[,-ncol(PDist_g)]) # This is the line we need
  # sum(final_predictors_try[1,-c(1:14)]) should give 1
  final_predictors_try1 <- cbind(final_predictors, PDistTot[, -ncol(PDistTot)])

  # head(final_predictors_try1,7)
  #
  # #3.Use chisquare to find suitability codes. Start for Britain GB, # # Could use a file for these chisquare values
  # # 1 = GB 21.02606 24.05393 26.21696 32.90923
  # # 2 = NI 18.30700 21.16080 23.20930 29.58830

  #
  chiSquare_vals <- data.frame(CQ1 = c(21.02606, 18.30700), CQ2 = c(24.05393, 21.16080), CQ3 = c(26.21696, 23.20930),
                               CQ4 = c(32.90923, 29.58830))
  suit_codes <- getSuitabilityCode(MahDist_min, chiSquare_vals)
  # # add suitab ility codes to the final data, using cbind
  final_predictors_try2 <- cbind(final_predictors_try1, suit_codes)
  # head(final_predictors_try2,7)
  #
  # #Write to csv :: # # write.csv(final_predictors_try, file = "finalPredictors_suitability.csv")
  #
  # Find max class group belongs to by getting the column name: use
  # belongs_to_end_grp <- colnames(final_predictors_try2[,15:57])[apply(final_predictors_try2[,15:57], 1, which.max)]
  # This sometimes returns a list, use unlist below to repair this
  belongs_to_end_grp <- colnames(final_predictors_try2[, 15:57])[apply(
    data.frame(matrix(unlist(final_predictors_try2[, 15:57]),
                      nrow = nrow(final_predictors_try2[, 15:57]),
                      byrow = T),
                stringsAsFactors = FALSE), 1, which.max)]

  #Relace p with EndGr
  belongs_to_end_grp <- gsub("p", "EndGr", belongs_to_end_grp)
  final_predictors_try3 <- cbind(final_predictors_try2, belongs_to_end_grp)

  # head(final_predictors_try3,7)
  # #4 Prediction: WE1.5 Algorithms for prediction of expected values of any index based on probability of end group
  # # membership and average values of the index amongst reference sites in each end group.
  # We predict WHPT NTAXA, and WHPT ASP

  getEndGroupMeansColsNeeded <- function(dframe) {
    dframe %>%
      # Don't select RIVAPCSMODEL since we know model what we are processing
      dplyr::filter(RIVPACS.Model == "RIVPACS IV GB") %>%
      dplyr::select(`End.Group`, `Season.Code`, `Season`, `TL2.WHPT.NTAXA..AbW.DistFam.`,
             `TL2.WHPT.ASPT..AbW.DistFam.`, `TL2.WHPT.NTAXA..AbW.CompFam.`, `TL2.WHPT.ASPT..AbW.CompFam.`)
  }

  endgroup_index_frame <- getEndGroupMeansColsNeeded(end_group_index)
  colnames(endgroup_index_frame) <- c("EndGrp", "SeasonCode", "Season",
                                      "TL2_WHPT_NTAXA_AbW_DistFam", "TL2_WHPT_ASPT_AbW_DistFam",
                                      "TL2_WHPT_NTAXA_AbW_CompFam", "TL2_WHPT_ASPT_AbW_CompFam")
  # Sort by the columns "EndGrp", "SeasonCode"
  endgroup_index_frame <- arrange(endgroup_index_frame, EndGrp, SeasonCode)

  # Prepare what you want to run - seasons, indices, and subset the data with the seasonCodes
  # seasons_to_run <- c(1,3) # add more seasons, :: USER INPUT
  # indices_to_run_old <- c(111,112,114, 115) # add more indices., TL2 WHPT NTAXA (AbW,DistFam),
  # index id = 111, TL2 WHPT ASPT (AbW,DistFam), index id = 112
  endgroup_index_frame <- dplyr::filter(endgroup_index_frame, SeasonCode %in% seasons_to_run)

  # Write a function that extracts user input columns and converts them to the values in c("") below :: USER INPUT
  indices_to_run <- c("TL2_WHPT_NTAXA_AbW_DistFam", "TL2_WHPT_ASPT_AbW_DistFam",
                      "TL2_WHPT_NTAXA_AbW_CompFam", "TL2_WHPT_ASPT_AbW_CompFam")

  # Run the index Scores
  # data_to_bindTo, season_to_run, index_id, end_group_IndexDFrame
  mainData <- getSeasonIndexScores(data_to_bindTo = final_predictors_try3,
                                          season_to_run = seasons_to_run,
                                          index_id = indices_to_run,
                                          end_group_IndexDFrame = endgroup_index_frame)

  #Remove failing sites from biological_data
  biological_data <- biological_data[!(biological_data$SITE %in% this_failing$SITE), ]
  #remove column "SITE", the first one of columns
  biological_data <- biological_data[, -1]

  #Append the biological data to the main output dataframe
  output <- cbind(mainData, biological_data)
  return(output)
}
