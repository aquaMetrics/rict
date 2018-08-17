#' Calculate River Invertebrate Classification Tool (RICT) predictions
#'
#' @param observed_values Dataframe of observed environmental values
#' @param expected_biotic_scores Dataframe of expected biotic scores for each
#' statistical grouping of rivers (referred to as 'end groups means').
#' @return Dataframe of predicted biotic scores and probility of observed values
#' falling into each statistical grouping of rivers.
#' @export
#'
#' @examples
#' \dontrun{
#' observed_values <- read.csv(file="inst/extdat/fba-test-whpt.csv")
#' expected_biotic_scores <- read.csv(file="inst/extdat/expected_biotic_scores.csv")
#' output <- calcRict(observed_values = observed_values,
#'                    expected_biotic_scores = expected_biotic_scores)
#' }
calcRict <- function(observed_values, expected_biotic_scores) {

  taxa_average_abundance <-
    read.csv(system.file("extdat", "TAXAAB.csv", package = "rict"))

  dfmean_gb685 <-
    read.delim(
      system.file("extdat", "DFMEAN_GB685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  dfcoeff_gb685 <-
    read.delim(
      system.file("extdat", "DFCOEFF_GB685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  # Change to character
  observed_values$Easting <- as.character(observed_values$Easting)
  observed_values$Northing <- as.character(observed_values$Northing)

  # Data validation
  # 1. MEAN_WIDTH, lower_bound=0.4, upper_bound=117
  valid_mean_width <-
    data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)) {
    valid_mean_width <-
      rbind(
        valid_mean_width,
        getValidEnvInput(observed_values$MEAN_WIDTH[i], 0.4, 117, "MEAN_WIDTH")
      )
  }
  # Change column names to suit env variable name, and cbind to original dataset
  colnames(valid_mean_width) <-
    paste0("mn_width_", noquote(colnames(valid_mean_width)))
  observed_values <- cbind(observed_values, valid_mean_width)
  head(observed_values, 3)

  # Data validation
  # 2. MEAN_DEPTH, lower_bound=1.7, upper_bound=300
  valid_mean_depth <-
    data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)) {
    valid_mean_depth <-
      rbind(
        valid_mean_depth,
        getValidEnvInput(observed_values$MEAN_DEPTH [i], 1.7, 300, "MEAN_DEPTH")
      )
  }
  colnames(valid_mean_depth) <-
    paste0("mn_depth_", noquote(colnames(valid_mean_depth)))
  observed_values <- cbind(observed_values, valid_mean_depth)

  # Data validation
  # 3. SLOPE, lower_bound=0.1, upper_bound=150
  valid_slope <-
    data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)) {
    valid_slope <-
      rbind(valid_slope,
            getValidEnvInput(observed_values$SLOPE [i], 0.1, 150, "SLOPE"))
  }
  colnames(valid_slope) <-
    paste0("vld_slope_", noquote(colnames(valid_slope))) # vld = valid
  observed_values <- cbind(observed_values, valid_slope)
  head(observed_values, 3)

  # Data validation
  # 4. DIST_FROM_SOURCE, lower_bound=0.1, upper_bound=202.8
  valid_dist_src <-
    data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)) {
    valid_dist_src <-
      rbind(
        valid_dist_src,
        getValidEnvInput(
          observed_values$DIST_FROM_SOURCE [i],
          0.1,
          202.8,
          "DIST_FROM_SOURCE"
        )
      )
  }
  colnames(valid_dist_src) <-
    paste0("vld_dist_src_", noquote(colnames(valid_dist_src))) # vld = valid
  observed_values <- cbind(observed_values, valid_dist_src)

  # Data validation
  # 5. ALTITUDE, has two sets of bounds, lower_bound=1, upper_bound=590, lower_low_bound=0, upper_up_bound = 1345
  # [0,1345] are hard coded, could be parameterised QED
  valid_altitude <-
    data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)) {
    valid_altitude <-
      rbind(valid_altitude,
            getValidEnvInput(observed_values$ALTITUDE [i], 1, 590, "ALTITUDE"))
  }
  colnames(valid_altitude) <-
    paste0("vld_alt_src_", noquote(colnames(valid_altitude))) # vld = valid
  observed_values <- cbind(observed_values, valid_altitude)

  # Data validation
  # 6. ALKALINITY, has bounds, lower_bound=1.2, upper_bound=366
  # getLogAlkalinity <- function (hardness, calcium, conduct, alkal, lower_b, upper_b)
  valid_alkalinity <-
    data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)) {
    valid_alkalinity <-
      rbind(
        valid_alkalinity,
        getLogAlkalinity(
          observed_values$HARDNESS[i],
          observed_values$CALCIUM[i],
          observed_values$CONDUCTIVITY[i],
          observed_values$ALKALINITY[i],
          1.2,
          366
        )
      )
  }
  # Above loop same as
  # thiscopy <- as.data.frame(with(observed_values, mapply(getLogAlkalinity, HARDNESS, CALCIUM, CONDUCTIVITY, ALKALINITY, 1.2, 366)))
  # thiscopy$V1$msg=="Succ"
  colnames(valid_alkalinity) <-
    paste0("vld_alkal_", noquote(colnames(valid_alkalinity))) # vld = valid
  observed_values <- cbind(observed_values, valid_alkalinity)

  # Data validation
  # 7. Validate SUBSTRATUM for sum of values "TOTSUB" in interval [97,103] exclussive,and MSUBSTR in interval [-8, 8].
  # Write to a file if errors found
  # Remove the site or records with such errors, and continue the prediction
  # getSubstrate <- function(bould_cob, pebbles_gr, snd, silt_cl, lower_b, upper_b)
  valid_substrate <-
    data.frame(log = as.numeric(), msg = as.character()) # Note that we don't use log for calculation of substrate
  for (i in 1:nrow(observed_values)) {
    valid_substrate <-
      rbind(
        valid_substrate,
        getSubstrate(
          observed_values$BOULDER_COBBLES[i],
          observed_values$PEBBLES_GRAVEL[i],
          observed_values$SAND[i],
          observed_values$SILT_CLAY[i],
          97,
          103
        )
      )
  }
  colnames(valid_substrate) <-
    paste0("vld_substr_", noquote(colnames(valid_substrate))) # vld = valid
  observed_values <- cbind(observed_values, valid_substrate)
  # observed_values %>%
  # subset(total>=97 & total<=103) %>%
  # select(-ends_with("total")) # Remove the column "total"
  #
  # Data validation and conversion
  # 8. Discharge category, bounds [0, 10]. Discharge calculated from velocity if not provided using width, depth
  valid_discharge <-
    data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(observed_values)) {
    valid_discharge <-
      rbind(
        valid_discharge,
        getLogDischarge(
          observed_values$MEAN_DEPTH[i],
          observed_values$MEAN_WIDTH[i],
          observed_values$DISCHARGE [i],
          observed_values$VELOCITY[i],
          0,
          10
        )
      )
  }
  colnames(valid_discharge) <-
    paste0("disch_", noquote(colnames(valid_discharge)))
  observed_values <- cbind(observed_values, valid_discharge)
  # Data validation and conversion
  # 9. Calculation of Lat/Long
  # Calculation of Lat/Long using BNG (British National Grids)
  concatenatedNGR <-
    with(observed_values, paste(NGR, substr(Easting, 1, 3), substr(Northing, 1, 3), sep = ""))
  # Use function getLatLong()
  # Uncomment:: lat.long <- with(observed_values, getLatLong(NGR,Easting, Northing, "WGS84") )
  # Use function getBNG()
  # Uncomment:: BNG <- with(observed_values, getBNG(NGR,Easting, Northing, "BNG") )
  BNG <-
    data.frame(
      easting = c(
        494200,
        422000,
        224600,
        404800,
        592100,
        407400,
        560400,
        443700,
        134400,
        251500,
        320300,
        263000,
        494200,
        422000,
        224600,
        404800,
        592100,
        407400,
        560400,
        443700,
        134400,
        251500,
        320300,
        263000
      ),
      northing = c(
        491000,
        364600,
        111600,
        325900,
        227200,
        110500,
        297000,
        1129000,
        655200,
        695500,
        858400,
        366300,
        491000,
        364600,
        111600,
        325900,
        227200,
        110500,
        297000,
        1129000,
        655200,
        695500,
        858400,
        366300
      )
    )
  # Assign to variables a s appropriate
  lat_long <-
    data.frame(
      lat = c(
        54.30592,
        53.17805,
        50.87721,
        52.83060,
        51.91020,
        50.89394,
        52.54722,
        60.04342,
        55.71722,
        56.12905,
        57.60853,
        53.17612,
        54.30592,
        53.17805,
        50.87721,
        52.83060,
        51.91020,
        50.89394,
        52.54722,
        60.04342,
        55.71722,
        56.12905,
        57.60853,
        53.17612
      ),
      lon = c(
        -0.5537803,
        -1.6722912,
        -4.4945403,
        -1.9301989,
         0.7914189,
        -1.8961545,
         0.3640949,
        -1.2173139,
        -6.2306341,
        -4.3907794,
        -3.3354977,
        -4.0512823,
        -0.5537803,
        -1.6722912,
        -4.4945403,
        -1.9301989,
         0.7914189,
        -1.8961545,
         0.3640949,
        -1.2173139,
        -6.2306341,
        -4.3907794,
        -3.3354977,
        -4.0512823
      )
    )
  observed_values$LATITUDE <- lat_long$lat # Uncomment:: lat.long$lat
  observed_values$LONGITUDE <- lat_long$lon # Uncomment :: lat.long$lon
  # Data validation and conversion
  # 10. Calculation of mean temperature (TMEAN), range temperature (TRANGE), using function calc.temps() from package "rnfra"
  # Lat long used for temperature lookups, using source MeanAirTempAirTempRangeASFunction.R
  # Uncomment::my.temperatures <- calc.temps(data.frame(
  #   Site_ID = observed_values$SITE,
  #   Easting4 = BNG$easting/100,
  #   Northing4 = BNG$northing/100,
  #   stringsAsFactors = FALSE))
  # #Assign to variables as appropriate
  # Uncomment:: observed_values$TMEAN <- my.temperatures$TMEAN
  # Uncomment::observed_values$TRANGE <- my.temperatures$TRANGE
  observed_values$TMEAN <-
    c(
      9.486258,
      9.749915,
      10.760873,
      9.590491,
      10.098385,
      10.558048,
      9.644871,
      7.530000,
      9.427188,
      8.757179,
      8.567249,
      10.224362,
      9.486258,
      9.749915,
      10.760873,
      9.590491,
      10.098385,
      10.558048,
      9.644871,
      7.530000,
      9.427188,
      8.757179,
      8.567249,
      10.224362
    )
  observed_values$TRANGE <-
    c(
      11.953139,
      12.562101,
      9.848472,
      12.650149,
      13.669552,
      12.495751,
      13.584181,
      8.880000,
      9.090752,
      12.241538,
      11.206061,
      10.298395,
      11.953139,
      12.562101,
      9.848472,
      12.650149,
      13.669552,
      12.495751,
      13.584181,
      8.880000,
      9.090752,
      12.241538,
      11.206061,
      10.298395
    )
  # ** PUT tthis function UP tOP before conversions? But we want to capture the Fails and warnings
  # Data validation and conversion
  # 11. Validation of Various Env. variables before transformation NO NEED for this as its been done by Helpfulfunctions.
  # observed_values$ALTITUDE <- replace(observed_values$ALTITUDE, observed_values$ALTITUDE ==0, 1)
  # observed_values$DIST_FROM_SOURCE <- replace(observed_values$DIST_FROM_SOURCE, observed_values$DIST_FROM_SOURCE <0.1, 0.1)
  # observed_values$MEAN_WIDTH <- replace(observed_values$MEAN_WIDTH, observed_values$MEAN_WIDTH <0.1, 0.1)
  # observed_values$MEAN_DEPTH <- replace(observed_values$MEAN_DEPTH, observed_values$MEAN_DEPTH <1.00, 1)
  # observed_values$DISCHARGE <- replace(observed_values$DISCHARGE, observed_values$DISCHARGE ==0, 0.1)
  # observed_values$ALKALINITY <- replace(observed_values$ALKALINITY, observed_values$ALKALINITY <0.1, 0.1)
  # observed_values$SLOPE<- replace(observed_values$SLOPE, observed_values$SLOPE <0.1, 0.1)

  # Data validation and conversion
  # 12. Write to file all Warnings and Failrures: SITE, MSG, iterate through the list of all variables with vld
  # WRITE TO LOG FILES all Warnings and Errors
  # 1. Warnings to log file :1
  # Deal with all warnings, save them in a file
  # Same as above, but using pipes, and using all the variables
  # assigned but not used: msg_columns <- names(select(observed_values, ends_with("_msg")))
  this_warning <- observed_values %>%
    filter(
      substr(vld_alt_src_msg, 1, 5) == "Warn:"    |
        substr(mn_width_msg, 1, 5) == "Warn:"
      |
        substr(mn_depth_msg, 1, 5) == "Warn:"     |
        substr(vld_alkal_msg, 1, 5) == "Warn:"
      |
        substr(disch_msg, 1, 5) == "Warn:"        |
        substr(vld_substr_msg, 1, 5) == "Warn:"
      |
        substr(vld_dist_src_msg, 1, 5) == "Warn:" |
        substr(vld_slope_msg, 1, 5) == "Warn:"
    )
  #  select("SITE","Year",msg_columns) # Select some columns
  # write.csv(this_warning, file = paste0(path,"/Warnings_file_data.csv"))
  #  which rows are these
  # observed_values[which(this_warning[1,1] %in% observed_values[,c("SITE")]),]
  #
  # 2. Failings to log file
  # Deal with all failings, save them in a file
  this_failing <- observed_values %>%
    filter(
      substr(vld_alt_src_msg, 1, 5) == "Fail:"    |
        substr(mn_width_msg, 1, 5) == "Fail:"
      |
        substr(mn_depth_msg, 1, 5) == "Fail:"     |
        substr(vld_alkal_msg, 1, 5) == "Fail:"
      |
        substr(disch_msg, 1, 5) == "Fail:"        |
        substr(vld_substr_msg, 1, 5) == "Fail:"
      |
        substr(vld_dist_src_msg, 1, 5) == "Fail:" |
        substr(vld_slope_msg, 1, 5) == "Fail:"
    )
  # select("SITE","Year",msg_columns) # Select some columns
  # write.csv(this_failing, file = paste0(path,"/Failings_file_data.csv"))
  # Put warnings and failures in a file of warnings_failings
  # assigned but not used Warnings_failings <- rbind(this_warning, this_failing)
  #
  # Data validation and conversion
  # 13.2 subset the instances to run in prediction by removing "this_failing",
  # use anti-join i.e."Return all rows from x where there are no matching values in y, keeping just columns from x.
  # This is a filtering join"
  final.predictors1 <- anti_join(observed_values, this_failing)
  # Generate data for classification
  # Final Data for classification e.g. Linear discriminant Analysis (LDA) classifier/predictor
  final.predictors <- data.frame(
    SITE                     =  final.predictors1$SITE,
    LATITUDE                 =  final.predictors1$LATITUDE,
    LONGITUDE                =  final.predictors1$LONGITUDE,
    LOG.ALTITUDE             =  final.predictors1$vld_alt_src_log,
    LOG.DISTANCE.FROM.SOURCE =  final.predictors1$vld_dist_src_log,
    LOG.WIDTH                =  final.predictors1$mn_width_log,
    LOG.DEPTH                =  final.predictors1$mn_depth_log,
    MEAN.SUBSTRATUM          =  final.predictors1$vld_substr_log,
    DISCHARGE.CATEGORY       =  final.predictors1$DISCHARGE,
    # observed_values$disch_log,
    ALKALINITY               =  final.predictors1$ALKALINITY,
    LOG.ALKALINITY           =  final.predictors1$vld_alkal_log,
    LOG.SLOPE                =  final.predictors1$vld_slope_log,
    MEAN.AIR.TEMP            =  final.predictors1$TMEAN,
    AIR.TEMP.RANGE           =  final.predictors1$TRANGE
  )
  colnames(final.predictors) <-
    c(
      "SITE",
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
      "AIR.TEMP.RANGE"
    )
  # Prediction Settings
  # 1. Enter
  # Uncomment: expected_biotic_scores <- read.csv(paste0(path,"/Test_Data_expected_biotic_scores_Copy.csv"))
  # Uncoment: dfmean_gb685 <- read.delim(paste0(path,"/DFMEAN_GB685.DAT"), header = FALSE, sep="", as.is=TRUE)
  # Uncoment::dfcoeff_gb685 <- read.delim(paste0(path,"/dfcoeff_gb685.DAT"), header = FALSE, sep="", as.is=TRUE)
  #
  # Prediction Settings
  # 2. Find the DFScores of each row using one line of coefficients dfcoeff_gb685[1,-1] # removes the first column
  # NRefg = number of reference sites in end group g , for GB = 685, for NI = 11
  NRefg <- 685

  # DFScore_g <- DFCoef1 * Env1 + ... + DFCoefn * Envn ; remove "SITE" col=1 from final.predictors, and  remove col=1 from dfcoeff_gb685
  DFScores <- as.data.frame(getDFScores(final.predictors, dfcoeff_gb685))
  # Calculate the Mahanalobis disance of point x from site g for all referene sites
  MahDist_g <- getMahDist(DFScores, dfmean_gb685)
  # for (i in 1:nrow(MahDist_g[i,]) { MahDist_g$min <- min(MahDist_g[i,]) }

  # Calculate the minimum Mahanalobis disance of point x from site g
  MahDist_min <- getMahDist_min(DFScores, dfmean_gb685)
  # Calculate the probability distribution
  PDist_g <- PDist(NRefg, MahDist_g)
  # Main dataframe needed:: Calculate probabilities of sites belonging to the endgroups, prob_g, l,as last
  # column 44 contrains the total "PGdistTot
  PDistTot <-
    as.data.frame(PDistTotal(PDist_g)) ## ALL probabilities p1..pn,  rowsums() add to 1, except when last row which
  # it "total" is removed i.e. rowSums(PDistTot[,-ncol(PDistTot)])=1
  # Combine with final data to see probabilities of each site wrt endGroups (43 of them, so 43 probabilites), per row
  # final.predictors <- cbind(final.predictors, PDist_g[,-ncol(PDist_g)]) # This is the line we need
  final.predictors_try <-
    cbind(final.predictors, PDistTot[,-ncol(PDistTot)]) # sum(final.predictors_try[1,-c(1:14)]) should give 1

  # 3.Use chisquare to find suitability codes. Start for Britain GB, Could use a file for these chisquare values
  # 1 = GB 21.02606 24.05393 26.21696 32.90923
  # 2 = NI 18.30700 21.16080 23.20930 29.58830
  chiSquare_vals <-
    data.frame(
      CQ1 = c(21.02606, 18.30700),
      CQ2 = c(24.05393, 21.16080),
      CQ3 = c(26.21696, 23.20930),
      CQ4 = c(32.90923, 29.58830)
    )
  suitCodes <- getSuitabilityCode(MahDist_min, chiSquare_vals)
  # add suitability codes to the final data, using cbind
  final.predictors_try <- cbind(final.predictors_try, suitCodes)

  # Find max class group belongs to by getting the column name: use
  BelongsTo_endGrp <-
    colnames(final.predictors_try[, 15:57])[apply(final.predictors_try[, 15:57], 1, which.max)]

  final.predictors_try <- cbind(final.predictors_try, BelongsTo_endGrp)
  # Write to csv :: # # write.csv(final.predictors_try, file = "finalPredictors_suitability.csv")
  # 4 Prediction: WE1.5 Algorithms for prediction of expected values of any index based on probability of end group
  # membership and average values of the index amongst reference sites in each end group. Algorithms for taxonomic
  # prediction (including prediction of occurrence and expected abundances
  endroup_IndexDFrame <-
    read.csv(system.file("extdat", "END_GROUP_INDEX.csv", package = "rict"),
             header = TRUE)

  # Remove empty columns, =  created, updated, egs_id, updated_by,

  endroup_IndexDFrame <- endroup_IndexDFrame[, c(2, 6, 7, 3)]
  # Choose season 7, index_id==1
  season_7 <-
    endroup_IndexDFrame[endroup_IndexDFrame$season_id == 7 &
                          endroup_IndexDFrame$index_id == 1,]

  # Group by end_group, value
  season_7_grp <- season_7[,-c(1, 3)] %>%
    group_by(end_group, value) %>%
    arrange(end_group)
  # Remove duplicates
  season_7_pred <-
    season_7_grp[!duplicated(season_7_grp$end_group),]

  # Filter out values for 43 sites, multiply with probabilities for prediction 4,8,9
  idx_mean <-
    getProbScores(final.predictors_try[, 15:57], season_7_pred[, 2]) # 15:57 are probability columns
  colnames(idx_mean) <- c("ExpIDXMean_ind_1_Seas7")
  # Append it to final
  final.predictors_try <- cbind(final.predictors_try, idx_mean)

  return(final.predictors_try)
}
