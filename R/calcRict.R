#' Calculate River Invertebrate Classification Tool(RICT) predictions
#'
#' @param environmentalVariables
#' Dataframe of observed environmental values
#' @param endPointMeans
#' Dataframe of endPointMeans
#' @return
#' Dataframe of predicted values
#' @export
#'
#' @examples
#' \dontrun{
#' environmentalVariables <- read.csv(file="inst/extdat/fba-test-WHPT.csv")
#' endPointMeans <- read.csv(file="inst/extdat/End_Group_Means.csv")
#' output <- calcRict(environmentalVariables=environmentalVariables, endPointMeans=endPointMeans)
#' }
calcRict <- function(environmentalVariables, endPointMeans) {
  # some variable name switching - needed to make Azure ML code to into package function
  # More explicit/unambiguous variable names required?
  EndPtMeans <- endPointMeans
  raw.input.data <- environmentalVariables

  #install.packages(c("cowplot", "plyr", "httr", "xml2", "stringr", "xts", "rjson", "ggmap", "ggplot2", "sp", "rgdal", "parallel"))
  #install.packages("inst/extdat/rnrfa_1.4.0.zip", repos = NULL, verbose = TRUE, dependencies = TRUE)
  #install.packages("https://cran.r-project.org/bin/windows/contrib/3.6/rnrfa_1.4.0.zip", repos = NULL, verbose = TRUE, dependencies = TRUE)
  #install.packages("src/validationpredictor_functions.zip", lib = ".", repos = NULL, verbose = TRUE, dependencies = TRUE)

  #library(rnrfa)
  #library(tidyverse)
   library(dplyr)

  #source("src/Predictionfunctions.R")
  #source("src/Helperfunctionsv1.R")
  #source("src/MeanAirTempAirTempRangeASFunction.R")
  taxaabDFrame <- read.csv(system.file("extdat", "TAXAAB.csv", package = "rict"))


  #EndPoints <- read.csv("inst\extdat\Test_Data_End_Point_Means_Copy.csv")
  EndPoints <- EndPtMeans

  DFMean_gd <- read.delim(system.file("extdat", "DFMEAN_GB685.DAT", package = "rict"),
                          header = FALSE, sep = "", as.is = TRUE)


  DFCoeff_gb685 <- read.delim(system.file("extdat", "DFCOEFF_GB685.DAT", package = "rict"),
                              header = FALSE, sep = "", as.is = TRUE)

  head(DFCoeff_gb685, 7)

  # #Change to character
  raw.input.data$Easting <- as.character(raw.input.data$Easting)
  raw.input.data$Northing <- as.character(raw.input.data$Northing)
  #
  # # Data validation
  # # 1. MEAN_WIDTH, lower_bound=0.4, upper_bound=117
  #
  valid_mean_width <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(raw.input.data)){
    valid_mean_width <- rbind(valid_mean_width, getValidEnvInput(raw.input.data$MEAN_WIDTH[i], 0.4, 117, "MEAN_WIDTH"))
  }
  # # Change column names to suit env variable name, and cbind to original dataset
  colnames(valid_mean_width) <- paste0("mn_width_", noquote(colnames(valid_mean_width)))
  raw.input.data <- cbind(raw.input.data, valid_mean_width)
  head(raw.input.data, 3)

  # # Data validation
  # # 2. MEAN_DEPTH, lower_bound=1.7, upper_bound=300

  valid_mean_depth <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(raw.input.data)){
    valid_mean_depth <- rbind(valid_mean_depth, getValidEnvInput(raw.input.data$MEAN_DEPTH [i], 1.7, 300, "MEAN_DEPTH"))
  }
  colnames(valid_mean_depth) <- paste0("mn_depth_", noquote(colnames(valid_mean_depth)))
  raw.input.data <- cbind(raw.input.data, valid_mean_depth)
  head(raw.input.data, 3)

  # # Data validation
  # # 3. SLOPE, lower_bound=0.1, upper_bound=150

  valid_slope <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(raw.input.data)){
    valid_slope <- rbind(valid_slope, getValidEnvInput(raw.input.data$SLOPE [i], 0.1, 150, "SLOPE"))
  }
  colnames(valid_slope) <- paste0("vld_slope_", noquote(colnames(valid_slope))) # vld = valid
  raw.input.data <- cbind(raw.input.data, valid_slope)
  head(raw.input.data, 3)

  # # Data validation
  # # 4. DIST_FROM_SOURCE, lower_bound=0.1, upper_bound=202.8
  #
  valid_dist_src <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(raw.input.data)){
    valid_dist_src <- rbind(valid_dist_src, getValidEnvInput(raw.input.data$DIST_FROM_SOURCE [i], 0.1, 202.8, "DIST_FROM_SOURCE"))
  }
  colnames(valid_dist_src) <- paste0("vld_dist_src_", noquote(colnames(valid_dist_src))) # vld = valid
  raw.input.data <- cbind(raw.input.data, valid_dist_src)
  #
  # # Data validation
  # # 5. ALTITUDE, has two sets of bounds, lower_bound=1, upper_bound=590, lower_low_bound=0, upper_up_bound = 1345
  # #[0,1345] are hard coded, could be parameterised QED
  #
  #
  valid_altitude <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(raw.input.data)){
    valid_altitude <- rbind(valid_altitude, getValidEnvInput(raw.input.data$ALTITUDE [i], 1, 590, "ALTITUDE"))
  }
  colnames(valid_altitude) <- paste0("vld_alt_src_", noquote(colnames(valid_altitude))) # vld = valid
  raw.input.data <- cbind(raw.input.data, valid_altitude)
  #
  #
  # # Data validation
  # # 6. ALKALINITY, has bounds, lower_bound=1.2, upper_bound=366
  # # getLogAlkalinity <- function (hardness, calcium, conduct, alkal, lower_b, upper_b)
  #
  valid_alkalinity <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(raw.input.data)){
    valid_alkalinity <- rbind(valid_alkalinity, getLogAlkalinity(raw.input.data$HARDNESS[i], raw.input.data$CALCIUM[i], raw.input.data$CONDUCTIVITY[i], raw.input.data$ALKALINITY[i], 1.2, 366))
  }
  # #Above loop same as # thiscopy <- as.data.frame(with(raw.input.data, mapply(getLogAlkalinity, HARDNESS, CALCIUM, CONDUCTIVITY, ALKALINITY, 1.2, 366)))
  # # thiscopy$V1$msg=="Succ"
  colnames(valid_alkalinity) <- paste0("vld_alkal_", noquote(colnames(valid_alkalinity))) # vld = valid
  raw.input.data <- cbind(raw.input.data, valid_alkalinity)

  # # Data validation
  # # 7. Validate SUBSTRATUM for sum of values "TOTSUB" in interval [97,103] exclussive,and MSUBSTR in interval [-8, 8]. Write to a file if errors found
  # # Remove the site or records with such errors, and continue the prediction
  #
  # # getSubstrate <- function(bould_cob, pebbles_gr, snd, silt_cl, lower_b, upper_b)
  #
  valid_substrate <- data.frame(log = as.numeric(), msg = as.character()) # Note that we don't use log for calculation of substrate
  for (i in 1:nrow(raw.input.data)){
    valid_substrate <- rbind(valid_substrate, getSubstrate(raw.input.data$BOULDER_COBBLES[i], raw.input.data$PEBBLES_GRAVEL[i], raw.input.data$SAND[i], raw.input.data$SILT_CLAY[i], 97, 103))
  }
  colnames(valid_substrate) <- paste0("vld_substr_", noquote(colnames(valid_substrate))) # vld = valid
  raw.input.data <- cbind(raw.input.data, valid_substrate)
  #  #raw.input.data %>%
  #  #  subset(total>=97 & total<=103) %>%
  # #    select(-ends_with("total")) # Remove the column "total"
  #
  # # Data validation and conversion
  # # 8. Discharge category, bounds [0, 10]. Discharge calculated from velocity if not provided using width, depth
  #
  valid_discharge <- data.frame(log = as.numeric(), msg = as.character())
  for (i in 1:nrow(raw.input.data)){
    valid_discharge <- rbind(valid_discharge, getLogDischarge(raw.input.data$MEAN_DEPTH[i], raw.input.data$MEAN_WIDTH[i], raw.input.data$DISCHARGE [i], raw.input.data$VELOCITY[i], 0, 10))
  }
  colnames(valid_discharge) <- paste0("disch_", noquote(colnames(valid_discharge)))
  raw.input.data <- cbind(raw.input.data, valid_discharge)
  #
  #
  # # Data validation and conversion
  # # 9. Calculation of Lat/Long
  #
  # # Calculation of Lat/Long using BNG (British National Grids)
  concatenatedNGR <- with(raw.input.data, paste(NGR, substr(Easting, 1, 3), substr(Northing, 1, 3), sep = ""))
  # #Use function getLatLong()
  # ## Uncomment:: lat.long <- with(raw.input.data, getLatLong(NGR,Easting, Northing, "WGS84") )
  # # Use function getBNG()
  # ## Uncomment:: BNG <- with(raw.input.data, getBNG(NGR,Easting, Northing, "BNG") )
  BNG <- data.frame(easting = c(494200, 422000, 224600, 404800, 592100, 407400, 560400, 443700, 134400, 251500, 320300, 263000, 494200, 422000, 224600, 404800, 592100, 407400, 560400, 443700, 134400,
                                251500, 320300, 263000),
                    northing = c(491000, 364600, 111600,  325900,  227200, 110500, 297000, 1129000, 655200,  695500,  858400,  366300,  491000,  364600, 111600,
                                 325900, 227200, 110500,  297000, 1129000, 655200, 695500, 858400, 366300))
  # #Assign to variables a s appropriate
  lat_long <- data.frame(lat = c(54.30592, 53.17805, 50.87721, 52.83060, 51.91020, 50.89394, 52.54722, 60.04342, 55.71722, 56.12905, 57.60853, 53.17612, 54.30592, 53.17805, 50.87721, 52.83060,
                                 51.91020, 50.89394, 52.54722, 60.04342, 55.71722, 56.12905, 57.60853, 53.17612),
                         lon = c(-0.5537803, -1.6722912, -4.4945403, -1.9301989, 0.7914189, -1.8961545, 0.3640949, -1.2173139, -6.2306341, -4.3907794, -3.3354977, -4.0512823, -0.5537803,
                                 -1.6722912, -4.4945403, -1.9301989, 0.7914189, -1.8961545,  0.3640949, -1.2173139, -6.2306341, -4.3907794, -3.3354977, -4.0512823))
  raw.input.data$LATITUDE <- lat_long$lat # Uncomment:: lat.long$lat
  raw.input.data$LONGITUDE <- lat_long$lon # Uncomment :: lat.long$lon
  #
  # # Data validation and conversion
  # # 10. Calculation of mean temperature (TMEAN), range temperature (TRANGE), using function calc.temps() from package "rnfra"
  #
  # # Lat long used for temperature lookups, using source MeanAirTempAirTempRangeASFunction.R
  # Uncomment::my.temperatures <- calc.temps(data.frame(
  #   Site_ID = raw.input.data$SITE,
  #   Easting4 = BNG$easting/100,
  #   Northing4 = BNG$northing/100,
  #   stringsAsFactors = FALSE))
  # #Assign to variables as appropriate
  # Uncomment:: raw.input.data$TMEAN <- my.temperatures$TMEAN
  # Uncomment::raw.input.data$TRANGE <- my.temperatures$TRANGE
  raw.input.data$TMEAN <- c(9.486258, 9.749915, 10.760873,  9.590491, 10.098385, 10.558048, 9.644871, 7.530000, 9.427188, 8.757179,
                            8.567249, 10.224362, 9.486258, 9.749915, 10.760873, 9.590491, 10.098385, 10.558048, 9.644871, 7.530000, 9.427188,
                            8.757179, 8.567249, 10.224362)
  raw.input.data$TRANGE <- c(11.953139, 12.562101,  9.848472, 12.650149, 13.669552, 12.495751, 13.584181, 8.880000, 9.090752,
                             12.241538, 11.206061, 10.298395, 11.953139, 12.562101, 9.848472, 12.650149, 13.669552, 12.495751,
                             13.584181, 8.880000, 9.090752, 12.241538, 11.206061, 10.298395)
  head(raw.input.data, 7)
  #
  # # ** PUT tthis function UP tOP before conversions? But we want to capture the Fails and warnings
  # # Data validation and conversion
  # # 11. Validation of Various Env. variables before transformation NO NEED for this as its been done by Helpfulfunctions.
  # # raw.input.data$ALTITUDE <- replace(raw.input.data$ALTITUDE, raw.input.data$ALTITUDE ==0, 1)
  #   #  raw.input.data$DIST_FROM_SOURCE <- replace(raw.input.data$DIST_FROM_SOURCE, raw.input.data$DIST_FROM_SOURCE <0.1, 0.1)
  #   #  raw.input.data$MEAN_WIDTH <- replace(raw.input.data$MEAN_WIDTH, raw.input.data$MEAN_WIDTH <0.1, 0.1)
  #   #  raw.input.data$MEAN_DEPTH <- replace(raw.input.data$MEAN_DEPTH, raw.input.data$MEAN_DEPTH <1.00, 1)
  #   #  raw.input.data$DISCHARGE <- replace(raw.input.data$DISCHARGE, raw.input.data$DISCHARGE ==0, 0.1)
  #   #  raw.input.data$ALKALINITY <- replace(raw.input.data$ALKALINITY, raw.input.data$ALKALINITY <0.1, 0.1)
  #   # raw.input.data$SLOPE<- replace(raw.input.data$SLOPE, raw.input.data$SLOPE <0.1, 0.1)
  #
  #
  # # Data validation and conversion
  # # 12. Write to file all Warnings and Failrures: SITE, MSG, iterate through the list of all variables with vld
  #

  # # WRITE TO LOG FILES all Warnings and Errors
  # # 1. Warnings to log file :1
  #
  # # Deal with all warnings, save them in a file
  # #Same as above, but using pipes, and using all the variables
  #assigned but not used: msg_columns <- names(select(raw.input.data, ends_with("_msg")))
  this_warning <- raw.input.data %>%
    filter(substr(vld_alt_src_msg, 1, 5) == "Warn:"    | substr(mn_width_msg, 1, 5) == "Warn:"
           | substr(mn_depth_msg, 1, 5) == "Warn:"     | substr(vld_alkal_msg, 1, 5) == "Warn:"
           | substr(disch_msg, 1, 5) == "Warn:"        | substr(vld_substr_msg, 1, 5) == "Warn:"
           | substr(vld_dist_src_msg, 1, 5) == "Warn:" | substr(vld_slope_msg, 1, 5) == "Warn:")
  #  select("SITE","Year",msg_columns) # Select some columns
  # # write.csv(this_warning, file = paste0(path,"/Warnings_file_data.csv"))
  # # which rows are these
  # # raw.input.data[which(this_warning[1,1] %in% raw.input.data[,c("SITE")]),]
  #
  # #2. Failings to log file
  #
  # # Deal with all failings, save them in a file
  this_failing <- raw.input.data %>%
    filter(substr(vld_alt_src_msg, 1, 5) == "Fail:"    | substr(mn_width_msg, 1, 5) == "Fail:"
           | substr(mn_depth_msg, 1, 5) == "Fail:"     | substr(vld_alkal_msg, 1, 5) == "Fail:"
           | substr(disch_msg, 1, 5) == "Fail:"        | substr(vld_substr_msg, 1, 5) == "Fail:"
           | substr(vld_dist_src_msg, 1, 5) == "Fail:" | substr(vld_slope_msg, 1, 5) == "Fail:")
  # select("SITE","Year",msg_columns) # Select some columns
  # # write.csv(this_failing, file = paste0(path,"/Failings_file_data.csv"))
  # Put warnings and failures in a file of warnings_failings
  # assigned but not used Warnings_failings <- rbind(this_warning, this_failing)
  #
  # # Data validation and conversion
  # # 13.2 subset the instances to run in prediction by removing "this_failing", use anti-join i.e."Return all rows from x where there are no matching values in y, keeping just columns from x.
  # # This is a filtering join"
  #
  final.predictors1 <- anti_join(raw.input.data, this_failing)
  #
  # # Generate data for classification
  #  # Final Data for classification e.g. Linear discriminant Analysis (LDA) classifier/predictor
  #
  final.predictors <- data.frame(
    SITE                     <-  final.predictors1$SITE,
    LATITUDE                 <-  final.predictors1$LATITUDE,
    LONGITUDE                <-  final.predictors1$LONGITUDE,
    LOG.ALTITUDE             <-  final.predictors1$vld_alt_src_log,
    LOG.DISTANCE.FROM.SOURCE <-  final.predictors1$vld_dist_src_log,
    LOG.WIDTH                <-  final.predictors1$mn_width_log,
    LOG.DEPTH                <-  final.predictors1$mn_depth_log,
    MEAN.SUBSTRATUM          <-  final.predictors1$vld_substr_log,
    DISCHARGE.CATEGORY       <-  final.predictors1$DISCHARGE,    #raw.input.data$disch_log,
    ALKALINITY               <-  final.predictors1$ALKALINITY,
    LOG.ALKALINITY           <-  final.predictors1$vld_alkal_log,
    LOG.SLOPE                <-  final.predictors1$vld_slope_log,
    MEAN.AIR.TEMP            <-  final.predictors1$TMEAN,
    AIR.TEMP.RANGE           <-  final.predictors1$TRANGE
  )
  colnames(final.predictors) <- c("SITE", "LATITUDE", "LONGITUDE", "LOG.ALTITUDE", "LOG.DISTANCE.FROM.SOURCE", "LOG.WIDTH",
                                  "LOG.DEPTH", "MEAN.SUBSTRATUM", "DISCHARGE.CATEGORY", "ALKALINITY", "LOG.ALKALINITY", "LOG.SLOPE",
                                  "MEAN.AIR.TEMP", "AIR.TEMP.RANGE")
  #
  # #  Prediction Settings
  # ## 1. Enter
  # ##Uncomment: EndPoints <- read.csv(paste0(path,"/Test_Data_End_Point_Means_Copy.csv"))
  # ## Uncoment: DFMean_gd <- read.delim(paste0(path,"/DFMEAN_GB685.DAT"), header = FALSE, sep="", as.is=TRUE)
  # ## Uncoment::DFCoeff_gb685 <- read.delim(paste0(path,"/DFCOEFF_GB685.DAT"), header = FALSE, sep="", as.is=TRUE)
  #
  # #   Prediction Settings
  # #2. Find the DFScores of each row using one line of coefficients DFCoeff_gb685[1,-1] # removes the first column
  #
  # # NRefg = number of reference sites in end group g , for GB = 685, for NI = 11
  NRefg <- 685
  #
  # #DFScore_g <- DFCoef1 * Env1 + ... + DFCoefn * Envn ; remove "SITE" col=1 from final.predictors, and  remove col=1 from DFCoeff_gb685
  DFScores <- as.data.frame(getDFScores(final.predictors, DFCoeff_gb685))
  #
  # # Calculate the Mahanalobis disance of point x from site g for all referene sites
  MahDist_g <- getMahDist(DFScores, DFMean_gd)
  # # for (i in 1:nrow(MahDist_g[i,]) { MahDist_g$min <- min(MahDist_g[i,]) }
  # # Calculate the minimum Mahanalobis disance of point x from site g
  MahDist_min <- getMahDist_min(DFScores, DFMean_gd)
  # #Calculate the probability distribution
  PDist_g <- PDist(NRefg, MahDist_g)
  # #Main dataframe needed:: Calculate probabilities of sites belonging to the endgroups, prob_g, l,as last column 44 contrains the total "PGdistTot
  PDistTot <- as.data.frame(PDistTotal(PDist_g)) ## ALL probabilities p1..pn,  rowsums() add to 1, except when last row which it "total" is removed i.e. rowSums(PDistTot[,-ncol(PDistTot)])=1
  # #Combine with final data to see probabilities of each site wrt endGroups (43 of them, so 43 probabilites), per row
  # #   final.predictors <- cbind(final.predictors, PDist_g[,-ncol(PDist_g)]) # This is the line we need
  final.predictors_try <- cbind(final.predictors, PDistTot[, -ncol(PDistTot)]) # sum(final.predictors_try[1,-c(1:14)]) should give 1
  #
  # #3.Use chisquare to find suitability codes. Start for Britain GB, # # Could use a file for these chisquare values
  # # 1 = GB 21.02606 24.05393 26.21696 32.90923
  # # 2 = NI 18.30700 21.16080 23.20930 29.58830

  #
  chiSquare_vals <- data.frame(CQ1 = c(21.02606, 18.30700), CQ2 = c(24.05393, 21.16080), CQ3 = c(26.21696, 23.20930), CQ4 = c(32.90923, 29.58830))
  suitCodes <- getSuitabilityCode(MahDist_min, chiSquare_vals)
  # # add suitab ility codes to the final data, using cbind
  final.predictors_try <- cbind(final.predictors_try, suitCodes)

  # Find max class group belongs to by getting the column name: use

  BelongsTo_endGrp <- colnames(final.predictors_try[, 15:57])[apply(final.predictors_try[, 15:57], 1, which.max)]

  final.predictors_try <- cbind(final.predictors_try, BelongsTo_endGrp)

  #
  # #Write to csv :: # # write.csv(final.predictors_try, file = "finalPredictors_suitability.csv")
  #
  # #4 Prediction: WE1.5 Algorithms for prediction of expected values of any index based on probability of end group
  # # membership and average values of the index amongst reference sites in each end group. Algorithms for taxonomic
  # # prediction (including prediction of occurrence and expected abundances
  #
  endroup_IndexDFrame <- read.csv(system.file("extdat", "END_GROUP_INDEX.csv", package = "rict"), header = TRUE)

  #Remove empty columns, =  created, updated, egs_id, updated_by,

  endroup_IndexDFrame <- endroup_IndexDFrame[, c(2, 6, 7, 3)]
  ## Choose season 7, index_id==1
  season_7 <- endroup_IndexDFrame[endroup_IndexDFrame$season_id == 7 & endroup_IndexDFrame$index_id == 1, ]

  ## Group by end_group, value
  season_7_grp <- season_7[, -c(1, 3)] %>%
    group_by(end_group, value) %>%
    arrange(end_group)
  ## Remove duplicates
  season_7_pred <- season_7_grp[!duplicated(season_7_grp$end_group), ]

  #Filter out valuesfor 43 sites, multiply with probabilities for prediction 4,8,9
  idx_mean <- getProbScores(final.predictors_try[, 15:57], season_7_pred[, 2]) # 15:57 are probability columns
  colnames(idx_mean) <- c("ExpIDXMean_ind_1_Seas7")
  ## Append it to final
  final.predictors_try <- cbind(final.predictors_try, idx_mean)

  return(final.predictors_try)
}
