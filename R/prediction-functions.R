
# Begin Exclude Linting

# 1. getDFScores: Make sure the dimensions DFcoeff (m x n) maps to dimensions of EnvValues (n x k), result will be (m x k)
# Convert DFCoeff and EnvValues to matrix, finalCopy <- as.matrix(final.predictors[,-c(1)]), removing first and last column
# Similarly newDFcoeff <- as.matrix(DFCoeff_gb685[,-1])
# Return finalCopy %*% newDFcoeff

getDFScores  <- function (EnvValues, DFCoeff) {
  Env_i<- as.matrix(EnvValues[,-c(1)])
  Coeff_j <-  as.matrix(DFCoeff[,-1])
  return (Env_i %*% Coeff_j)
}

# 2. getDFScoresTotal: Returns the sums of all DFscores per site g
getDFScoresTotal <- function (EnvValues, DFCoeff) {
  return (rowSums(getDFScores(EnvValues, DFCoeff)))
}

#3. getMahdist: Calculate the Mahanalobis distance of point x from site g

getMahDist <- function (DFscore, meanvalues) {
  meanvalues <- as.matrix(meanvalues)
  DFscore <- as.matrix(DFscore)
  mah_Score <- matrix(0, nrow=nrow(DFscore), ncol = nrow(meanvalues))
  # Declare a matrix of zeros, with nrow, ncol dimensions
  for(row_dfscore in 1:nrow(DFscore)){
    for(row_means in 1:nrow(meanvalues)) {
      mah_Score[row_dfscore, row_means] <- sum((DFscore[row_dfscore,] - meanvalues[row_means,])^2)
      # apply rowSums() or sum()
    }
  }
  #l_mah_dist <- (meansA-valuesB)^2
  return(mah_Score)
}

#4.getMahDist_min:  Calculate the minimum Mahanalobis distance of point x from site g
getMahDist_min <- function (DFscore, meanvalues) {
  mahdist_min <- getMahDist(DFscore, meanvalues)
  toappend <-data.frame(min=c())
  for(i in 1:nrow(mahdist_min)) {
    toappend <- rbind(toappend,min(mahdist_min[i,]))
  }
  #Bind the result to the last column of input file "mahdist_min". No need to change "toappend1 to character as all are to be numeric
  names(toappend) <- c("minMah")
  return (cbind(mahdist_min,toappend))
}

# 5. getProbScores: Multiply end-group probabilities with IDXmean, Taxapr, Taxaab,
# Similar to DFScores() - combine them

getProbScores  <- function (Proball, IDXMean) {
  Env_i<- as.matrix(Proball)
  Coeff_j <-  as.matrix(IDXMean)
  return (Env_i %*% Coeff_j)
}


#6.PDist: Calculate the Probability distribution PDist_g for each site
#6.PDist: Calculate the Probability distribution PDist_g for each site
PDist <- function (nmref_sites, mahdist) {
  endGrp_Score <- matrix(0, nrow=nrow(mahdist), ncol = ncol(mahdist) )
  for(i in 1:nrow(mahdist)) {
    endGrp_Score[i,] <- nmref_sites*exp(-mahdist[i,]/2)
  }
  return (endGrp_Score)
}

PDist_old <- function (nmref_sites, mahdist) {
  return (nmref_sites*exp(-mahdist/2))
}

#7. PDistTotal: Calculate Total probabilities of all sites, bind the row sums to the last column

PDistTotal <- function (distr_g){
  return (cbind(distr_g/rowSums(distr_g),rowSums(distr_g)))
}

#8.getSuitabilityCode: Suitability code - input from getMahDist_min, and suitability codes
getSuitabilityCode <- function (minMahDist, suitCodes) {
  suit_frame <- as.character(data.frame(c(), c())) # Note rbind works with character data.frames
  for( i in 1:nrow(minMahDist)) { # Case 1
    if(minMahDist[i,ncol(minMahDist)]<suitCodes[1,"CQ1"]){ # for GB, row = 1
      #print(c("Here in loop case 1, row =",i))
      suit_frame <- rbind(suit_frame, c(1,">5%"))
    }#endif
    else { #Case 2
      if((suitCodes[1,"CQ1"]<=minMahDist[i,ncol(minMahDist)]) & (minMahDist[i,ncol(minMahDist)]<suitCodes[1,"CQ2"])){ # for GB, row = 1
        #print(c("Here in loop case 2, row =",i))
        suit_frame <- rbind(suit_frame, c(2,"<5%"))
      }#endif
      else { #Case 3
        if((suitCodes[1,"CQ2"]<=minMahDist[i,ncol(minMahDist)]) & (minMahDist[i,ncol(minMahDist)]<suitCodes[1,"CQ3"])){ # for GB, row = 1
          suit_frame <- rbind(suit_frame, c(3,"<2%"))
        }#endif
        else { #Case 4
          if((suitCodes[1,"CQ3"]<=minMahDist[i,ncol(minMahDist)]) & (minMahDist[i,ncol(minMahDist)]<suitCodes[1,"CQ4"])){ # for GB, row = 1
            suit_frame <- rbind(suit_frame, c(4,"<1%"))
          }#endif
          else{ #last case - no need for "if"
            if (minMahDist[i,ncol(minMahDist)]>=suitCodes[1,"CQ4"]){
              suit_frame <- rbind(suit_frame, c(5,"<0.1%"))
            }
          }#else last case
        }#else case 4
      }#else case 3
    }#else case 2
  }# for
  colnames(suit_frame) <- c("SuitCode","SuitText") # past0 to "log" for name of attribute/parameter
  return (suit_frame) # Return both message and log value
}


# 9. Get End group means from excel/csv file, filter only IV GB Model and rename column names and select the few columns

getEndGroupMeans_xlsx <- function(filepathname) {
  file <- readxl::read_excel(filepathname)
  file <- dplyr::rename(file, RIVPACSMODEL = .data$`RIVPACS Model`)
  file <- dplyr::rename(file, EndGrp = .data$`End Group`)
  file <- dplyr::rename(file, SeasonCode = .data$`Season Code`)
  file <- dplyr::rename(file, Season = .data$`Season`)
  file <- dplyr::rename(file, TL2_WHPT_NTAXA_AbW_DistFam = .data$`TL2 WHPT NTAXA (AbW,DistFam)`)
  file <- dplyr::rename(file, TL2_WHPT_ASPT_AbW_DistFam = .data$`TL2 WHPT ASPT (AbW,DistFam)`)
  file <- dplyr::rename(file, TL2_WHPT_NTAXA_AbW_CompFam = .data$`TL2 WHPT NTAXA (AbW,CompFam)`)
  file <- dplyr::rename(file, TL2_WHPT_ASPT_AbW_CompFam = .data$`TL2 WHPT ASPT (AbW,CompFam)`)
  file <- dplyr::filter(file, .data$RIVPACSMODEL == "RIVPACS IV GB")
  # Dont select RIVAPCSMODEL since we know model what we are processing
  file <-  dplyr::select(file, .data$`EndGrp`, .data$`SeasonCode`, .data$`Season`,
                  .data$`TL2_WHPT_NTAXA_AbW_DistFam`,
                  .data$`TL2_WHPT_ASPT_AbW_DistFam`,
                  .data$`TL2_WHPT_NTAXA_AbW_CompFam`,
                  .data$`TL2_WHPT_ASPT_AbW_CompFam`)
  return(file)
 }

#csv read version of getEndGroupMeans()

getEndGroupMeans <- function(filepathname) {
  end_group_means <- utils::read.csv(filepathname, header = TRUE)
  end_group_means <- dplyr::rename(end_group_means, RIVPACSMODEL = .data$`RIVPACS.Model`)
  end_group_means <- dplyr::rename(end_group_means, EndGrp = .data$`End.Group`)
  end_group_means <- dplyr::rename(end_group_means, SeasonCode = .data$`Season.Code`)
  end_group_means <- dplyr::rename(end_group_means, Season = .data$`Season`)
  end_group_means <- dplyr::rename(end_group_means, TL2_WHPT_NTAXA_AbW_DistFam = .data$`TL2.WHPT.NTAXA..AbW.DistFam.`)
  end_group_means <- dplyr::rename(end_group_means, TL2_WHPT_ASPT_AbW_DistFam = .data$`TL2.WHPT.ASPT..AbW.DistFam.`)
  end_group_means <- dplyr::rename(end_group_means, TL2_WHPT_NTAXA_AbW_CompFam = .data$`TL2.WHPT.NTAXA..AbW.CompFam.`)
  end_group_means <- dplyr::rename(end_group_means, TL2_WHPT_ASPT_AbW_CompFam = .data$`TL2.WHPT.ASPT..AbW.CompFam.`)
  end_group_means <- dplyr::filter(end_group_means, .data$RIVPACSMODEL == "RIVPACS IV GB") # Don't select
  # RIVAPCSMODEL since we know model what we are processing.
  end_group_means<- dplyr::select(end_group_means, .data$`EndGrp`, .data$`SeasonCode`, .data$`Season`,
                                  .data$`TL2_WHPT_NTAXA_AbW_DistFam`, .data$`TL2_WHPT_ASPT_AbW_DistFam`,
                                  .data$`TL2_WHPT_NTAXA_AbW_CompFam`, .data$`TL2_WHPT_ASPT_AbW_CompFam`)
  return(end_group_means)
}

# 10.getSeasonIndexScores: Calculate predictions of probability scores for indices WHPT, given season ids,
# whpt values. Use "getProbScores()"
getSeasonIndexScores <- function(data_to_bindTo, season_to_run, index_id, end_group_IndexDFrame){
  # Declare a matrix of zeros, with nrow, ncol dimensions
  # index_Score <- matrix(0, nrow=nrow(end_group_IndexDFrame), ncol = nrow(end_group_IndexDFrame))
  # mainDFrame <- data_to_bindTo

  # filter for Spring, SeasonCode==1, if exists
  spring_whpt_ntaxa_Abw_Dist <- NULL
  spring_whpt_aspt_Abw_Dist <- NULL
  spring_whpt_ntaxa_Abw_CompFam <- NULL
  spring_whpt_aspt_Abw_CompFam <- NULL

  if (1 %in% end_group_IndexDFrame$SeasonCode ){
    spring_whpt_all <- dplyr::filter(end_group_IndexDFrame, .data$SeasonCode == 1)
    # Check what index iit is you want , and getProbScores
    if ("TL2_WHPT_NTAXA_AbW_DistFam" %in% colnames(end_group_IndexDFrame)){ # column exists
      spring_whpt_ntaxa_Abw_Dist <- as.data.frame(getProbScores(Proball = data_to_bindTo[, paste0("p", 1:43)],
                                                                IDXMean = dplyr::select(spring_whpt_all,
                                                                       .data$TL2_WHPT_NTAXA_AbW_DistFam)))
      colnames(spring_whpt_ntaxa_Abw_Dist) <- c("TL2_WHPT_NTAXA_AbW_DistFam_spr")
      #print(nrow(spring_whpt_ntaxa_Abw_Dist))
      #print(spring_whpt_ntaxa_Abw_Dist)
    }

    if ("TL2_WHPT_ASPT_AbW_DistFam" %in% colnames(end_group_IndexDFrame)){ # column exists
      spring_whpt_aspt_Abw_Dist <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                               dplyr::select(spring_whpt_all,
                                                                      .data$TL2_WHPT_ASPT_AbW_DistFam)))
      colnames(spring_whpt_aspt_Abw_Dist) <- c("TL2_WHPT_ASPT_AbW_DistFam_spr")
      #print(nrow(spring_whpt_aspt_Abw_Dist))
      #print(spring_whpt_aspt_Abw_Dist)
    }

    if ("TL2_WHPT_NTAXA_AbW_CompFam" %in% colnames(end_group_IndexDFrame)){ # column exists
      spring_whpt_ntaxa_Abw_CompFam <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                   dplyr::select(spring_whpt_all,
                                                                          .data$TL2_WHPT_NTAXA_AbW_CompFam)))
      colnames(spring_whpt_ntaxa_Abw_CompFam) <- c("TL2_WHPT_NTAXA_AbW_CompFam_spr")
      #print(nrow(spring_whpt_ntaxa_Abw_CompFam))
      #print(spring_whpt_ntaxa_Abw_CompFam)
    }

    if ("TL2_WHPT_ASPT_AbW_CompFam" %in% colnames(end_group_IndexDFrame)){ # column exists
      spring_whpt_aspt_Abw_CompFam <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                  dplyr::select(spring_whpt_all,
                                                                         .data$TL2_WHPT_ASPT_AbW_CompFam)))
      colnames(spring_whpt_aspt_Abw_CompFam) <- c("TL2_WHPT_NTAXA_ASPT_CompFam_spr")
      # print(nrow(spring_whpt_aspt_Abw_CompFam))
      # print(spring_whpt_aspt_Abw_CompFam)
      # Change column_name to include spring
    }
  }

  # filter for autumn, SeasonCode == 3, if exists
  autumn_whpt_ntaxa_Abw_Dist <- NULL
  autumn_whpt_aspt_Abw_Dist <- NULL
  autumn_whpt_ntaxa_Abw_CompFam <- NULL
  autumn_whpt_aspt_Abw_CompFam <- NULL

  if (3 %in% end_group_IndexDFrame$SeasonCode ) {
    autumn_whpt_all <- dplyr::filter(end_group_IndexDFrame, .data$SeasonCode == 3)
    # Check what index iit is you want , and getProbScores
    if("TL2_WHPT_NTAXA_AbW_DistFam" %in% colnames(end_group_IndexDFrame) ){ # column exists
      autumn_whpt_ntaxa_Abw_Dist <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                dplyr::select(autumn_whpt_all,
                                                                       .data$TL2_WHPT_NTAXA_AbW_DistFam)))
      colnames(autumn_whpt_ntaxa_Abw_Dist) <- c("TL2_WHPT_NTAXA_AbW_DistFam_aut")
      # print(nrow(autumn_whpt_ntaxa_Abw_Dist))
      # print(autumn_whpt_ntaxa_Abw_Dist)
    }

    if ("TL2_WHPT_ASPT_AbW_DistFam" %in% colnames(end_group_IndexDFrame) ){ # column exists
      autumn_whpt_aspt_Abw_Dist <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                               dplyr::select(autumn_whpt_all,
                                                                      .data$TL2_WHPT_ASPT_AbW_DistFam)))
      colnames(autumn_whpt_aspt_Abw_Dist) <- c("TL2_WHPT_ASPT_AbW_DistFam_aut")
      # print(nrow(autumn_whpt_aspt_Abw_Dist))
      # print(autumn_whpt_aspt_Abw_Dist)
    }

    if("TL2_WHPT_NTAXA_AbW_CompFam" %in% colnames(end_group_IndexDFrame) ){ # column exists
      autumn_whpt_ntaxa_Abw_CompFam <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                   dplyr::select(autumn_whpt_all,
                                                                          .data$TL2_WHPT_NTAXA_AbW_CompFam)))
      colnames(autumn_whpt_ntaxa_Abw_CompFam) <- c("TL2_WHPT_NTAXA_AbW_CompFam_aut")
      # print(nrow(autumn_whpt_ntaxa_Abw_CompFam))
      # print(autumn_whpt_ntaxa_Abw_CompFam)
    }

    if("TL2_WHPT_ASPT_AbW_CompFam" %in% colnames(end_group_IndexDFrame) ){ # column exists
      autumn_whpt_aspt_Abw_CompFam <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                  dplyr::select(autumn_whpt_all,
                                                                                .data$TL2_WHPT_ASPT_AbW_CompFam)))
      colnames(autumn_whpt_aspt_Abw_CompFam) <- c("TL2_WHPT_ASPT_AbW_CompFam_aut")
      # print(nrow(autumn_whpt_aspt_Abw_CompFam))
      # print(autumn_whpt_aspt_Abw_CompFam)
    }
  }

  # filter for summer, SeasonCode == 2, if exists
  summer_whpt_ntaxa_Abw_Dist <- NULL
  summer_whpt_aspt_Abw_Dist <- NULL
  summer_whpt_ntaxa_Abw_CompFam <- NULL
  summer_whpt_aspt_Abw_CompFam <- NULL

  if (2 %in% end_group_IndexDFrame$SeasonCode) {
    summer_whpt_all <- dplyr::filter(end_group_IndexDFrame, .data$SeasonCode == 2)
    # Check what index iit is you want , and getProbScores
    if("TL2_WHPT_NTAXA_AbW_DistFam" %in% colnames(end_group_IndexDFrame) ){ # column exists
      summer_whpt_ntaxa_Abw_Dist <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                dplyr::select(summer_whpt_all,
                                                                       .data$TL2_WHPT_NTAXA_AbW_DistFam)))
      colnames(summer_whpt_ntaxa_Abw_Dist) <- c("TL2_WHPT_NTAXA_AbW_DistFam_sum")
      # print(nrow(autumn_whpt_ntaxa_Abw_Dist))
      # print(autumn_whpt_ntaxa_Abw_Dist)
    }

    if ("TL2_WHPT_ASPT_AbW_DistFam" %in% colnames(end_group_IndexDFrame)){ # column exists
      summer_whpt_aspt_Abw_Dist <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                               dplyr::select(summer_whpt_all,
                                                                             .data$TL2_WHPT_ASPT_AbW_DistFam)))
      colnames(summer_whpt_aspt_Abw_Dist) <- c("TL2_WHPT_ASPT_AbW_DistFam_sum")
      # print(nrow(autumn_whpt_aspt_Abw_Dist))
      # print(autumn_whpt_aspt_Abw_Dist)
    }

    if("TL2_WHPT_NTAXA_AbW_CompFam" %in% colnames(end_group_IndexDFrame)){ # column exists
      summer_whpt_ntaxa_Abw_CompFam <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                   dplyr::select(summer_whpt_all,
                                                                                 .data$TL2_WHPT_NTAXA_AbW_CompFam)))
      colnames(summer_whpt_ntaxa_Abw_CompFam) <- c("TL2_WHPT_NTAXA_AbW_CompFam_sum")
      # print(nrow(autumn_whpt_ntaxa_Abw_CompFam))
      # print(autumn_whpt_ntaxa_Abw_CompFam)
    }

    if("TL2_WHPT_ASPT_AbW_CompFam" %in% colnames(end_group_IndexDFrame)){ # column exists
      summer_whpt_aspt_Abw_CompFam <- as.data.frame(getProbScores(data_to_bindTo[, paste0("p", 1:43)],
                                                                  dplyr::select(summer_whpt_all,
                                                                         .data$TL2_WHPT_ASPT_AbW_CompFam)))
      colnames(summer_whpt_aspt_Abw_CompFam) <- c("TL2_WHPT_ASPT_AbW_CompFam_sum")
      # print(nrow(autumn_whpt_aspt_Abw_CompFam))
      # print(autumn_whpt_aspt_Abw_CompFam)
    }
  }

  # Spring Dist
  bind_all <- cbind(data_to_bindTo, spring_whpt_ntaxa_Abw_Dist)
  bind_all <- cbind(bind_all, spring_whpt_aspt_Abw_Dist)
  # Autumn Dist
  bind_all <- cbind(bind_all, autumn_whpt_ntaxa_Abw_Dist)
  bind_all <- cbind(bind_all, autumn_whpt_aspt_Abw_Dist)

  # Spring CompFam
  bind_all <- cbind(bind_all, spring_whpt_ntaxa_Abw_CompFam)
  bind_all <- cbind(bind_all, spring_whpt_aspt_Abw_CompFam)
  # Autumn CompFam
  bind_all <- cbind(bind_all, autumn_whpt_ntaxa_Abw_CompFam)
  bind_all <- cbind(bind_all, autumn_whpt_aspt_Abw_CompFam)

  # Summer Dist
  if(!is.null(summer_whpt_ntaxa_Abw_Dist)) {
  bind_all <- cbind(bind_all, summer_whpt_ntaxa_Abw_Dist)
  bind_all <- cbind(bind_all, summer_whpt_aspt_Abw_Dist)
  # Summer CompFam
  bind_all <- cbind(bind_all, summer_whpt_ntaxa_Abw_CompFam)
  bind_all <- cbind(bind_all, summer_whpt_aspt_Abw_CompFam)
  }
  return(bind_all)
}

# getSeasonIndexScores_old <- function (data_to_bindTo, season_to_run, index_id,
#                                       end_group_IndexDFrame, final.predictors_try){
#   #index_Score <- matrix(0, nrow=nrow(end_group_IndexDFrame), ncol = nrow(end_group_IndexDFrame) )
#   # Declare a matrix of zeros, with nrow, ncol dimensions
#   mainDFrame <- data_to_bindTo
#
#   # for each index you get, and for each season, produce a probability  score and add to the main dataset
#   for (i in 1:length(index_id)) {
#     # Choose all seasons, index_id==1
#     season_run <- end_group_IndexDFrame[(end_group_IndexDFrame$season_id %in% season_to_run)
#                                       & end_group_IndexDFrame$index_id == index_id[i], ]
#     #Group by end_group, season_id, value
#     season_all_grp <- season_run[, -c(1)] # Remove the index_id, leave "end_group", "season_id"", and "value"
#       season_grp <- dplyr::group_by(season_all_grp, .data$end_group, .data$season_id, .data$value)
#       dplyr::arrange(season_grp, .data$end_group)
#     #Remove any values with NA, if any occur
#     season_all_grp <- season_all_grp[stats::complete.cases(season_all_grp), ]
#
#     for(j in 1:length(season_to_run)) {
#       season_1 <- season_all_grp[season_all_grp$season_id == season_to_run[j], ]
#       season_1_pred <- season_1[!duplicated(season_1$end_group), ]
#       idx_mean_1 <- getProbScores(final.predictors_try[, 15:57], season_1_pred[, 3])
#       # 15:57 are probability columns * season_value
#       # Name columns according to the index_id and seasons here
#
#       if(index_id[i]==111 & season_to_run[j]==1)
#         colnames(idx_mean_1) <- c("TL2_WHPT_NTAXA_Abw_DistFam_Spring")
#       if(index_id[i]==111 & season_to_run[j]==3)
#         colnames(idx_mean_1) <- c("TL2_WHPT_NTAXA_Abw_DistFam_Autumn")
#       if(index_id[i]==112 & season_to_run[j]==1)
#         colnames(idx_mean_1) <- c("TL2_WHPT_ASPT_Abw_DistFam_Spring")
#       if(index_id[i]==112 & season_to_run[j]==3)
#         colnames(idx_mean_1) <- c("TL2_WHPT_ASPT_Abw_DistFam_Autumn")
#       ###
#       if(index_id[i]==114 & season_to_run[j]==1)
#         colnames(idx_mean_1) <- c("TL2_WHPT_NTAXA_Abw_CompFam_Spring") # Current uses 115 index - wrong use!!
#       if(index_id[i]==114 & season_to_run[j]==3)
#         colnames(idx_mean_1) <- c("TL2_WHPT_NTAXA_Abw_CompFam_Autumn")
#       ###
#       if(index_id[i]==115 & season_to_run[j]==1)
#         colnames(idx_mean_1) <- c("TL2_WHPT_ASPT_Abw_CompFam_Spring") # Current uses 115 index - wrong use!!
#       if(index_id[i]==115 & season_to_run[j]==3)
#         colnames(idx_mean_1) <- c("TL2_WHPT_ASPT_Abw_CompFam_Autumn")
#
#
#       # bind to the maindataframe
#       mainDFrame <- cbind(mainDFrame, idx_mean_1)
#
#     }
#   }
#
#   return (mainDFrame)
# }

##-------------------------------------------- old functions --------------

#Calculate the function scores, DFScore
getDFScore_old <- function (DFCoeff, EnvValues) {

  DFScore_d <- data.frame(matrix(0, nrow=nrow(EnvValues) ))
  #print( c("outloop, ",nrow(EnvValues)))
  for ( i in 1:nrow(EnvValues)) {
    #print(c("Dcoeff= ",as.numeric(DFCoeff[,-1][,1])))
    #print(c("Env = ",EnvValues[i,-1]))
    DFScore_d[i] <- (sum(as.numeric(DFCoeff[,-1][,1])*EnvValues[i,-1])) # I thin use just one column , column==1, of ceofficients for all Env variables
    # print(c(" in loop, DFScore = ", DFScore_d[i]))
  }
  #Use only numeric  return of rows equivalent to number of instances
  DFScores <- as.numeric(DFScore_d[1,])
  DFScores <- as.data.frame(DFScores)
  return (DFScores) # gives mutlipel values, only get row one, not nrows
} # Done, cbind this to original dataset

#Calculate Probabilities of Endgroup
# getProbEndGroup_old <- function (DFCoeff, EnvValues, DFMean, NRef_g) {
#   DFScore_d <- data.frame(matrix(0, nrow=nrow(DFCoeff) )) # make a dataframe
#   MahDist_g <- data.frame(matrix(0, nrow=nrow(DFCoeff) ))
#   PDist_g   <- data.frame(matrix(0, nrow=nrow(DFCoeff) ))
#   Prob_g    <-  data.frame(matrix(0, nrow=nrow(DFCoeff) ))
#   for(j in 2:nrow(DFCoeff)) {
#     DFScore_d [j-1,] <- sum(DFCoeff[,i] * EnvValues[i,-1])
#     MahDist_g [j-1,] <- sum((DFScore_d[,i]-DFMean[i,])^2)
#   }
#   #All should be in loop of end group = g
#   MahDist_min <- min(MahDist_g)
#   PDist_g     <- NRef_g*exp(-MahDist_g/2)
#   PDist_total <-  sum(PDist_g)
#   Prob_g      <- PDist_g/PDist_total
#   return (0)
# }

# Calculate the minimum Mahanalobis distance of point x from site g

getMahDist_min_old <- function (DFscore, meanvalues) {
  mah_Score <- matrix(0, nrow=nrow(DFscore), ncol = nrow(meanvalues) )
  for(row_dfscore in 1:nrow(DFscore)){
    for(row_means in 1:nrow(meanvalues)) {
      mah_Score[row_dfscore, row_means] <- min((DFscore[row_dfscore,] - meanvalues[row_means,])^2) # apply rowSums() or sum()
    }
  }
  return (mah_Score)
}

#Calculate Mahalabois distance
# getMahDist_old <- function (meansA, valuesB) {
#   l_mah_dist <- 0
#   for (i in 1: ncol(meansA)) {
#     l_mah_dist <- l_mah_dist + (valuesB - meansA)^2;
#     l_pDist    <- l_NRef * exp( (-1*l_mah_dist) /2);
#   }
#   return (l_mah_dist)
# }

# End Exclude Linting
