
# Begin Exclude Linting

# 1. getDFScores: Make sure the dimensions DFcoeff (m x n) maps to dimensions of EnvValues (n x k), result will be (m x k)
# Convert DFCoeff and EnvValues to matrix, finalCopy <- as.matrix(final.predictors[,-c(1)]), removing first and last column
# Similarly newDFcoeff <- as.matrix(DFCoeff_gb685[,-1])
# Return finalCopy %*% newDFcoeff

getDFScores <- function(EnvValues, DFCoeff) {
  Env_i <- as.matrix(EnvValues[, -c(1)])
  Coeff_j <- as.matrix(DFCoeff[, -1])
  return(Env_i %*% Coeff_j)
}

# 2. getDFScoresTotal: Returns the sums of all DFscores per site g
getDFScoresTotal <- function(EnvValues, DFCoeff) {
  return(rowSums(getDFScores(EnvValues, DFCoeff)))
}

# 3. getMahdist: Calculate the Mahanalobis distance of point x from site g

getMahDist <- function(DFscore, meanvalues) {
  meanvalues <- as.matrix(meanvalues)
  DFscore <- as.matrix(DFscore)
  mah_Score <- matrix(0, nrow = nrow(DFscore), ncol = nrow(meanvalues))
  # Declare a matrix of zeros, with nrow, ncol dimensions
  for (row_dfscore in 1:nrow(DFscore)) {
    for (row_means in 1:nrow(meanvalues)) {
      mah_Score[row_dfscore, row_means] <- sum((DFscore[row_dfscore, ] - meanvalues[row_means, ])^2)
      # apply rowSums() or sum()
    }
  }
  # l_mah_dist <- (meansA-valuesB)^2
  return(mah_Score)
}

# 4.getMahDist_min:  Calculate the minimum Mahanalobis distance of point x from site g
getMahDist_min <- function(DFscore, meanvalues) {
  mahdist_min <- getMahDist(DFscore, meanvalues)
  toappend <- data.frame(min = c())
  for (i in 1:nrow(mahdist_min)) {
    toappend <- rbind(toappend, min(mahdist_min[i, ]))
  }
  # Bind the result to the last column of input file "mahdist_min". No need to change "toappend1 to character as all are to be numeric
  names(toappend) <- c("minMah")
  return(cbind(mahdist_min, toappend))
}

# 5. getProbScores: Multiply end-group probabilities with IDXmean, Taxapr, Taxaab,
# Similar to DFScores() - combine them

getProbScores <- function(Proball, IDXMean) {
  Env_i <- as.matrix(Proball)
  Coeff_j <- as.matrix(IDXMean)
  return(Env_i %*% Coeff_j)
}


# 6.PDist: Calculate the Probability distribution PDist_g for each site
# 6.PDist: Calculate the Probability distribution PDist_g for each site
PDist <- function(nmref_sites, mahdist) {
  endGrp_Score <- matrix(0, nrow = nrow(mahdist), ncol = ncol(mahdist))
  for (i in 1:nrow(mahdist)) {
    endGrp_Score[i, ] <- nmref_sites * exp(-mahdist[i, ] / 2)
  }
  return(endGrp_Score)
}

PDist_old <- function(nmref_sites, mahdist) {
  return(nmref_sites * exp(-mahdist / 2))
}

# 7. PDistTotal: Calculate Total probabilities of all sites, bind the row sums to the last column

PDistTotal <- function(distr_g) {
  return(cbind(distr_g / rowSums(distr_g), rowSums(distr_g)))
}

# 8.getSuitabilityCode: Suitability code - input from getMahDist_min, and suitability codes
getSuitabilityCode <- function(minMahDist, suitCodes) {
  suit_frame <- as.character(data.frame(c(), c())) # Note rbind works with character data.frames
  for (i in 1:nrow(minMahDist)) { # Case 1
    if (minMahDist[i, ncol(minMahDist)] < suitCodes[1, "CQ1"]) { # for GB, row = 1
      # print(c("Here in loop case 1, row =",i))
      suit_frame <- rbind(suit_frame, c(1, ">5%"))
    } # endif
    else { # Case 2
      if ((suitCodes[1, "CQ1"] <= minMahDist[i, ncol(minMahDist)]) & (minMahDist[i, ncol(minMahDist)] < suitCodes[1, "CQ2"])) { # for GB, row = 1
        # print(c("Here in loop case 2, row =",i))
        suit_frame <- rbind(suit_frame, c(2, "<5%"))
      } # endif
      else { # Case 3
        if ((suitCodes[1, "CQ2"] <= minMahDist[i, ncol(minMahDist)]) & (minMahDist[i, ncol(minMahDist)] < suitCodes[1, "CQ3"])) { # for GB, row = 1
          suit_frame <- rbind(suit_frame, c(3, "<2%"))
        } # endif
        else { # Case 4
          if ((suitCodes[1, "CQ3"] <= minMahDist[i, ncol(minMahDist)]) & (minMahDist[i, ncol(minMahDist)] < suitCodes[1, "CQ4"])) { # for GB, row = 1
            suit_frame <- rbind(suit_frame, c(4, "<1%"))
          } # endif
          else { # last case - no need for "if"
            if (minMahDist[i, ncol(minMahDist)] >= suitCodes[1, "CQ4"]) {
              suit_frame <- rbind(suit_frame, c(5, "<0.1%"))
            }
          } # else last case
        } # else case 4
      } # else case 3
    } # else case 2
  } # for
  colnames(suit_frame) <- c("SuitCode", "SuitText") # past0 to "log" for name of attribute/parameter
  return(suit_frame) # Return both message and log value
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
  file <- dplyr::select(
    file, .data$`EndGrp`, .data$`SeasonCode`, .data$`Season`,
    .data$`TL2_WHPT_NTAXA_AbW_DistFam`,
    .data$`TL2_WHPT_ASPT_AbW_DistFam`,
    .data$`TL2_WHPT_NTAXA_AbW_CompFam`,
    .data$`TL2_WHPT_ASPT_AbW_CompFam`
  )
  return(file)
}

# csv read version of getEndGroupMeans()

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
  end_group_means <- dplyr::select(
    end_group_means, .data$`EndGrp`, .data$`SeasonCode`, .data$`Season`,
    .data$`TL2_WHPT_NTAXA_AbW_DistFam`, .data$`TL2_WHPT_ASPT_AbW_DistFam`,
    .data$`TL2_WHPT_NTAXA_AbW_CompFam`, .data$`TL2_WHPT_ASPT_AbW_CompFam`
  )
  return(end_group_means)
}


# Uses data.table fread. Used in AZURE and R Studio codes for consistency.
# This particular function is for all 80 indices, and removes unwanted characters in column names

getEndGroupMeans_dtableCopy <- function(filepathname, model) {

  infile <- readCSVFile(filepathname)
  names(infile) <-str_replace_all(names(infile), c( "%" = "perc", "/"="", "&" = "" ))
  #print(colnames(infile))
  infile%>%
    rename(RIVPACSMODEL = `RIVPACS Model`) %>%
    rename(EndGrp = `End Group`) %>%
    rename(SeasonCode = `Season Code`) %>%
    rename(Season = `Season`)%>%
    rename(TL1_BMWP = `TL1 BMWP`)%>%
    rename(TL1_NTAXA = `TL1 NTAXA`)%>%
    rename(TL1_ASPT = `TL1 ASPT`)%>%
    rename(TL2_WHPT_Score_nonAb_DistFam      = `TL2 WHPT Score (nonAb,DistFam)`)%>%
    rename(TL2_WHPT_NTAXA_nonAb_DistFam      = `TL2 WHPT NTAXA (nonAb,DistFam)`)%>%
    rename(TL2_WHPT_ASPT_nonAb_DistFam       = `TL2 WHPT ASPT (nonAb,DistFam)`)%>%
    rename(TL2_WHPT_Score_nonAb_CompFam      = `TL2 WHPT Score (nonAb,CompFam)`)%>%
    rename(TL2_WHPT_NTAXA_nonAb_CompFam      = `TL2 WHPT NTAXA (nonAb,CompFam)`)%>%
    rename(TL2_WHPT_ASPT_nonAb_CompFam       = `TL2 WHPT ASPT (nonAb,CompFam)`)%>%
    rename(TL2_WHPT_Score_AbW_DistFam        = `TL2 WHPT Score (AbW,DistFam)`)%>%
    rename(TL2_WHPT_NTAXA_AbW_DistFam        = `TL2 WHPT NTAXA (AbW,DistFam)`)%>%
    rename(TL2_WHPT_ASPT_AbW_DistFam         = `TL2 WHPT ASPT (AbW,DistFam)`)%>%
    rename(TL2_WHPT_Score_AbW_CompFam        = `TL2 WHPT Score (AbW,CompFam)`)%>%
    rename(TL2_WHPT_NTAXA_AbW_CompFam        = `TL2 WHPT NTAXA (AbW,CompFam)`)%>%
    rename(TL2_WHPT_ASPT_AbW_CompFam         = `TL2 WHPT ASPT (AbW,CompFam)`)%>%
    rename(TL1_AWIC_Fam                      = `TL1 AWIC(Fam)`)%>%
    rename(TL4_AWIC_Sp_Murphy                = `TL4 AWIC(Sp) Murphy`)%>%
    rename(TL5_AWIC_Sp_Murphy                = `TL5 AWIC(Sp) Murphy`)%>%
    rename(TL4_WFD_AWIC_Sp_McFarland         = `TL4 WFD AWIC(Sp) McFarland`)%>%
    rename(TL5_WFD_AWIC_Sp_McFarland         = `TL5 WFD AWIC(Sp) McFarland`)%>%
    rename(TL4_Raddum                        = `TL4 Raddum`)%>%
    rename(TL5_Raddum                        = `TL5 Raddum`)%>%
    rename(TL4_SEPA_per_Acid_Sensitive_Taxa  = `TL4 SEPA perc Acid Sensitive Taxa`)%>%
    rename(TL5_SEPA_perc_Acid_Sensitive_Taxa = `TL5 SEPA perc Acid Sensitive Taxa`)%>%
    rename(TL4_MetTol                        = `TL4 MetTol`)%>%
    rename(TL5_MetTol                        = `TL5 MetTol`)%>%
    rename(TL1_2_LIFE_Fam_CompFam            = `TL12 LIFE(Fam) (CompFam)`)%>%
    rename(TL2_LIFE_Fam_DistFam              = `TL2 LIFE(Fam) (DistFam)`)%>%
    rename(TL3_LIFE_Fam_DistFam              = `TL3 LIFE(Fam) (DistFam)`)%>%
    rename(TL4_LIFE_Sp                       = `TL4 LIFE(Sp)`)%>%
    rename(TL5_LIFE_Sp                       = `TL5 LIFE(Sp)`)%>%
    rename(TL3_PSI_Fam                       = `TL3 PSI(Fam)`)%>%
    rename(TL4_PSI_Sp                        = `TL4 PSI(Sp)`)%>%
    rename(TL5_PSI_Sp                        = `TL5 PSI(Sp)`)%>%
    rename(TL3_E_PSI_fam69                   = `TL3 E-PSI(fam69)`)%>%
    rename(TL4_E_PSI_mixed_level             = `TL4 E-PSI(mixed level)`)%>%
    rename(TL5_E_PSI_mixed_level             = `TL5 E-PSI(mixed level)`)%>%
    rename(TL4_oFSIsp                        = `TL4 oFSIsp` )%>%
    rename(TL5_oFSIsp                        = `TL5 oFSIsp`)%>%
    rename(TL4_ToFSIsp                       = `TL4 ToFSIsp`)%>%
    rename(TL5_ToFSIsp                       = `TL5 ToFSIsp`)%>%
    rename(TL4_CoFSIsp                       = `TL4 CoFSIsp`)%>%
    rename(TL5_CoFSIsp                       = `TL5 CoFSIsp`)%>%
    rename(TL4_GSFI_FI05                     = `TL4 GSFI FI05`)%>%
    rename(TL5_GSFI_FI05                     = `TL5 GSFI FI05`)%>%
    rename(TL4_GSFI_FI09                     = `TL4 GSFI FI09`)%>%
    rename(TL5_GSFI_FI09                     = `TL5 GSFI FI09`)%>%
    rename(TL4_GSFI_FI091                    = `TL4 GSFI FI091`)%>%
    rename(TL5_GSFI_FI091                    = `TL5 GSFI FI091`)%>%
    rename(TL4_GSFI_FI091_K                  = `TL4 GSFI FI091_K`)%>%
    rename(TL5_GSFI_FI091_K                  = `TL5 GSFI FI091_K`)%>%
    rename(TL4_GSFI_FI092                    = `TL4 GSFI FI092`)%>%
    rename(TL5_GSFI_FI092                    = `TL5 GSFI FI092`)%>%
    rename(TL4_GSFI_FI11_12                  = `TL4 GSFI FI11_12`)%>%
    rename(TL5_GSFI_FI11_12                  = `TL5 GSFI FI11_12`)%>%
    rename(TL4_GSFI_FI14_16                  = `TL4 GSFI FI14_16`)%>%
    rename(TL5_GSFI_FI14_16                  = `TL5 GSFI FI14_16`)%>%
    rename(TL4_GSFI_FI15_17                  = `TL4 GSFI FI15_17`)%>%
    rename(TL5_GSFI_FI15_17                  = `TL5 GSFI FI15_17`)%>%
    rename(TL4_GSFI_FI152                    = `TL4 GSFI FI152`)%>%
    rename(TL5_GSFI_FI152                    = `TL5 GSFI FI152`)%>%
    rename(TL2_SPEAR_Fam_perc                = `TL2 SPEAR(Fam) perc`)%>%
    rename(TL4_SPEAR_Sp_perc                 = `TL4 SPEAR(Sp) perc`)%>%
    rename(TL5_SPEAR_Sp_perc                 = `TL5 SPEAR(Sp) perc`)%>%
    rename(SPEAR_pesticides_TL2_fam_Knillmann_2018  = `SPEAR(pesticides) TL2 fam Knillmann 2018`)%>%
    rename(SPEAR_refuge_TL2_fam_Knillmann_2018      = `SPEAR(refuge) TL2 fam Knillmann 2018`)%>%
    rename(SPEAR_pesticides_TL4_sp_Knillmann_2018   = `SPEAR(pesticides) TL4 sp Knillmann 2018`)%>%
    rename(SPEAR_refuge_TL4_sp_Knillmann_2018       = `SPEAR(refuge) TL4 sp Knillmann 2018`)%>%
    rename(SPEAR_pesticides_TL5_sp_Knillmann_2018   = `SPEAR(pesticides) TL5 sp Knillmann 2018`)%>%
    rename(SPEAR_refuge_TL5_sp_Knillmann_2018       = `SPEAR(refuge) TL5 sp Knillmann 2018`)%>%
    rename(TL4_CCI                           = `TL4 CCI`)%>%
    rename(TL5_CCI                           = `TL5 CCI`)%>%
    rename(TL2_08_Group_ARMI_NTaxa           = `TL2 08 Group ARMI NTaxa`)%>%
    rename(TL2_08_Group_ARMI_Score           = `TL2 08 Group ARMI Score`)%>%
    rename(TL2_33_Group_ARMI_NTaxa           = `TL2 33 Group ARMI NTaxa`)%>%
    rename(TL2_33_Group_ARMI_Score           = `TL2 33 Group ARMI Score`)%>%
    rename(TL2_33_Group_Flow_Silt_NTaxa      =`TL2 33 Group Flow  Silt NTaxa`)%>%
    rename(TL2_33_Group_Flow_Silt_Score      = `TL2 33 Group Flow  Silt Score`)%>%
    rename(TL2_14_Group_Urban_NTaxa          = `TL2 14 Group Urban NTaxa`)%>%
    rename(TL2_14_Group_Urban_Score          = `TL2 14 Group Urban Score`)%>%
    filter(RIVPACSMODEL == model) %>%
    # Dont select RIVAPCSMODEL since we know model what we are processing. Remove column 1 = RIVPACSMODEL
    #select(`EndGrp`, `SeasonCode`,`Season`,`TL2_WHPT_NTAXA_AbW_DistFam`,`TL2_WHPT_ASPT_AbW_DistFam`,`TL2_WHPT_NTAXA_AbW_CompFam`,`TL2_WHPT_ASPT_AbW_CompFam`)
    select(-1)
}
# 10.getSeasonIndexScores: Calculate predictions of probability scores for indices WHPT, given season ids,
# whpt values. Use "getProbScores()"
getSeasonIndexScores <- function(data_to_bindTo, season_to_run, index_id, end_group_IndexDFrame, DistNames, all_indices) {
  # Declare a matrix of zeros, with nrow, ncol dimensions
  # index_Score <- matrix(0, nrow=nrow(end_group_IndexDFrame), ncol = nrow(end_group_IndexDFrame))
  # mainDFrame <- data_to_bindTo

  # filter for Spring, SeasonCode==1, if exists
  spring_whpt_ntaxa_Abw_Dist <- NULL
  spring_whpt_aspt_Abw_Dist <- NULL
  spring_whpt_ntaxa_Abw_CompFam <- NULL
  spring_whpt_aspt_Abw_CompFam <- NULL

  end_groups <- end_group_IndexDFrame %>% dplyr::filter(SeasonCode %in% season_to_run)

  if(all_indices == TRUE) {
  end_groups <- as.matrix(dplyr::select(end_groups,-EndGrp, -SeasonCode, -Season))
  end_groups <- end_groups[seq_len(length(DistNames)), ]
  probabilities <- as.matrix(data_to_bindTo[, DistNames])
  all <- as.matrix(probabilities) %*%  as.matrix(end_groups)
  all <- as.data.frame(all)
  all <- cbind(data_to_bindTo, all)
  return(all)
  }
  if (1 %in% end_group_IndexDFrame$SeasonCode) {
    spring_whpt_all <- dplyr::filter(end_group_IndexDFrame, .data$SeasonCode == 1)
    spring_whpt_all <- spring_whpt_all[seq_len(length(DistNames)), ]
    # Check what index iit is you want , and getProbScores
    if ("TL2_WHPT_NTAXA_AbW_DistFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      spring_whpt_ntaxa_Abw_Dist <- as.data.frame(getProbScores(
        Proball = data_to_bindTo[, DistNames],
        IDXMean = dplyr::select(
          spring_whpt_all,
          .data$TL2_WHPT_NTAXA_AbW_DistFam
        )
      ))
      colnames(spring_whpt_ntaxa_Abw_Dist) <- c("TL2_WHPT_NTAXA_AbW_DistFam_spr")
      # print(nrow(spring_whpt_ntaxa_Abw_Dist))
      # print(spring_whpt_ntaxa_Abw_Dist)
    }

    if ("TL2_WHPT_ASPT_AbW_DistFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      spring_whpt_aspt_Abw_Dist <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          spring_whpt_all,
          .data$TL2_WHPT_ASPT_AbW_DistFam
        )
      ))
      colnames(spring_whpt_aspt_Abw_Dist) <- c("TL2_WHPT_ASPT_AbW_DistFam_spr")
      # print(nrow(spring_whpt_aspt_Abw_Dist))
      # print(spring_whpt_aspt_Abw_Dist)
    }

    if ("TL2_WHPT_NTAXA_AbW_CompFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      spring_whpt_ntaxa_Abw_CompFam <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          spring_whpt_all,
          .data$TL2_WHPT_NTAXA_AbW_CompFam
        )
      ))
      colnames(spring_whpt_ntaxa_Abw_CompFam) <- c("TL2_WHPT_NTAXA_AbW_CompFam_spr")
      # print(nrow(spring_whpt_ntaxa_Abw_CompFam))
      # print(spring_whpt_ntaxa_Abw_CompFam)
    }

    if ("TL2_WHPT_ASPT_AbW_CompFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      spring_whpt_aspt_Abw_CompFam <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          spring_whpt_all,
          .data$TL2_WHPT_ASPT_AbW_CompFam
        )
      ))
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

  if (3 %in% end_group_IndexDFrame$SeasonCode) {
    autumn_whpt_all <- dplyr::filter(end_group_IndexDFrame, .data$SeasonCode == 3)
    autumn_whpt_all <- autumn_whpt_all[seq_len(length(DistNames)), ]
    # Check what index iit is you want , and getProbScores
    if ("TL2_WHPT_NTAXA_AbW_DistFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      autumn_whpt_ntaxa_Abw_Dist <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          autumn_whpt_all,
          .data$TL2_WHPT_NTAXA_AbW_DistFam
        )
      ))
      colnames(autumn_whpt_ntaxa_Abw_Dist) <- c("TL2_WHPT_NTAXA_AbW_DistFam_aut")
      # print(nrow(autumn_whpt_ntaxa_Abw_Dist))
      # print(autumn_whpt_ntaxa_Abw_Dist)
    }

    if ("TL2_WHPT_ASPT_AbW_DistFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      autumn_whpt_aspt_Abw_Dist <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          autumn_whpt_all,
          .data$TL2_WHPT_ASPT_AbW_DistFam
        )
      ))
      colnames(autumn_whpt_aspt_Abw_Dist) <- c("TL2_WHPT_ASPT_AbW_DistFam_aut")
      # print(nrow(autumn_whpt_aspt_Abw_Dist))
      # print(autumn_whpt_aspt_Abw_Dist)
    }

    if ("TL2_WHPT_NTAXA_AbW_CompFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      autumn_whpt_ntaxa_Abw_CompFam <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          autumn_whpt_all,
          .data$TL2_WHPT_NTAXA_AbW_CompFam
        )
      ))
      colnames(autumn_whpt_ntaxa_Abw_CompFam) <- c("TL2_WHPT_NTAXA_AbW_CompFam_aut")
      # print(nrow(autumn_whpt_ntaxa_Abw_CompFam))
      # print(autumn_whpt_ntaxa_Abw_CompFam)
    }

    if ("TL2_WHPT_ASPT_AbW_CompFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      autumn_whpt_aspt_Abw_CompFam <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          autumn_whpt_all,
          .data$TL2_WHPT_ASPT_AbW_CompFam
        )
      ))
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
    summer_whpt_all <- summer_whpt_all[seq_len(length(DistNames)), ]
    # Check what index iit is you want , and getProbScores
    if ("TL2_WHPT_NTAXA_AbW_DistFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      summer_whpt_ntaxa_Abw_Dist <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          summer_whpt_all,
          .data$TL2_WHPT_NTAXA_AbW_DistFam
        )
      ))
      colnames(summer_whpt_ntaxa_Abw_Dist) <- c("TL2_WHPT_NTAXA_AbW_DistFam_sum")
      # print(nrow(autumn_whpt_ntaxa_Abw_Dist))
      # print(autumn_whpt_ntaxa_Abw_Dist)
    }

    if ("TL2_WHPT_ASPT_AbW_DistFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      summer_whpt_aspt_Abw_Dist <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          summer_whpt_all,
          .data$TL2_WHPT_ASPT_AbW_DistFam
        )
      ))
      colnames(summer_whpt_aspt_Abw_Dist) <- c("TL2_WHPT_ASPT_AbW_DistFam_sum")
      # print(nrow(autumn_whpt_aspt_Abw_Dist))
      # print(autumn_whpt_aspt_Abw_Dist)
    }

    if ("TL2_WHPT_NTAXA_AbW_CompFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      summer_whpt_ntaxa_Abw_CompFam <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          summer_whpt_all,
          .data$TL2_WHPT_NTAXA_AbW_CompFam
        )
      ))
      colnames(summer_whpt_ntaxa_Abw_CompFam) <- c("TL2_WHPT_NTAXA_AbW_CompFam_sum")
      # print(nrow(autumn_whpt_ntaxa_Abw_CompFam))
      # print(autumn_whpt_ntaxa_Abw_CompFam)
    }

    if ("TL2_WHPT_ASPT_AbW_CompFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      summer_whpt_aspt_Abw_CompFam <- as.data.frame(getProbScores(
        data_to_bindTo[, DistNames],
        dplyr::select(
          summer_whpt_all,
          .data$TL2_WHPT_ASPT_AbW_CompFam
        )
      ))
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
  if (!is.null(summer_whpt_ntaxa_Abw_Dist)) {
    bind_all <- cbind(bind_all, summer_whpt_ntaxa_Abw_Dist)
    bind_all <- cbind(bind_all, summer_whpt_aspt_Abw_Dist)
    # Summer CompFam
    bind_all <- cbind(bind_all, summer_whpt_ntaxa_Abw_CompFam)
    bind_all <- cbind(bind_all, summer_whpt_aspt_Abw_CompFam)
  }
  return(bind_all)
}

getSeasonIndexScores_new <- function (data_to_bindTo, season_to_run, index_id, end_group_IndexDFrame, model){
  #index_Score <- matrix(0, nrow=nrow(end_group_IndexDFrame), ncol = nrow(end_group_IndexDFrame) ) # Declare a matrix of zeros, with nrow, ncol dimensions

  # NOTE: GB model takes c(15:57), while NI model takes c(13:23)
  probScores <- if (model=="RIVPACS IV GB") c(15:57) else { c(13:23) } # GB, NI model

  #print( end_group_IndexDFrame$SeasonCode)

  #filter for Spring, SeasonCode==1, if exists
  spring_whpt_ntaxa_Abw_Dist    <- NULL
  spring_whpt_aspt_Abw_Dist     <- NULL
  spring_whpt_ntaxa_Abw_CompFam <- NULL
  spring_whpt_aspt_Abw_CompFam  <- NULL

  # Declare score variales for all seasons
  spring_allIndices_SCores <- NULL
  summer_allIndices_SCores <- NULL
  autumn_allIndices_SCores <- NULL

  # Bind these to input dataframe and output them
  bind_all <- NULL

  if(1 %in% end_group_IndexDFrame$SeasonCode ) {
    print("Processing Spring")

    # Check index, and getProbScores
    # Filter season and index together for all the indices
    spr_all <- select (filter(end_group_IndexDFrame,SeasonCode==1),everything())[,c(-1,-2, -3)] # All indices, removes " EndGrp, SeasonCode", "Season"
    spring_allIndices_Scores <- as.data.frame(getProbScores(data_to_bindTo[,probScores], select(spr_all, everything()) )) #LEave Season when multiplying out
    spr <- data.frame(Season = rep("Spring",nrow(spring_allIndices_Scores)))
    #print("bind_all")
    bind_all <- rbind(bind_all,cbind(spr,spring_allIndices_Scores))
    #print(bind_all)
  }

  #filter for Summer, SeasonCode==2, if exists
  summer_whpt_ntaxa_Abw_Dist      <- NULL
  summer_whpt_aspt_Abw_Dist       <- NULL
  summer_whpt_ntaxa_Abw_CompFam   <- NULL
  summer_whpt_aspt_Abw_CompFam    <- NULL

  if(2 %in% end_group_IndexDFrame$SeasonCode ) {
    summer_whpt_all <- filter(end_group_IndexDFrame,SeasonCode==2)
    #print (summer_whpt_all)
    print("Processing summer")

    # Filter season and index together for all the indices
    sum_all <- select (filter(end_group_IndexDFrame,SeasonCode==2),everything())[,c(-1,-2, -3)] # all indices
    summer_allIndices_Scores <- as.data.frame(getProbScores(data_to_bindTo[,probScores], select(sum_all, everything())))
    summ <- data.frame(Season = rep("Summer",nrow(summer_allIndices_Scores)))
    #print(summer_allIndices_Scores)
    bind_all <- rbind(bind_all, cbind(summ,summer_allIndices_Scores)) # Remove row 1
    #print(bind_all)
  }

  #filter for autumn, SeasonCode==3, if exists
  autumn_whpt_ntaxa_Abw_Dist    <- NULL
  autumn_whpt_aspt_Abw_Dist     <- NULL
  autumn_whpt_ntaxa_Abw_CompFam <- NULL
  autumn_whpt_aspt_Abw_CompFam  <- NULL

  if(3 %in% end_group_IndexDFrame$SeasonCode ) {
    print("Processing autumn")
    # Filter season and index together for all the indices
    aut_all <- select (filter(end_group_IndexDFrame,SeasonCode==3),everything())[,c(-1,-2, -3)] # all indices
    autumn_allIndices_Scores <- as.data.frame(getProbScores(data_to_bindTo[,probScores], select(aut_all, everything())))
    aut <- data.frame(Season = rep("Autumn",nrow(autumn_allIndices_Scores)))
    print("Autum scores -->")
    #print(autumn_allIndices_Scores)
    bind_all <- rbind(bind_all,cbind(aut,autumn_allIndices_Scores)) # remove row 1
  }

  #Add more seasons 4,5,6,7 etc

  #Add it to main Data input
  bind_all <- cbind(data_to_bindTo, bind_all)

  return(bind_all)

}# new

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

## -------------------------------------------- old functions --------------

# Calculate the function scores, DFScore
getDFScore_old <- function(DFCoeff, EnvValues) {
  DFScore_d <- data.frame(matrix(0, nrow = nrow(EnvValues)))
  # print( c("outloop, ",nrow(EnvValues)))
  for (i in 1:nrow(EnvValues)) {
    # print(c("Dcoeff= ",as.numeric(DFCoeff[,-1][,1])))
    # print(c("Env = ",EnvValues[i,-1]))
    DFScore_d[i] <- (sum(as.numeric(DFCoeff[, -1][, 1]) * EnvValues[i, -1])) # I thin use just one column , column==1, of ceofficients for all Env variables
    # print(c(" in loop, DFScore = ", DFScore_d[i]))
  }
  # Use only numeric  return of rows equivalent to number of instances
  DFScores <- as.numeric(DFScore_d[1, ])
  DFScores <- as.data.frame(DFScores)
  return(DFScores) # gives mutlipel values, only get row one, not nrows
} # Done, cbind this to original dataset

# Calculate Probabilities of Endgroup
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

getMahDist_min_old <- function(DFscore, meanvalues) {
  mah_Score <- matrix(0, nrow = nrow(DFscore), ncol = nrow(meanvalues))
  for (row_dfscore in 1:nrow(DFscore)) {
    for (row_means in 1:nrow(meanvalues)) {
      mah_Score[row_dfscore, row_means] <- min((DFscore[row_dfscore, ] - meanvalues[row_means, ])^2) # apply rowSums() or sum()
    }
  }
  return(mah_Score)
}

# Calculate Mahalabois distance
# getMahDist_old <- function (meansA, valuesB) {
#   l_mah_dist <- 0
#   for (i in 1: ncol(meansA)) {
#     l_mah_dist <- l_mah_dist + (valuesB - meansA)^2;
#     l_pDist    <- l_NRef * exp( (-1*l_mah_dist) /2);
#   }
#   return (l_mah_dist)
# }

# End Exclude Linting
