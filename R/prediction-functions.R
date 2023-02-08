# Prediction helper functions

# getDFScores: Make sure the dimensions DFcoeff (m x n) maps to dimensions of
# EnvValues (n x k), result will be (m x k). Convert DFCoeff and EnvValues to
# matrix, finalCopy <- as.matrix(final.predictors[,-c(1)]), removing first and
# last column Similarly newDFcoeff <- as.matrix(DFCoeff_gb685[,-1])
# Return finalCopy %*% newDFcoeff
getDFScores <- function(EnvValues, DFCoeff) {
  Env_i <- as.matrix(EnvValues[, -c(1)])
  Coeff_j <- as.matrix(DFCoeff[, -1])
  return(Env_i %*% Coeff_j)
}

# getMahdist: Calculate the Mahanalobis distance of point x from site g
getMahDist <- function(DFscore, meanvalues) {
  meanvalues <- as.matrix(meanvalues)
  DFscore <- as.matrix(DFscore)
  mah_Score <- matrix(0, nrow = nrow(DFscore), ncol = nrow(meanvalues))
  # Declare a matrix of zeros, with nrow, ncol dimensions
  for (row_dfscore in seq_len(nrow(DFscore))) {
    for (row_means in seq_len(nrow(meanvalues))) {
      mah_Score[row_dfscore, row_means] <- sum((DFscore[row_dfscore, ] - meanvalues[row_means, ])^2)
      # apply rowSums() or sum()
    }
  }
  return(mah_Score)
}

# getMahDist_min: Calculate the minimum Mahanalobis distance of point x from site g
getMahDist_min <- function(DFscore, meanvalues) {
  mahdist_min <- getMahDist(DFscore, meanvalues)
  toappend <- data.frame(min = c())
  for (i in seq_len(nrow(mahdist_min))) {
    toappend <- rbind(toappend, min(mahdist_min[i, ]))
  }
  # Bind the result to the last column of input file "mahdist_min". No need to
  # change "toappend1 to character as all are to be numeric
  names(toappend) <- c("minMah")
  return(cbind(mahdist_min, toappend))
}

# getProbScores: Multiply end-group probabilities with IDXmean, Taxapr, Taxaab,
# Similar to DFScores() - combine them
getProbScores <- function(Proball, IDXMean) {
  Env_i <- as.matrix(Proball)
  Coeff_j <- as.matrix(IDXMean)
  return(Env_i %*% Coeff_j)
}

# PDist: Calculate the Probability distribution PDist_g for each site
PDist <- function(nmref_sites, mahdist) {
  endGrp_Score <- matrix(0, nrow = nrow(mahdist), ncol = ncol(mahdist))
  for (i in seq_len(nrow(mahdist))) {
    endGrp_Score[i, ] <- nmref_sites * exp(-mahdist[i, ] / 2)
  }
  return(endGrp_Score)
}

# PDistTotal: Calculate Total probabilities of all sites, bind the row sums to
# the last column
PDistTotal <- function(distr_g) {
  return(cbind(distr_g / rowSums(distr_g), rowSums(distr_g)))
}

# getSuitabilityCode: Suitability code - input from getMahDist_min, and
# suitability codes
getSuitabilityCode <- function(minMahDist, suitCodes, area, model) {
  suitCodes <- suitCodes[suitCodes$area == area, ]
  suitCodes <- suitCodes[suitCodes$model == model, ]
  # Note rbind works with character data.frames
  suit_frame <- as.character(data.frame(c(), c()))
  for (i in seq_len(nrow(minMahDist))) {
    if (minMahDist[i, ncol(minMahDist)] < suitCodes[, "CQ1"]) {
      suit_frame <- rbind(suit_frame, c(1, ">5%"))
    } else if((suitCodes[, "CQ1"] <= minMahDist[i, ncol(minMahDist)]) &
        (minMahDist[i, ncol(minMahDist)] < suitCodes[, "CQ2"])) {
        suit_frame <- rbind(suit_frame, c(2, "<5%"))
      } else if((suitCodes[, "CQ2"] <= minMahDist[i, ncol(minMahDist)]) &
          (minMahDist[i, ncol(minMahDist)] < suitCodes[, "CQ3"])) {
          suit_frame <- rbind(suit_frame, c(3, "<2%"))
        } else if((suitCodes[, "CQ3"] <= minMahDist[i, ncol(minMahDist)]) &
            (minMahDist[i, ncol(minMahDist)] < suitCodes[, "CQ4"])) {
            suit_frame <- rbind(suit_frame, c(4, "<1%"))
          } else { # last case - no need for "if"
              suit_frame <- rbind(suit_frame, c(5, "<0.1%"))
          }
  }
  # past0 to "log" for name of attribute/parameter
  colnames(suit_frame) <- c("SuitCode", "SuitText")
  return(suit_frame) # Return both message and log value
}

# This particular function is for all 80 indices, and removes unwanted
# characters in column names
rename_end_group_means <- function(data) {
  names(data) <- gsub("%", "perc", names(data))
  names(data) <- gsub("/", "", names(data))
  names(data) <- gsub("&", "", names(data))

  data <-
    data.frame(
      "RIVPACS_Model" = data$`RIVPACS Model`,
      "End_Group" = data$`End Group`,
      "Season_Code" = data$`Season Code`,
      "Season" = data$`Season`,
      "TL1_BMWP" = data$`TL1 BMWP`,
      "TL1_NTAXA" = data$`TL1 NTAXA`,
      "TL1_ASPT" = data$`TL1 ASPT`,
      "TL2_WHPT_Score_nonAb_DistFam" = data$`TL2 WHPT Score (nonAb,DistFam)`,
      "TL2_WHPT_NTAXA_nonAb_DistFam" = data$`TL2 WHPT NTAXA (nonAb,DistFam)`,
      "TL2_WHPT_ASPT_nonAb_DistFam" = data$`TL2 WHPT ASPT (nonAb,DistFam)`,
      "TL2_WHPT_Score_nonAb_CompFam" = data$`TL2 WHPT Score (nonAb,CompFam)`,
      "TL2_WHPT_NTAXA_nonAb_CompFam" = data$`TL2 WHPT NTAXA (nonAb,CompFam)`,
      "TL2_WHPT_ASPT_nonAb_CompFam" = data$`TL2 WHPT ASPT (nonAb,CompFam)`,
      "TL2_WHPT_Score_AbW_DistFam" = data$`TL2 WHPT Score (AbW,DistFam)`,
      "TL2_WHPT_NTAXA_AbW_DistFam" = data$`TL2 WHPT NTAXA (AbW,DistFam)`,
      "TL2_WHPT_ASPT_AbW_DistFam" = data$`TL2 WHPT ASPT (AbW,DistFam)`,
      "TL2_WHPT_Score_AbW_CompFam" = data$`TL2 WHPT Score (AbW,CompFam)`,
      "TL2_WHPT_NTAXA_AbW_CompFam" = data$`TL2 WHPT NTAXA (AbW,CompFam)`,
      "TL2_WHPT_ASPT_AbW_CompFam" = data$`TL2 WHPT ASPT (AbW,CompFam)`,
      "TL1_AWIC_Fam" = data$`TL1 AWIC(Fam)`,
      "TL4_AWIC_Sp_Murphy" = data$`TL4 AWIC(Sp) Murphy`,
      "TL5_AWIC_Sp_Murphy" = data$`TL5 AWIC(Sp) Murphy`,
      "TL4_WFD_AWIC_Sp_McFarland" = data$`TL4 WFD AWIC(Sp) McFarland`,
      "TL5_WFD_AWIC_Sp_McFarland" = data$`TL5 WFD AWIC(Sp) McFarland`,
      "TL4_Raddum" = data$`TL4 Raddum`,
      "TL5_Raddum" = data$`TL5 Raddum`,
      "TL4_SEPA_per_Acid_Sensitive_Taxa" = data$`TL4 SEPA perc Acid Sensitive Taxa`,
      "TL5_SEPA_perc_Acid_Sensitive_Taxa" = data$`TL5 SEPA perc Acid Sensitive Taxa`,
      "TL4_MetTol" = data$`TL4 MetTol`,
      "TL5_MetTol" = data$`TL5 MetTol`,
      "TL1_2_LIFE_Fam_CompFam" = data$`TL12 LIFE(Fam) (CompFam)`,
      "TL2_LIFE_Fam_DistFam" = data$`TL2 LIFE(Fam) (DistFam)`,
      "TL3_LIFE_Fam_DistFam" = data$`TL3 LIFE(Fam) (DistFam)`,
      "TL4_LIFE_Sp" = data$`TL4 LIFE(Sp)`,
      "TL5_LIFE_Sp" = data$`TL5 LIFE(Sp)`,
      "TL3_PSI_Fam" = data$`TL3 PSI(Fam)`,
      "TL4_PSI_Sp" = data$`TL4 PSI(Sp)`,
      "TL5_PSI_Sp" = data$`TL5 PSI(Sp)`,
      "TL3_E_PSI_fam69" = data$`TL3 E-PSI(fam69)`,
      "TL4_E_PSI_mixed_level" = data$`TL4 E-PSI(mixed level)`,
      "TL5_E_PSI_mixed_level" = data$`TL5 E-PSI(mixed level)`,
      "TL4_oFSIsp" = data$`TL4 oFSIsp`,
      "TL5_oFSIsp" = data$`TL5 oFSIsp`,
      "TL4_ToFSIsp" = data$`TL4 ToFSIsp`,
      "TL5_ToFSIsp" = data$`TL5 ToFSIsp`,
      "TL4_CoFSIsp" = data$`TL4 CoFSIsp`,
      "TL5_CoFSIsp" = data$`TL5 CoFSIsp`,
      "TL4_GSFI_FI05" = data$`TL4 GSFI FI05`,
      "TL5_GSFI_FI05" = data$`TL5 GSFI FI05`,
      "TL4_GSFI_FI09" = data$`TL4 GSFI FI09`,
      "TL5_GSFI_FI09" = data$`TL5 GSFI FI09`,
      "TL4_GSFI_FI091" = data$`TL4 GSFI FI091`,
      "TL5_GSFI_FI091" = data$`TL5 GSFI FI091`,
      "TL4_GSFI_FI091_K" = data$`TL4 GSFI FI091_K`,
      "TL5_GSFI_FI091_K" = data$`TL5 GSFI FI091_K`,
      "TL4_GSFI_FI092" = data$`TL4 GSFI FI092`,
      "TL5_GSFI_FI092" = data$`TL5 GSFI FI092`,
      "TL4_GSFI_FI11_12" = data$`TL4 GSFI FI11_12`,
      "TL5_GSFI_FI11_12" = data$`TL5 GSFI FI11_12`,
      "TL4_GSFI_FI14_16" = data$`TL4 GSFI FI14_16`,
      "TL5_GSFI_FI14_16" = data$`TL5 GSFI FI14_16`,
      "TL4_GSFI_FI15_17" = data$`TL4 GSFI FI15_17`,
      "TL5_GSFI_FI15_17" = data$`TL5 GSFI FI15_17`,
      "TL4_GSFI_FI152" = data$`TL4 GSFI FI152`,
      "TL5_GSFI_FI152" = data$`TL5 GSFI FI152`,
      "TL2_SPEAR_Fam_perc" = data$`TL2 SPEAR(Fam) perc`,
      "TL4_SPEAR_Sp_perc" = data$`TL4 SPEAR(Sp) perc`,
      "TL5_SPEAR_Sp_perc" = data$`TL5 SPEAR(Sp) perc`,
      "SPEAR_pesticides_TL2_fam_Knillmann_2018" = data$`SPEAR(pesticides) TL2 fam Knillmann 2018`,
      "SPEAR_refuge_TL2_fam_Knillmann_2018" = data$`SPEAR(refuge) TL2 fam Knillmann 2018`,
      "SPEAR_pesticides_TL4_sp_Knillmann_2018" = data$`SPEAR(pesticides) TL4 sp Knillmann 2018`,
      "SPEAR_refuge_TL4_sp_Knillmann_2018" = data$`SPEAR(refuge) TL4 sp Knillmann 2018`,
      "SPEAR_pesticides_TL5_sp_Knillmann_2018" = data$`SPEAR(pesticides) TL5 sp Knillmann 2018`,
      "SPEAR_refuge_TL5_sp_Knillmann_2018" = data$`SPEAR(refuge) TL5 sp Knillmann 2018`,
      "TL4_CCI" = data$`TL4 CCI`,
      "TL5_CCI" = data$`TL5 CCI`,
      "TL2_08_Group_ARMI_NTaxa" = data$`TL2 08 Group ARMI NTaxa`,
      "TL2_08_Group_ARMI_Score" = data$`TL2 08 Group ARMI Score`,
      "TL2_33_Group_ARMI_NTaxa" = data$`TL2 33 Group ARMI NTaxa`,
      "TL2_33_Group_ARMI_Score" = data$`TL2 33 Group ARMI Score`,
      "TL2_33_Group_Flow_Silt_NTaxa" = data$`TL2 33 Group Flow  Silt NTaxa`,
      "TL2_33_Group_Flow_Silt_Score" = data$`TL2 33 Group Flow  Silt Score`,
      "TL2_14_Group_Urban_NTaxa" = data$`TL2 14 Group Urban NTaxa`,
      "TL2_14_Group_Urban_Score" = data$`TL2 14 Group Urban Score`
    )
}

# EndGrpProb_Replacemen): - function inputs x endgroups, any of 1-43, and
# appends a 'p' to make p1,p2,..,p43. It then selects the probabilities pi =
# p1,p2,p3, .. from any SITE i of the predicted values y[i,pi] =
# final.predictionsxxx dataframe[i,pi] usage: site1 <- EndGrpProb_Replacement
# (a$EndGrp_Probs,final.predictors_try2, 1) # for site k=1
EndGrpProb_Replacement <- function(x, final_data, k) {
  allprobss <- sapply(x, function(x) ifelse(x == x, paste0("p", x), 0)) # apply to all values in EndGroup
  # allprobss <- noquote(allprobss)
  c <- as.double(unname(final_data[k, c(allprobss)]))
  return(c) # How do you select with duplicates included???
}

# groupSitesFunction: Calculates endgroup probabilities with index values per
# group of similar site, taxa level, season code and furse_Code returns a site
# with calculated site index, and binds to the whole site in main data frame
# when called.
groupSitesFunction <- function(allSites, k, siteindex, b1) {
  perGroupSite <- b1[allSites[k, 1:5]$Season_Code == b1$Season_Code &
    allSites[k, 1:5]$TL == b1$TL &
    allSites[k, 1:5]$Furse_Code == b1$Furse_Code, ]
  a <- data.frame(t(colSums(perGroupSite$mlist_endGrps[[1]] * perGroupSite[, 11:18])))
  siteName <- paste0("TST-", paste0(siteindex, "-R"))
  siteX <- cbind(data.frame(siteName, perGroupSite[1, 1:10]), a) # Add this site to AllSites - for final output
  return(siteX)
}

# getSeasonIndexScores: Calculate predictions of probability scores for indices
# WHPT, given season ids, whpt values. Use "getProbScores()"
#' @importFrom rlang .data
getSeasonIndexScores <- function(data_to_bindTo, season_to_run, index_id,
                                 end_group_IndexDFrame, DistNames, all_indices) {
  # variables for storing Spring results
  spring_whpt_ntaxa_Abw_Dist <- NULL
  spring_whpt_aspt_Abw_Dist <- NULL
  spring_whpt_ntaxa_Abw_CompFam <- NULL
  spring_whpt_aspt_Abw_CompFam <- NULL

  end_groups <- end_group_IndexDFrame %>% dplyr::filter(SeasonCode %in% season_to_run)

  if (all_indices == TRUE) {
    predictions <- purrr::map_df(c(1, 2, 3), function(season) {
      end_groups <- end_groups[end_groups$SeasonCode == season, ]
      end_groups <- as.matrix(dplyr::select(end_groups,
                                            -.data$EndGrp,
                                            -.data$SeasonCode,
                                            -.data$Season))
      end_groups <- end_groups[seq_len(length(DistNames)), ]
      probabilities <- as.matrix(data_to_bindTo[, DistNames])
      all <- as.matrix(probabilities) %*% as.matrix(end_groups)
      all <- as.data.frame(all)
      all$SEASON <- season
      all <- cbind(data_to_bindTo, all)
    })
    return(predictions)
  }
  # Filter for Spring, SeasonCode==1, if exists
  if (1 %in% end_group_IndexDFrame$SeasonCode) {
    spring_whpt_all <- dplyr::filter(end_group_IndexDFrame, .data$SeasonCode == 1)
    spring_whpt_all <- spring_whpt_all[seq_len(length(DistNames)), ]
    # Check what index it is you want, and getProbScores
    if ("TL2_WHPT_NTAXA_AbW_DistFam" %in% colnames(end_group_IndexDFrame)) { # column exists
      spring_whpt_ntaxa_Abw_Dist <- as.data.frame(getProbScores(
        Proball = data_to_bindTo[, DistNames],
        IDXMean = dplyr::select(
          spring_whpt_all,
          .data$TL2_WHPT_NTAXA_AbW_DistFam
        )
      ))
      colnames(spring_whpt_ntaxa_Abw_Dist) <- c("TL2_WHPT_NTAXA_AbW_DistFam_spr")
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
