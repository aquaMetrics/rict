
prediction <- function(data) {
  data <- utils::read.csv(system.file("extdat",
                                      "test-data.csv",
                                      package = "rict"))

  # calc temp, lat, lon, logalt, logdist, logwith, logalk, log slope, mean_substrate - currently done in validation?
  data$temperature_range <- 15
  data$temperature_mean <- 10
  data$log_altitude <- log10(data$altitude)
  data$log_dist_from_source <- log10(data$dist_from_source)
  data$log_discharge <- log10(data$discharge)
  data$log_width <- log10(data$width)
  data$log_depth <- log10(data$depth)
  data$log_alkalinity <- log10(data$alkalinity)
  data$log_slope <- log10(data$slope)
  data$lat <- rict::osg_parse(grid_refs = paste0(data$ngr, data$northing, data$easting))[[1]]
  data$long <- rict::osg_parse(grid_refs = paste0(data$ngr, data$northing, data$easting))[[2]]
  data$mean_substrate <- rowMeans(data[, c('boulders_cobbles', 'silt_clay', 'pebbles_gravel')], na.rm = T)

  # These should be binary data files?
  mean_end_group <-
    utils::read.delim(
      system.file("extdat", "df-mean-gb-685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  coefficents <-
    utils::read.delim(
      system.file("extdat", "df-coeff-gb-685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  coefficents$predictors <- c("lat",
                              "long",
                              "log_altitude",
                              "log_dist_from_source",
                              "log_width",
                              "log_depth",
                              "mean_substrate",
                              "log_discharge",
                              "alkalinity",
                              "log_alkalinity",
                              "log_slope",
                              "temperature_mean",
                              "temperature_range")
  coefficents$V1 <- NULL
  # save coefficents as binary

  # remove observed values - only need predictions for unique predictors (not unique determinands)
  data <- select(data, -c(value, determinand)) %>%  unique()

  # the names in the coefficients data to select the predictors
  # non_predictors <- names(data[, ! names(data) %in% coefficents$predictors])
  data <- nest(data, predictors = c(coefficents$predictors))

  # Multiple predictors and coefficients
  data$df_scores <- map(data$predictors, function(predictors) {
    df_score <- data.frame(as.matrix(predictors) %*% as.matrix(select(coefficents, -predictors)))
  })

  # Calculate the Mahanalobis distance from each site to reference groups means
  data$mah_dist <- map(data$df_scores, function(df_score){
    mah_dist <- map_df(1:nrow(mean_end_group), function(x){
      data.frame(value = sum((df_score - mean_end_group[x, ])^2))
    })
    mah_dist <- as.data.frame(t(mah_dist))
  })

  # Find which reference group is has shortest Mahanalobis distance from each site
  data$min_mah_dist <-  map(data$mah_dist, `min`)

  # Calculate the probability distributions
  # nr_efg_groups should be binary file?
  nr_efg_groups <- utils::read.csv(system.file("extdat", "end-grp-assess-scores.csv", package = "rict"))
  nr_efg_groups <- nr_efg_groups %>% select(-EndGrpNo)

  data$prob_distribution <- map(data$mah_dist, function(mah_dist) {
      endGrp_Score <- as.matrix(rowSums(nr_efg_groups)) %*% as.matrix(exp(-mah_dist / 2))
  })

  data$prob_distribution_total <- map(data$prob_distribution, function(prob_distribution) {
    return(cbind(prob_distribution / rowSums(prob_distribution), rowSums(prob_distribution)))
  })

 # Suitability code
 data$suit_codes  <- map(data$min_mah_dist, function(min_mah_dist) {
   chi_square_vals <- data.frame(
     CQ1 = c(21.02606, 18.30700),
     CQ2 = c(24.05393, 21.16080),
     CQ3 = c(26.21696, 23.20930),
     CQ4 = c(32.90923, 29.58830)
   )
   chi_square_vals <- chi_square_vals[1, ]
      if (min_mah_dist < chi_square_vals[, "CQ1"]) {
        suit_frame <- c(1, ">5%")
      } else if (chi_square_vals[, "CQ1"] <= min_mah_dist &
            chi_square_vals[, "CQ2"] > min_mah_dist) {
          suit_frame <- c(2, "<5%")
        } else if ((chi_square_vals[, "CQ2"] <= min_mah_dist) &
              (min_mah_dist < chi_square_vals[, "CQ3"])) {
            suit_frame <- c(3, "<2%")
          } else if (chi_square_vals[, "CQ3"] <= min_mah_dist &
                min_mah_dist < chi_square_vals[, "CQ4"]) {
              suit_frame <- c(4,"<1%")
            } else  {
                suit_frame <- c(5, "<0.1%")
            }
    return(data.frame(suit_code = suit_frame[1], suit_text = suit_frame[2]))
  })

 # Calculate predicted scores
 getSeasonIndexScores <- map(data$prob_distribution_total) {

   end_group_index <- utils::read.csv(system.file("extdat", "x-103-end-group-means.csv", package = "rict"))


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


 test <- data %>% select(-prob_distribution,-prob_distribution_total) %>% unnest()
 data <-  unique(data)
}



