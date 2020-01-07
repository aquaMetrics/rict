singleYearClassification <- function(predictions) {

  # set global random seed for rnorm functions etc
  set.seed(1234)
  # Part 1: This Script reads all prediction indices for classification
  gb685_assess_score <- utils::read.csv(system.file("extdat", "end-grp-assess-scores.csv", package = "rict"))
  adjusted_params <- utils::read.csv(system.file("extdat", "adjust-params-ntaxa-aspt.csv", package = "rict"))

  # Enter source files
  # Use the column header as site names in the final output
  all_sites <- predictions[, 1]
  # Keep YEAR, WATERBODY
  year_waterBody <- predictions[, c("YEAR", "WATERBODY")]

  # Combine all_sites with more information  - e.g. YEAR, WATERBODY
  all_sites <- cbind(all_sites, year_waterBody)

  # predictions <- predictions[,-1]
  # Create a routine that checks column names and puts them in the "predictions" dataframe
  # predictions <- predictions[,-c(1,3:15)]
  # Change all names to upper case
  names(predictions) <- toupper(names(predictions))

  # Remove the "_CompFarm_" columns
  predictions$`_COMPFAM_` <- NULL
  # Get the biological data TL2_WHPT_NTAXA_AbW_DistFam_spr
  names_biological <- c(
    "SPR_SEASON_ID", "SPR_TL2_WHPT_ASPT (ABW,DISTFAM)",
    "SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SPR_NTAXA_BIAS",
    "SUM_SEASON_ID", "SUM_TL2_WHPT_ASPT (ABW,DISTFAM)",
    "SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SUM_NTAXA_BIAS",
    "AUT_SEASON_ID", "AUT_TL2_WHPT_ASPT (ABW,DISTFAM)",
    "AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)", "AUT_NTAXA_BIAS"
  )

  biological_data <- predictions[, names_biological]
  # Remove biological_data from predictions
  predictions <- predictions[, !names(predictions) %in% names_biological]

  # Store all_probabilities in one dataframe. Use p1,p2,... etc in case data column positions change in future
  prob_names <- paste0("p", 1:43)
  all_probabilities <- predictions[, toupper(prob_names)] # Needs to change when not uppercase
  # Input Adjustment factors for reference site quality scores (Q1, Q2, Q3, Q4, Q5)

  # Extract Ubias8 from Biological data #
  ubias_main <- biological_data[, "SPR_NTAXA_BIAS"][1] # Put new AZURE

  # OBSERVED ASPT
  # observed_aspt <- read.csv("src/observed_aspt.csv")
  # obs_aspt_spr <- observed_aspt[,1]
  obs_aspt_spr <- biological_data[, "SPR_TL2_WHPT_ASPT (ABW,DISTFAM)"]
  # obs_aspt_aut <- observed_aspt[,2]
  obs_aspt_aut <- biological_data[, "AUT_TL2_WHPT_ASPT (ABW,DISTFAM)"]

  # OBSERVED NTAXA
  # obs_ntaxa_spr <- observed_ntaxa[,1]
  obs_ntaxa_spr <- biological_data[, "SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)"]
  # Obs_ntaxa_aut <- observed_ntaxa[,2]
  Obs_ntaxa_aut <- biological_data[, "AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)"] # change AZURE

  # Input Multiplicative Adjustment factors adjusted_params, 1,..,5)
  adjusted_params <- as.matrix(adjusted_params)
  qij <- computeScoreProportions(gb685_assess_score[, -1]) # Remove the first Column

  # Part 2:  Calculate AdjustedExpected from all probabilities, WE4.5 of WFD72C
  # Compute rj = sum(Pi*qij)
  rj <- as.matrix(getWeighted_proportion_Rj(all_probabilities, qij)) # We should have five of these

  # Multiply rj by adjusted_params, note each row of adjusted_params is for NTAXA, ASPT, so transpose to multiply by rj
  rjaj <- compute_RjAj(rj, adjusted_params)
  # one_over_rjaj <- 1 / rjaj

  # Write a function that computes aspt, ntaxa adjusted (1 = "NTAXA", 2="ASPT")
  # or select them by name as declared in the classification functions
  ntaxa_adjusted <- dplyr::select(predictions, dplyr::contains("_NTAXA_")) / rjaj[, "NTAXA"]
  # Compute AdjExpected as E=predictions/Sum(rj*adjusted_params)
  aspt_adjusted <- dplyr::select(predictions, dplyr::contains("_ASPT_")) / rjaj[, "ASPT"]

  adjusted_expected <- cbind(ntaxa_adjusted, aspt_adjusted)
  adjusted_expected_new <- cbind(as.data.frame(all_sites), adjusted_expected) # Include site names from predictions

  # Part 3:  Calculation of Exp_ref from "AdjustedExpected_new" values, divide
  # by K ( = 1.0049 for NTAXA,  = 0.9921 for ASPT)
  n_runs <- 10000
  # ******* FOR ASPT ************
  Exp_ref_aspt <- aspt_adjusted / 0.9921
  Ubias8 <- ubias_main

  # find the non-bias corrected  EQR = obs/ExpRef
  nonBiasCorrected_WHPT_aspt_spr <- obs_aspt_spr / dplyr::select(Exp_ref_aspt, dplyr::contains("_spr"))
  nonBiasCorrected_WHPT_aspt_aut <- obs_aspt_aut / dplyr::select(Exp_ref_aspt, dplyr::contains("_aut"))

  # Now do the Obs_rb withONE SITE obs_aspt_spr[1]
  sdobs_aspt <- sdobs_one_year_new(0.269, 0.279, 1)

  SiteProbabilityclasses_spr_aspt <- data.frame() # Store site probabilities in a dataframe
  SiteProbabilityclasses_aut_aspt <- data.frame() # Store site probabilities in a dataframe
  SiteProbabilityclasses_spr_aut_comb_aspt <- data.frame()
  EQRAverages_aspt_spr <- data.frame() # Store average EQRs for spr in a dataframe
  EQRAverages_aspt_aut <- data.frame() # Store average EQRs for spr in a dataframe

  # **************  For NTAXA   *************
  Exp_ref_ntaxa <- ntaxa_adjusted / 1.0049 # select(adjusted_expected_new, contains("_NTAXA_"))/1.0049
  # head(Exp_ref_ntaxa,18)

  # Find the non-bias corrected  EQR = obs/ExpRef, from the raw inputs,
  # not used but useful for output checing purposes only
  nonBiasCorrected_WHPT_ntaxa_spr <- obs_ntaxa_spr / dplyr::select(Exp_ref_ntaxa, dplyr::contains("_spr"))
  nonBiasCorrected_WHPT_ntaxa_aut <- Obs_ntaxa_aut / dplyr::select(Exp_ref_ntaxa, dplyr::contains("_aut"))

  # Now do the obs_rb with ONE SITE obs_ntaxa_spr[1]
  sdobs_ntaxa <- sdobs_one_year_new(0.247, 0.211, 1)

  SiteProbabilityclasses_spr_ntaxa <- data.frame() # Store site probabilities in a dataframe
  SiteProbabilityclasses_aut <- data.frame() # Store site probabilities in a dataframe
  SiteProbabilityclasses_aut_ntaxa <- data.frame()
  SiteProbabilityclasses_spr_aut_comb_ntaxa <- data.frame()
  SiteProbabilityclasses_spr_aut_comb_aspt <- data.frame()

  # MINTA
  SiteMINTA_whpt_spr <- data.frame()
  SiteMINTA_whpt_aut <- data.frame()
  SiteMINTA_whpt_spr_aut <- data.frame()

  EQRAverages_ntaxa_spr <- data.frame() # Store average EQRs for spr in a dataframe
  EQRAverages_ntaxa_aut <- data.frame() # Store average EQRs for spr in a dataframe

  # ASPT
  SiteProbabilityclasses_spr_aspt <- data.frame() # Store site probabilities in a dataframe
  SiteProbabilityclasses_aut_aspt <- data.frame() # Store site probabilities in a dataframe
  SiteProbabilityclasses_spr_aut_comb_aspt <- data.frame()
  classArray_siteOne_spr_aut_ntaxa <- data.frame()
  classArray_siteOne_spr_aut_aspt <- data.frame()

  # Setup biases
  Ubias8r_spr <- getUbias8r_new(n_runs, Ubias8)
  Ubias8r_aut <- getUbias8r_new(n_runs, Ubias8)

  for (k in seq_len(nrow(predictions))) {

    # LOOP all the sites from here
    # Part 1. Adjust the Observed values
    # Loop strarts from here with site = k, i.e. sqr (sqrt(Obs) + ZObs) + Ubias8r

    ObsIDX8r_spr <- getObsIDX8rB(obs_ntaxa_spr[k], getZObs_r_new(sdobs_ntaxa, n_runs))
    ObsIDX8r_aut <- getObsIDX8rB(Obs_ntaxa_aut[k], getZObs_r_new(sdobs_ntaxa, n_runs))

    Obs_site1_ntaxa_spr <- ObsIDX8r_spr + Ubias8r_spr # rename "Obs_site1_ntaxa_spr" to ObsIDX8rb_spr
    Obs_site1_ntaxa_aut <- ObsIDX8r_aut + Ubias8r_aut # rename "Obs_site1_ntaxa_aut" to ObsIDX8rb_aut

    # Part 2 . Do the RefAdjExpected bias

    sdexp8_ntaxa <- 0.53 # For aspt we use a different valsue
    ExpIDX8r_ntaxa_spr <- data.frame(val = (Exp_ref_ntaxa[k, 1] + getZObs_r_new(sdexp8_ntaxa, n_runs)))
    ExpIDX8r_ntaxa_aut <- data.frame(val = (Exp_ref_ntaxa[k, 2] + getZObs_r_new(sdexp8_ntaxa, n_runs)))

    EQR_ntaxa_spr <- as.data.frame(Obs_site1_ntaxa_spr / ExpIDX8r_ntaxa_spr[, 1])
    EQR_ntaxa_aut <- as.data.frame(Obs_site1_ntaxa_aut / ExpIDX8r_ntaxa_aut[, 1])

    # Part 1: for "Spring" - DO FOR NTAXA

    # Find the averages of both spr and autum, declare a function to compute this
    eqr_av_spr <- getAvgEQR_SprAut(EQR_spr = EQR_ntaxa_spr, EQR_aut = EQR_ntaxa_aut, k, row_name = T)

    # Classify these for each SITE using the EQR just for spring
    classArray_siteOne_spr_ntaxa <- getClassarray_ntaxa(EQR_ntaxa_spr)
    classArray_siteOne_aut_ntaxa <- getClassarray_ntaxa(EQR_ntaxa_aut)

    # Define an array to hold probability of class for each site- how much of
    # the site belongs to each classes, adds up to 100%.
    # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or
    # ntaxa_spr, ntaxa_aut, spr_aut_av_taxa, and spt etc.
    probClass_spr <- matrix(0, ncol = 1, nrow = 5)
    probClass_aut <- matrix(0, ncol = 1, nrow = 5)

    for (i in 1:5) {
      probClass_spr[i] <- 100 * sum(classArray_siteOne_spr_ntaxa[classArray_siteOne_spr_ntaxa == i, ] / i) / n_runs
      probClass_aut[i] <- 100 * sum(classArray_siteOne_aut_ntaxa[classArray_siteOne_aut_ntaxa == i, ] / i) / n_runs
    }

    probabilityClass <- getProbClassLabelFromEQR()
    a_ntaxa_spr <- t(probClass_spr) # spr
    colnames(a_ntaxa_spr) <- getProbClassLabelFromEQR()[, 1]
    rownames(a_ntaxa_spr) <- as.character(predictions[k, "SITE"])

    # Find most probable class, i.e the maximum, and add it to the site
    mostProb <- getMostProbableClass(a_ntaxa_spr)
    a_ntaxa_spr <- cbind(a_ntaxa_spr, mostProb)
    SiteProbabilityclasses_spr_ntaxa <- rbind(SiteProbabilityclasses_spr_ntaxa, a_ntaxa_spr)
    # Add the averages of spr,aut
    EQRAverages_ntaxa_spr <- rbind(EQRAverages_ntaxa_spr, eqr_av_spr)

    # Part 2: for Autumn
    a_ntaxa_aut <- t(probClass_aut) # aut
    colnames(a_ntaxa_aut) <- getProbClassLabelFromEQR()[, 1]
    rownames(a_ntaxa_aut) <- as.character(predictions[k, "SITE"])

    mostProb <- getMostProbableClass(a_ntaxa_aut)
    a_ntaxa_aut <- cbind(a_ntaxa_aut, mostProb)
    SiteProbabilityclasses_aut_ntaxa <- rbind(SiteProbabilityclasses_aut_ntaxa, a_ntaxa_aut)
    # Add the averages of spr,aut

    # Part 3:: Do combined spr, aut processing
    # First find the row averages of all the 10,000 simulations
    rowAverage_spr_aut <- data.frame(rowMeans(cbind(EQR_ntaxa_spr, EQR_ntaxa_aut)))
    # Classify these for each SITE using the EQR just for spring
    classArray_siteOne_combined_spr <- getClassarray_ntaxa(rowAverage_spr_aut)
    # Define an array to hold probability of class
    probClass_spr_aut_comb <- matrix(0, ncol = 1, nrow = 5)
    # Process probabilities
    for (i in 1:5) {
      probClass_spr_aut_comb[i] <- 100 *
        sum(classArray_siteOne_combined_spr[classArray_siteOne_combined_spr == i, ] / i) / n_runs
    }

    a_ntaxa_spr_aut <- t(probClass_spr_aut_comb) # spr
    colnames(a_ntaxa_spr_aut) <- getProbClassLabelFromEQR()[, 1] # Rename the columns to H G M P B
    rownames(a_ntaxa_spr_aut) <- as.character(predictions[k, "SITE"])
    # Find most probable class, i.e the maximum, and add it to the site
    mostProb <- getMostProbableClass(a_ntaxa_spr_aut)
    a_ntaxa_spr_aut <- cbind(a_ntaxa_spr_aut, mostProb)
    SiteProbabilityclasses_spr_aut_comb_ntaxa <- rbind(SiteProbabilityclasses_spr_aut_comb_ntaxa, a_ntaxa_spr_aut)

    # **** Workout FOR ASPT STARTS HERE
    ### RALPH
    u_9a <- 4.35
    u_9b <- 0.271
    u_9c <- 2.5

    #### RALPH
    Ubias9r_spr <- getUbias9r_new(u_9a, u_9b, u_9c, obs_aspt_spr[k], n_runs, Ubias8r_spr)
    Ubias9r_aut <- getUbias9r_new(u_9a, u_9b, u_9c, obs_aspt_aut[k], n_runs, Ubias8r_aut)

    Ubias7r_spr <- Ubias8r_spr * Ubias9r_spr
    Ubias7r_aut <- Ubias8r_aut * Ubias9r_aut

    ObsIDX9r_spr <- getObsIDX9r(obs_aspt_spr[k], getZObs_r_new(sdobs_aspt, n_runs))
    ObsIDX9r_aut <- getObsIDX9r(obs_aspt_aut[k], getZObs_r_new(sdobs_aspt, n_runs))

    ObsIDX7r_spr <- ObsIDX8r_spr * ObsIDX9r_spr
    ObsIDX7r_aut <- ObsIDX8r_aut * ObsIDX9r_aut

    ObsIDX7rb_spr <- ObsIDX7r_spr + Ubias7r_spr
    ObsIDX7rb_aut <- ObsIDX7r_aut + Ubias7r_aut

    ObsIDX8rb_spr <- ObsIDX8r_spr + Ubias8r_spr
    ObsIDX8rb_aut <- ObsIDX8r_aut + Ubias8r_aut

    # Obs_site1_aspt_spr <-
    # getObsIDX8r_new(obs_aspt_spr[k],getZObs_r_new(sdobs_aspt,n_runs)) +
    # getUbias8r_new (n_runs, Ubias8) # ths is replaced by "ObsIDX9rB_spr"
    # Obs_site1_aspt_aut <-
    # getObsIDX8r_new(obs_aspt_aut[k], getZObs_r_new(sdobs_aspt,n_runs)) +
    # getUbias8r_new (n_runs, Ubias8)

    ObsIDX9rb_spr <- ObsIDX7rb_spr / ObsIDX8rb_spr
    ObsIDX9rb_aut <- ObsIDX7rb_aut / ObsIDX8rb_aut

    # Part 2 . Do the RefAdjExpected bias
    # Expected reference adjusted , as an array , ONE SITE, site 14

    sdexp9_aspt <- 0.081 # For aspt we use a different value, 0.081
    ExpIDX9r_aspt_spr <- data.frame(val = (Exp_ref_aspt[k, 1] + getZObs_r_new(sdexp9_aspt, n_runs)))
    ExpIDX9r_aspt_aut <- data.frame(val = (Exp_ref_aspt[k, 2] + getZObs_r_new(sdexp9_aspt, n_runs)))

    # Calculating simulated EQR
    EQR_aspt_spr <- as.data.frame(ObsIDX9rb_spr / ExpIDX9r_aspt_spr[, 1])
    EQR_aspt_aut <- as.data.frame(ObsIDX9rb_aut / ExpIDX9r_aspt_aut[, 1])

    # Part 1: for "Spring"
    # Find the averages of both spr and autum, declare a function to compute this
    eqr_av_spr_aspt <- getAvgEQR_SprAut(EQR_aspt_spr, EQR_aspt_aut, k, row_name = T)

    # Classify these for each SITE using the EQR just for spring
    classArray_siteOne_spr_aspt <- getClassarray_aspt(EQR_aspt_spr)
    classArray_siteOne_aut_aspt <- getClassarray_aspt(EQR_aspt_aut)

    # define an array to hold probability of class for each site- how much of
    # the site belongs to each classes, adds up to 100%
    # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or
    # ntaxa_spr, ntaxa_aut, spr_aut_av_taxa, and spt etc
    probClass_spr <- matrix(0, ncol = 1, nrow = 5)
    probClass_aut <- matrix(0, ncol = 1, nrow = 5)

    for (i in 1:5) {
      probClass_spr[i] <- 100 * sum(classArray_siteOne_spr_aspt[classArray_siteOne_spr_aspt == i, ] / i) / n_runs
      probClass_aut[i] <- 100 * sum(classArray_siteOne_aut_aspt[classArray_siteOne_aut_aspt == i, ] / i) / n_runs
    }

    # Work out ASPT probability of classes
    # probabilityClass <- getProbClassLabelFromEQR()
    a_aspt_spr <- t(probClass_spr) # spr
    colnames(a_aspt_spr) <- getProbClassLabelFromEQR()[, 1]
    rownames(a_aspt_spr) <- as.character(predictions[k, "SITE"])

    # Find most probable class, i.e the maximum, and add it to the site
    mostProb <- getMostProbableClass(a_aspt_spr)
    # add the site to the dataframe
    a_aspt_spr <- cbind(a_aspt_spr, mostProb)

    SiteProbabilityclasses_spr_aspt <- rbind(SiteProbabilityclasses_spr_aspt, a_aspt_spr)
    # Add the averages of spr
    EQRAverages_aspt_spr <- rbind(EQRAverages_aspt_spr, eqr_av_spr_aspt)

    # Part 2: for Autumn
    a_aspt_aut <- t(probClass_aut) # aut
    colnames(a_aspt_aut) <- getProbClassLabelFromEQR()[, 1]
    rownames(a_aspt_aut) <- as.character(predictions[k, "SITE"])
    mostProb <- getMostProbableClass(a_aspt_aut)
    a_aspt_aut <- cbind(a_aspt_aut, mostProb)
    SiteProbabilityclasses_aut_aspt <- rbind(SiteProbabilityclasses_aut_aspt, a_aspt_aut)

    # Part 3:: start the combined spr_aut processing
    # First find the row averages of all the 10,000 simulations
    rowAverage_spr_aut <- data.frame(rowMeans(cbind(EQR_aspt_spr, EQR_aspt_aut)))
    # Classify these for each SITE using the EQR just for spring
    classArray_siteOne_combined_spr_aspt <- getClassarray_aspt(rowAverage_spr_aut)
    # Define an array to hold probability of class
    probClass_spr_aut_comb <- matrix(0, ncol = 1, nrow = 5)
    # Process probabilities

    for (i in 1:5) {
      probClass_spr_aut_comb[i] <- 100 *
        sum(classArray_siteOne_combined_spr_aspt[classArray_siteOne_combined_spr_aspt == i, ] / i) / n_runs
    }

    a_aspt_spr_aut <- t(probClass_spr_aut_comb) # spr
    colnames(a_aspt_spr_aut) <- getProbClassLabelFromEQR()[, 1] # Rename the columns to H G M P B
    rownames(a_aspt_spr_aut) <- as.character(predictions[k, "SITE"])
    # Find most probable class, i.e the maximum, and add it to the site
    mostProb <- getMostProbableClass(a_aspt_spr_aut)
    a_aspt_spr_aut <- cbind(a_aspt_spr_aut, mostProb)
    SiteProbabilityclasses_spr_aut_comb_aspt <- rbind(SiteProbabilityclasses_spr_aut_comb_aspt, a_aspt_spr_aut)

    ########  Calculate the MINTA - worse class = 5 i.e. max of class from NTAXA and ASPT ######
    matrix_ntaxa_spr <- as.matrix(classArray_siteOne_spr_ntaxa)
    matrix_aspt_spr <- as.matrix(classArray_siteOne_spr_aspt)
    minta_ntaxa_aspt_spr <- getMINTA_ntaxa_aspt(as.matrix(classArray_siteOne_spr_ntaxa),
                                                as.matrix(classArray_siteOne_spr_aspt))

    # Now calculate proportion of each class H to B for MINTA
    # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or
    # ntaxa_spr, ntaxa_aut, spr_aut_av_taxa, and spt etc
    minta_probClass_spr <- matrix(0, ncol = 1, nrow = 5)

    for (i in 1:5) {
      minta_probClass_spr[i] <- 100 * sum(minta_ntaxa_aspt_spr[minta_ntaxa_aspt_spr == i, ] / i) / n_runs
    }

    # probabilityClass <- getProbClassLabelFromEQR()
    aa <- t(minta_probClass_spr) # spr
    colnames(aa) <- getProbClassLabelFromEQR()[, 1]
    rownames(aa) <- as.character(predictions[k, "SITE"])
    # Find most probable MINTA class, i.e the maximum, and add it to the site
    mostProb <- getMostProbableClass(aa)
    aa <- cbind(aa, mostProb)
    # Now bind the MINTA proportion to the dataframe
    SiteMINTA_whpt_spr <- rbind(SiteMINTA_whpt_spr, aa)

    # Do the MINTA aut case
    minta_ntaxa_aspt_aut <- getMINTA_ntaxa_aspt(as.matrix(classArray_siteOne_aut_ntaxa),
                                                as.matrix(classArray_siteOne_aut_aspt))
    # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or
    # ntaxa_spr, ntaxa_aut, spr_aut_av_taxa, and spt etc
    minta_probClass_aut <- matrix(0, ncol = 1, nrow = 5)
    for (i in 1:5) {
      minta_probClass_aut[i] <- 100 * sum(minta_ntaxa_aspt_aut[minta_ntaxa_aspt_aut == i, ] / i) / n_runs
    }

    # probabilityClass <- getProbClassLabelFromEQR()
    aa <- t(minta_probClass_aut) # spr
    colnames(aa) <- getProbClassLabelFromEQR()[, 1]
    rownames(aa) <- as.character(predictions[k, "SITE"])
    # Find most probable MINTA class, i.e the maximum, and add it to the site
    mostProb <- getMostProbableClass(aa)
    aa <- cbind(aa, mostProb)
    # Now bind the MINTA proportion to the dataframe
    SiteMINTA_whpt_aut <- rbind(SiteMINTA_whpt_aut, aa)

    # Do the MINTA spr_aut case
    minta_ntaxa_aspt_spr_aut <- getMINTA_ntaxa_aspt(as.matrix(classArray_siteOne_combined_spr),
                                                    as.matrix(classArray_siteOne_combined_spr_aspt))
    # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or
    # ntaxa_spr, ntaxa_aut, spr_aut_av_taxa, and spt etc
    minta_probClass_spr_aut <- matrix(0, ncol = 1, nrow = 5)

    for (i in 1:5) {
      minta_probClass_spr_aut[i] <- 100 * sum(minta_ntaxa_aspt_spr_aut[minta_ntaxa_aspt_spr_aut == i, ] / i) / n_runs
    }

    # probabilityClass <- getProbClassLabelFromEQR()
    aa <- t(minta_probClass_spr_aut) # spr
    colnames(aa) <- getProbClassLabelFromEQR()[, 1]
    rownames(aa) <- as.character(predictions[k, "SITE"])
    # Find most probable MINTA class, i.e the maximum, and add it to the site
    mostProb <- getMostProbableClass(aa)
    aa <- cbind(aa, mostProb)
    # Now bind the MINTA proportion to the dataframe
    SiteMINTA_whpt_spr_aut <- rbind(SiteMINTA_whpt_spr_aut, aa)
    ##### MINTA ENDS HERE  #############
  } # END of FOR LOOP

  # MINTA outputs
  # head(SiteMINTA_whpt_spr,11)
  colnames(SiteMINTA_whpt_spr) <- c(paste0("mintawhpt_spr_", names(SiteMINTA_whpt_spr)))
  colnames(SiteMINTA_whpt_aut) <- c(paste0("mintawhpt_aut_", names(SiteMINTA_whpt_aut)))
  colnames(SiteMINTA_whpt_spr_aut) <- c(paste0("mintawhpt_spr_aut_", names(SiteMINTA_whpt_spr_aut)))
  # Combine all MINTA

  allMINTA_whpt <- cbind(SiteMINTA_whpt_spr, SiteMINTA_whpt_aut)
  allMINTA_whpt <- cbind(allMINTA_whpt, SiteMINTA_whpt_spr_aut)
  # ****** For NTAXA outputs ********
  # Find the averages of these across seasons aver#(spr, aut)
  colnames(EQRAverages_ntaxa_spr) <- c(paste0("NTAXA_", colnames(EQRAverages_ntaxa_spr)))
  whpt_ntaxa_spr_aut_averages <- data.frame(NTAXA_aver_spr_aut = rowMeans(EQRAverages_ntaxa_spr))

  # Rename column names so they dont conflict
  colnames(SiteProbabilityclasses_spr_ntaxa) <- paste0(colnames(SiteProbabilityclasses_spr_ntaxa), "_NTAXA_spr")
  colnames(SiteProbabilityclasses_aut_ntaxa) <- paste0(colnames(SiteProbabilityclasses_aut_ntaxa), "_NTAXA_aut")
  colnames(SiteProbabilityclasses_spr_aut_comb_ntaxa) <- paste0(colnames(SiteProbabilityclasses_spr_aut_comb_ntaxa),
                                                                "_NTAXA_spr_aut")

  # Get ntaxa spr average
  averages_spr_ntaxa <- cbind(SiteProbabilityclasses_spr_ntaxa, EQRAverages_ntaxa_spr[1]) #
  # Get ntaxa aut
  averages_aut_ntaxa <- cbind(SiteProbabilityclasses_aut_ntaxa, EQRAverages_ntaxa_spr[2]) #
  all_spr_aut_ntaxa_averages <- cbind(averages_spr_ntaxa, averages_aut_ntaxa)

  allProbClasses_ave_ntaxa <- cbind(all_spr_aut_ntaxa_averages, SiteProbabilityclasses_spr_aut_comb_ntaxa)
  allResults <- cbind(allProbClasses_ave_ntaxa, whpt_ntaxa_spr_aut_averages)

  # ****** For ASPT outputs ********
  # Find the averages of these across seasons aver#(spr, aut)
  colnames(EQRAverages_aspt_spr) <- c(paste0("ASPT_", colnames(EQRAverages_aspt_spr)))
  whpt_aspt_spr_aut_averages_aspt <- data.frame(ASPT_aver_spr_aut = rowMeans(EQRAverages_aspt_spr))

  # Rename column names so they dont conflict
  colnames(SiteProbabilityclasses_spr_aspt) <- paste0(colnames(SiteProbabilityclasses_spr_aspt), "_ASPT_spr")
  colnames(SiteProbabilityclasses_aut_aspt) <- paste0(colnames(SiteProbabilityclasses_aut_aspt), "_ASPT_aut")
  colnames(SiteProbabilityclasses_spr_aut_comb_aspt) <- paste0(colnames(SiteProbabilityclasses_spr_aut_comb_aspt),
                                                               "_ASPT_spr_aut")

  averages_spr_aspt <- cbind(SiteProbabilityclasses_spr_aspt, EQRAverages_aspt_spr[1]) #
  probclasses_ave_aspt <- cbind(SiteProbabilityclasses_aut_aspt, EQRAverages_aspt_spr[2]) # averages_spr_aspt)
  allProbClasses_ave_aspt <- cbind(averages_spr_aspt, probclasses_ave_aspt) #  SiteProbabilityclasses_spr_aut_comb_aspt)
  allProbClasses_ave_aspt <- cbind(allProbClasses_ave_aspt, SiteProbabilityclasses_spr_aut_comb_aspt)
  allResults_aspt <- cbind(allProbClasses_ave_aspt, whpt_aspt_spr_aut_averages_aspt)
  # Name the columns of all ASPT
  allResults_ntaxa_aspt <- cbind(allResults, allResults_aspt)

  # Add MINTA whpt results
  allResults_ntaxa_aspt <- cbind(allResults_ntaxa_aspt, allMINTA_whpt)
  allResults_ntaxa_aspt_minta_combined <- cbind(all_sites, allResults_ntaxa_aspt)

  allResults_ntaxa_aspt_minta_combined$SITE <-
    allResults_ntaxa_aspt_minta_combined$all_sites
  allResults_ntaxa_aspt_minta_combined$all_sites <- NULL

  allResults_ntaxa_aspt_minta_combined$mintawhpt_spr_aut_mostProb_MINTA_ <-
    allResults_ntaxa_aspt_minta_combined$mintawhpt_spr_aut_mostProb
  allResults_ntaxa_aspt_minta_combined$mintawhpt_spr_aut_mostProb <- NULL
  return(allResults_ntaxa_aspt_minta_combined)
}
