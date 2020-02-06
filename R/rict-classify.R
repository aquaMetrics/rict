#' Calculate classification
#'
#' @param data Dataframe of predicted endgroups values from
#'   'rict__predict' function
#' @param year_type "single" or "multi" depending if multi-year classification
#'   required - default is "multi"
#' @param store_eqrs Boolean to signal if simulate EQRs should be stored. If
#'   TRUE, EQRs are stored allowing `rict_compare` to compare EQR results
#' @return Dataframe of classification results
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' predictions <- rict_predict(demo_observed_values)
#' classifications <- rict_classify(predictions)
#' }
#'
rict_classify <- function(data = NULL, year_type = "multi", store_eqrs = F) {
  message("Classifying...")
  # This is a hack - best to combine single year and multiple year into single
  # function? For now, I've just stuck the single year into a different
  # function until these can be merged
  if (year_type == "single") {
    classification_results <- singleYearClassification(data, store_eqrs)
    return(classification_results)
  } else {
    # set global random seed for rnorm functions etc
    set.seed(1234)
    # Part 1: This Script reads all prediction indices for classification

    gb685_assess_score <- utils::read.csv(system.file("extdat",
      "end-grp-assess-scores.csv",
      package = "rict"
    ))
    adjusted_params <- utils::read.csv(system.file("extdat",
      "adjust-params-ntaxa-aspt.csv",
      package = "rict"
    ))


    # Enter source files
    # Use the column header as site names in the final output
    all_sites <- data[, 1]
    # Keep YEAR, WATERBODY

    year_waterBody <- data[, c("YEAR", "WATERBODY")]

    # Combine all_sites with more information  - e.g. YEAR, WATERBODY
    all_sites <- cbind(all_sites, year_waterBody)

    # data <- data[,-1] Create a routine that checks column names
    # and puts them in the "data" dataframe
    # data <- data[,-c(1,3:15)] Change all names to upper case
    names(data) <- toupper(names(data))

    # Remove the "_CompFarm_" columns
    data$`_COMPFAM_` <- NULL
    # Get the biological data TL2_WHPT_NTAXA_AbW_DistFam_spr
    names_biological <- c(
      "SPR_SEASON_ID", "SPR_TL2_WHPT_ASPT (ABW,DISTFAM)",
      "SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SPR_NTAXA_BIAS",
      "SUM_SEASON_ID", "SUM_TL2_WHPT_ASPT (ABW,DISTFAM)",
      "SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SUM_NTAXA_BIAS",
      "AUT_SEASON_ID", "AUT_TL2_WHPT_ASPT (ABW,DISTFAM)",
      "AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)", "AUT_NTAXA_BIAS"
    )

    biological_data <- data[, names_biological]
    # head(biological_data,9) # works now
    # head(names(biological_data),9) # works now
    # Remove biological_data from data
    data <- data[, !names(data) %in% names_biological]

    # Store all_probabilities in one dataframe.
    # Use p1,p2,... etc in case data column positions change in future
    prob_names <- paste0("p", 1:43)
    # Needs to change when not uppercase
    all_probabilities <- data[, toupper(prob_names)]
    # Input Adjustment factors for reference site quality scores (Q1, Q2, Q3, Q4, Q5)
    # Extract Ubias8 from Biological data
    ubias_main <- biological_data[, "SPR_NTAXA_BIAS"][1] # Put new AZURE

    # Put the ubias_main default value of 1.68 if the user does not enter any value or
    # enters a -9
    if (is.na(ubias_main) | ubias_main == -9) {
      # For NI model, the default is ZERO
      ubias_main <- 1.68
    }

    # OBSERVED ASPT
    obs_aspt_spr <- biological_data[, "SPR_TL2_WHPT_ASPT (ABW,DISTFAM)"]
    obs_aspt_aut <- biological_data[, "AUT_TL2_WHPT_ASPT (ABW,DISTFAM)"]

    # OBSERVED NTAXA
    obs_ntaxa_spr <- biological_data[, "SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)"]
    obs_ntaxa_aut <- biological_data[, "AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)"] # change AZURE

    # Input Multiplicative Adjustment factors adjusted_params, 1,..,5)
    adjusted_params <- as.matrix(adjusted_params)
    qij <- computeScoreProportions(gb685_assess_score[, -1]) # Remove the first Column

    # Part 2:  Calculate AdjustedExpected from all probabilities, WE4.5 of WFD72C
    # Compute rj = sum(Pi*qij)
    rj <- as.matrix(getWeighted_proportion_Rj(all_probabilities, qij)) # We should have five of these

    # Multiply rj by adjusted_params, note each row of adjusted_params is for
    # NTAXA, ASPT, so transpose to multiply by rj
    rjaj <- compute_RjAj(rj, adjusted_params)
    # one_over_rjaj <- 1 / rjaj

    # Write a function that computes aspt, ntaxa adjusted (1 = "NTAXA", 2="ASPT")
    # or select them by name as declared in the classification functions
    ntaxa_adjusted <- dplyr::select(data, dplyr::contains("_NTAXA_")) / rjaj[, "NTAXA"]
    # Compute AdjExpected as E=data/Sum(rj*adjusted_params)
    aspt_adjusted <- dplyr::select(data, dplyr::contains("_ASPT_")) / rjaj[, "ASPT"]

    adjusted_expected <- cbind(ntaxa_adjusted, aspt_adjusted)
    # Include site names from data
    adjusted_expected_new <- cbind(as.data.frame(all_sites), adjusted_expected)

    # Part 3:  Calculation of Exp_ref from "AdjustedExpected_new" values,
    # divide by K ( = 1.0049 for NTAXA,  = 0.9921 for ASPT)
    # run simulations from here
    n_runs <- 10000

    # ******* FOR ASPT ************
    Exp_ref_aspt <- aspt_adjusted / 0.9921
    Ubias8 <- ubias_main

    # find the non-bias corrected  EQR = obs/ExpRef
    nonBiasCorrected_WHPT_aspt_spr <-
      obs_aspt_spr / dplyr::select(Exp_ref_aspt, dplyr::contains("_spr"))
    nonBiasCorrected_WHPT_aspt_aut <-
      obs_aspt_aut / dplyr::select(Exp_ref_aspt, dplyr::contains("_aut"))

    # Now do the obs_rb with ONE SITE obs_aspt_spr[1]
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
    # not used but useful for output checking purposes only
    nonBiasCorrected_WHPT_ntaxa_spr <-
      obs_ntaxa_spr / dplyr::select(Exp_ref_ntaxa, dplyr::contains("_spr"))
    nonBiasCorrected_WHPT_ntaxa_aut <-
      obs_ntaxa_aut / dplyr::select(Exp_ref_ntaxa, dplyr::contains("_aut"))

    # Now do the obs_rb with ONE SITE obs_ntaxa_spr[1]
    sdobs_ntaxa <- sdobs_one_year_new(0.247, 0.211, 1)
    # Define sdexp
    sdexp8_ntaxa <- 0.53
    sdexp9_aspt <- 0.081
    # Define for ASPT
    u_9a <- 4.35
    u_9b <- 0.271
    u_9c <- 2.5

    SiteProbabilityclasses_spr_ntaxa <- data.frame() # Store site probabilities in a dataframe
    SiteProbabilityclasses_aut <- data.frame() # Store site probabilities in a dataframe
    SiteProbabilityclasses_aut_ntaxa <- data.frame()
    SiteProbabilityclasses_spr_aut_comb_ntaxa <- data.frame()
    SiteProbabilityclasses_spr_aut_comb_aspt <- data.frame()

    # MINTA
    SiteMINTA_whpt_spr <- data.frame()
    SiteMINTA_whpt_aut <- data.frame()
    SiteMINTA_whpt_spr_aut <- data.frame()

    # ASPT
    SiteProbabilityclasses_spr_aspt <- data.frame() # Store site probabilities in a dataframe
    SiteProbabilityclasses_aut_aspt <- data.frame() # Store site probabilities in a dataframe
    SiteProbabilityclasses_spr_aut_comb_aspt <- data.frame()
    classArray_siteOne_spr_aut_ntaxa <- data.frame()
    classArray_siteOne_spr_aut_aspt <- data.frame()

    # Setup biases
    Ubias8r_spr <- getUbias8r_new(n_runs, Ubias8)
    Ubias8r_aut <- getUbias8r_new(n_runs, Ubias8)

    # Store all multiYear
    EQRAverages_ntaxa_spr_aut <- data.frame() # Store average EQRs for spr in a dataframe
    EQRAverages_aspt_spr_aut <- data.frame() # Store average EQRs for spr in a dataframe

    # initalise all MultiYear
    multiYear_EQRAverages_ntaxa_spr <- data.frame(n = n_runs)
    multiYear_EQRAverages_ntaxa_aut <- data.frame(n = n_runs)
    multiYear_EQRAverages_ntaxa_spr_aut <- data.frame(n = n_runs) # Stores averages for nyears-use to calculate,
    # find all spring, all autumn, then average these for each index

    multiYear_EQRAverages_aspt_spr <- data.frame(n = n_runs)
    multiYear_EQRAverages_aspt_aut <- data.frame(n = n_runs)
    multiYear_EQRAverages_aspt_spr_aut <- data.frame(n = n_runs) # Stores averages for nyears-use to calculate,
    # find all spring, all autumn, then average these for each index

    multipleSite_encoutered <- FALSE
    # Store the duplicated names of sites as single names
    namesOfSites <- data.frame()

    # Create store for EQRs to retain for compare function
    if (store_eqrs == T) {
    ASPT <- list()
    NTAXA <- list()
    MINTA <- list()
    }
    # Variable that flags if last site has not been processed
    lastSiteProcessed <- FALSE

    # Collection of indices
    indicesDistinct <- data.frame()
    k <- 1

    while (k <= nrow(data) | (lastSiteProcessed == FALSE)) {
      # initalise all MultiYear AGAIN for each site
      multiYear_EQRAverages_ntaxa_spr <- data.frame(n = n_runs)
      multiYear_EQRAverages_ntaxa_aut <- data.frame(n = n_runs)
      # Stores averages for nyears-use to calculate, find all spring, all autumn, then average these for each index
      multiYear_EQRAverages_ntaxa_spr_aut <- data.frame(n = n_runs)

      multiYear_EQRAverages_aspt_spr <- data.frame(n = n_runs)
      multiYear_EQRAverages_aspt_aut <- data.frame(n = n_runs)
      # Stores averages for nyears-use to calculate, find all spring, all
      # autumn, then average these for each index
      multiYear_EQRAverages_aspt_spr_aut <- data.frame(n = n_runs)

      # Declare a boolean variable that indicates multiple sites encountered,
      # then switch it back to FALSE at start of loop
      j <- k
      # print(c(" j = ", j))
      indicesDistinct <- rbind(indicesDistinct, j)

      if (j < nrow(data) && (data[j, "SITE"] == data[j + 1, "SITE"])) {
        multipleSite_encoutered <- TRUE
      }

      # Get site out
      siteToProcess <- data[j, "SITE"]
      while ((data[j, "SITE"] == siteToProcess && j <= nrow(data))) {
        # print(c("Processing site j= ",j, " as ", as.character(data[j,"SITE"]), " and site j= ",j+1," as ",
        # as.character(data[j+1,"SITE"])))

        # Part 1: Deal with NTAXA: observed and Expected Calculations
        obsIDX8r_spr <- getObsIDX8rB(obs_ntaxa_spr[j], getZObs_r_new(sdobs_ntaxa, n_runs))
        obsIDX8r_aut <- getObsIDX8rB(obs_ntaxa_aut[j], getZObs_r_new(sdobs_ntaxa, n_runs))
        obs_site1_ntaxa_spr <- obsIDX8r_spr + Ubias8r_spr # rename "obs_site1_ntaxa_spr" to obsIDX8rb_spr
        obs_site1_ntaxa_aut <- obsIDX8r_aut + Ubias8r_aut # rename "obs_site1_ntaxa_aut" to obsIDX8rb_aut
        # Part 2 . Do the RefAdjExpected bias
        ExpIDX8r_ntaxa_spr <- data.frame(val = (Exp_ref_ntaxa[j, 1] + getZObs_r_new(sdexp8_ntaxa, n_runs)))
        ExpIDX8r_ntaxa_aut <- data.frame(val = (Exp_ref_ntaxa[j, 2] + getZObs_r_new(sdexp8_ntaxa, n_runs)))

        EQR_ntaxa_spr <- as.data.frame(obs_site1_ntaxa_spr / ExpIDX8r_ntaxa_spr[, 1])
        EQR_ntaxa_aut <- as.data.frame(obs_site1_ntaxa_aut / ExpIDX8r_ntaxa_aut[, 1])

        # Store these multi sites for ntaxa here
        # Afterwards, use:  EQR_ntaxa_spr <- rowMeans(multiYear_EQRAverages_ntaxa_spr[,-1])
        multiYear_EQRAverages_ntaxa_spr <- cbind(multiYear_EQRAverages_ntaxa_spr, EQR_ntaxa_spr)
        # Afterwards, use  EQR_ntaxa_aut <- rowMeans(multiYear_EQRAverages_ntaxa_aut[,-1])
        multiYear_EQRAverages_ntaxa_aut <- cbind(multiYear_EQRAverages_ntaxa_aut, EQR_ntaxa_aut)

        # Part 1: Deal with ASPT: observed and Expected Calculations
        # ****************************************
        # **** Workout FOR ASPT STARTS HERE
        # Part 1: Deal with ASPT : observed and Expected Calculations

        Ubias9r_spr <- getUbias9r_new(u_9a, u_9b, u_9c, obs_aspt_spr[j], n_runs, Ubias8r_spr)
        Ubias9r_aut <- getUbias9r_new(u_9a, u_9b, u_9c, obs_aspt_aut[j], n_runs, Ubias8r_aut)
        Ubias7r_spr <- Ubias8r_spr * Ubias9r_spr
        Ubias7r_aut <- Ubias8r_aut * Ubias9r_aut
        obsIDX9r_spr <- getObsIDXniner(obs_aspt_spr[j], getZObs_r_new(sdobs_aspt, n_runs))
        obsIDX9r_aut <- getObsIDXniner(obs_aspt_aut[j], getZObs_r_new(sdobs_aspt, n_runs))
        obsIDX7r_spr <- obsIDX8r_spr * obsIDX9r_spr
        obsIDX7r_aut <- obsIDX8r_aut * obsIDX9r_aut
        obsIDX7rb_spr <- obsIDX7r_spr + Ubias7r_spr
        obsIDX7rb_aut <- obsIDX7r_aut + Ubias7r_aut
        obsIDX8rb_spr <- obsIDX8r_spr + Ubias8r_spr
        obsIDX8rb_aut <- obsIDX8r_aut + Ubias8r_aut
        obsIDX9rb_spr <- obsIDX7rb_spr / obsIDX8rb_spr
        obsIDX9rb_aut <- obsIDX7rb_aut / obsIDX8rb_aut
        # Part 2 . Do the RefAdjExpected bias
        ExpIDX9r_aspt_spr <- data.frame(val = (Exp_ref_aspt[j, 1] + getZObs_r_new(sdexp9_aspt, n_runs)))
        ExpIDX9r_aspt_aut <- data.frame(val = (Exp_ref_aspt[j, 2] + getZObs_r_new(sdexp9_aspt, n_runs)))
        # Calculating simulated EQR
        EQR_aspt_spr <- data.frame(obsIDX9rb_spr / ExpIDX9r_aspt_spr[, 1])
        EQR_aspt_aut <- data.frame(obsIDX9rb_aut / ExpIDX9r_aspt_aut[, 1])

        # Store these multi sites for aspt here
        # Afterwards, use:  EQR_aspt_spr <- rowMeans(multiYear_EQRAverages_aspt_spr[,-1])
        multiYear_EQRAverages_aspt_spr <- cbind(multiYear_EQRAverages_aspt_spr, EQR_aspt_spr)
        # Afterwards, use:  EQR_aspt_aut <- rowMeans(multiYear_EQRAverages_aspt_aut[,-1])
        multiYear_EQRAverages_aspt_aut <- cbind(multiYear_EQRAverages_aspt_aut, EQR_aspt_aut)
        j <- j + 1
      }

      if (multipleSite_encoutered == FALSE) {
        if (k >= nrow(data)) {
          lastSiteProcessed <- TRUE
        }
        # Part 1: Deal with NTAXA: observed and Expected Calculations
        obsIDX8r_spr <- getObsIDX8rB(obs_ntaxa_spr[k], getZObs_r_new(sdobs_ntaxa, n_runs))
        obsIDX8r_aut <- getObsIDX8rB(obs_ntaxa_aut[k], getZObs_r_new(sdobs_ntaxa, n_runs))
        obs_site1_ntaxa_spr <- obsIDX8r_spr + Ubias8r_spr # rename "obs_site1_ntaxa_spr" to obsIDX8rb_spr
        obs_site1_ntaxa_aut <- obsIDX8r_aut + Ubias8r_aut # rename "obs_site1_ntaxa_aut" to obsIDX8rb_aut
        # Part 2 . Do the RefAdjExpected bias
        ExpIDX8r_ntaxa_spr <- data.frame(val = (Exp_ref_ntaxa[k, 1] + getZObs_r_new(sdexp8_ntaxa, n_runs)))
        ExpIDX8r_ntaxa_aut <- data.frame(val = (Exp_ref_ntaxa[k, 2] + getZObs_r_new(sdexp8_ntaxa, n_runs)))

        EQR_ntaxa_spr <- as.data.frame(obs_site1_ntaxa_spr / ExpIDX8r_ntaxa_spr[, 1])
        EQR_ntaxa_aut <- as.data.frame(obs_site1_ntaxa_aut / ExpIDX8r_ntaxa_aut[, 1])

        # Part 1: Deal with ASPT: observed and Expcted Calculations
        # ****************************************
        # **** Workout FOR ASPT STARTS HERE
        # Part 1: Deal with ASPT : observed and Expected Calculations

        Ubias9r_spr <- getUbias9r_new(u_9a, u_9b, u_9c, obs_aspt_spr[k], n_runs, Ubias8r_spr)
        Ubias9r_aut <- getUbias9r_new(u_9a, u_9b, u_9c, obs_aspt_aut[k], n_runs, Ubias8r_aut)
        Ubias7r_spr <- Ubias8r_spr * Ubias9r_spr
        Ubias7r_aut <- Ubias8r_aut * Ubias9r_aut
        obsIDX9r_spr <- getObsIDXniner(obs_aspt_spr[k], getZObs_r_new(sdobs_aspt, n_runs))
        obsIDX9r_aut <- getObsIDXniner(obs_aspt_aut[k], getZObs_r_new(sdobs_aspt, n_runs))
        obsIDX7r_spr <- obsIDX8r_spr * obsIDX9r_spr
        obsIDX7r_aut <- obsIDX8r_aut * obsIDX9r_aut
        obsIDX7rb_spr <- obsIDX7r_spr + Ubias7r_spr
        obsIDX7rb_aut <- obsIDX7r_aut + Ubias7r_aut
        obsIDX8rb_spr <- obsIDX8r_spr + Ubias8r_spr
        obsIDX8rb_aut <- obsIDX8r_aut + Ubias8r_aut
        obsIDX9rb_spr <- obsIDX7rb_spr / obsIDX8rb_spr
        obsIDX9rb_aut <- obsIDX7rb_aut / obsIDX8rb_aut
        # Part 2 . Do the RefAdjExpected bias
        ExpIDX9r_aspt_spr <- data.frame(val = (Exp_ref_aspt[k, 1] + getZObs_r_new(sdexp9_aspt, n_runs)))
        ExpIDX9r_aspt_aut <- data.frame(val = (Exp_ref_aspt[k, 2] + getZObs_r_new(sdexp9_aspt, n_runs)))
        # Calculating simulated EQR
        EQR_aspt_spr <- as.data.frame(obsIDX9rb_spr / ExpIDX9r_aspt_spr[, 1])
        EQR_aspt_aut <- as.data.frame(obsIDX9rb_aut / ExpIDX9r_aspt_aut[, 1])
      }

      if (multipleSite_encoutered == TRUE) {
        if ((j == nrow(data)) | (j - 1 == nrow(data))) {
          # Means last site was a duplicate, and so is processed
          lastSiteProcessed <- TRUE
        }
        # Move to current record just processed
        j <- j - 1
        # ******************************************
        # Part 1.1: for "Spring" - DO FOR NTAXA
        # Combined ntaxa spr-aut

        EQR_ntaxa_spr <- data.frame(EQR_ntaxa_spr = rowMeans(data.frame(multiYear_EQRAverages_ntaxa_spr[, -1])))
        EQR_ntaxa_aut <- data.frame(EQR_ntaxa_aut = rowMeans(data.frame(multiYear_EQRAverages_ntaxa_aut[, -1])))

        # ******************************************
        # Part 1.1: ASPT for "Spring"
        # Find the averages of both spr and autum, declare a function to compute this
        # First find all rowMeans, and store them in EQR appropriate variables

        EQR_aspt_spr <- data.frame(EQR_aspt_spr = rowMeans(data.frame(multiYear_EQRAverages_aspt_spr[, -1])))
        EQR_aspt_aut <- data.frame(EQR_aspt_aut = rowMeans(data.frame(multiYear_EQRAverages_aspt_aut[, -1])))
      }

      # Calculate EQRs here, i.e. rowSums if multipleTrue else just getAvgEQR() for single season
      eqr_av_spr <- data.frame(rowMeans(getAvgEQR_SprAut(EQR_ntaxa_spr, EQR_ntaxa_aut, k, row_name = T)))
      eqr_av_spr_aspt <- data.frame(rowMeans(getAvgEQR_SprAut(EQR_aspt_spr, EQR_aspt_aut, k, row_name = T)))

      # START TO CALCULATE probability of class
      # Part 2: Start calculating for NTAXA probability of CLASS

      multiYear_EQRAverages_ntaxa_spr_aut <- data.frame(rbind(cbind(EQR_ntaxa_spr, EQR_ntaxa_aut)))
      multiYear_EQRAverages_ntaxa_spr_aut <- data.frame(
        EQR_ntax_aspr_aut =
          rowMeans(multiYear_EQRAverages_ntaxa_spr_aut)
      )
      multiYear_EQRAverages_aspt_spr_aut <- data.frame(rbind(cbind(EQR_aspt_spr, EQR_aspt_aut)))
      multiYear_EQRAverages_aspt_spr_aut <- data.frame(
        EQR_aspt_spr_aut =
          rowMeans(multiYear_EQRAverages_aspt_spr_aut)
      )
      # data.frame(EQR_ntaxa_spr))

      classArray_siteOne_spr_aut_ntaxa <- getClassarray_ntaxa(multiYear_EQRAverages_ntaxa_spr_aut)
      # define an array to hold probability of class for each site- how much of the site belongs to each classes,
      # adds up to 100%
      # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or ntaxa_spr, ntaxa_aut,
      # spr_aut_av_taxa, and spt etc
      probClass_spr <- matrix(0, ncol = 1, nrow = 5)
      for (i in 1:5) {
        probClass_spr[i] <-
          100 * sum(classArray_siteOne_spr_aut_ntaxa[classArray_siteOne_spr_aut_ntaxa == i, ] / i) / n_runs
      }

      # Part 2.1: for Spring_aut
      probabilityClass <- getProbClassLabelFromEQR()
      a_ntaxa_spr_aut <- t(probClass_spr) # spr, need a_ntaxa_spr
      colnames(a_ntaxa_spr_aut) <- getProbClassLabelFromEQR()[, 1]
      rownames(a_ntaxa_spr_aut) <- as.character(data[j, "SITE"]) # c(paste0("TST-",j))

      # Find most probable class, i.e the maximum, and add it to the site
      mostProb <- getMostProbableClass(a_ntaxa_spr_aut)
      a_ntaxa_spr_aut <- data.frame(cbind(a_ntaxa_spr_aut, mostProb)) # add the site to the dataframe
      SiteProbabilityclasses_spr_aut_comb_ntaxa <- rbind(SiteProbabilityclasses_spr_aut_comb_ntaxa, a_ntaxa_spr_aut)

      # Add the averages of spr,aut
      EQRAverages_ntaxa_spr_aut <- rbind(EQRAverages_ntaxa_spr_aut, eqr_av_spr)

      # bind EQRs into list dataframe column
      if( store_eqrs == T) {
      eqr <- list(c(multiYear_EQRAverages_ntaxa_spr_aut))
      NTAXA <- rbind(NTAXA, eqr)
      }
      # ************** Now do the ASPT from HERE - using the calculations from ASPT ABOVE*******************

      # Part 2: Start calculating for ASPT probability of CLASS
      # Classify these for each SITE using the EQR just for spring
      # data.frame(EQR_ntaxa_aut))
      classArray_siteOne_spr_aut_aspt <- getClassarray_aspt(multiYear_EQRAverages_aspt_spr_aut)
      # define an array to hold probability of class for each site- how much of the site belongs to each classes,
      # adds up to 100%
      probClass_spr <- matrix(0, ncol = 1, nrow = 5) # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for
      # two seasons or ntaxa_spr, ntaxa_aut, spr_aut_av_taxa, and spt etc
      for (i in 1:5) {
        probClass_spr[i] <-
          100 * sum(classArray_siteOne_spr_aut_aspt[classArray_siteOne_spr_aut_aspt == i, ] / i) / n_runs
      }

      # Work out ASPT probability of classes
      # probabilityClass <- getProbClassLabelFromEQR()
      a_aspt_spr_aut <- t(probClass_spr) # spr
      colnames(a_aspt_spr_aut) <- getProbClassLabelFromEQR()[, 1]
      # print(c(" j =",j," site = ",as.character(data[j,"SITE"]), "pated TST = ",paste0("TST-",j)))
      rownames(a_aspt_spr_aut) <- as.character(data[j, "SITE"]) # c(paste0("TST-",j))

      # Find most probable class, i.e the maximum, and add it to the site
      mostProb <- getMostProbableClass(a_aspt_spr_aut)
      # add the site to the dataframe
      a_aspt_spr_aut <- data.frame(cbind(a_aspt_spr_aut, mostProb))
      SiteProbabilityclasses_spr_aut_comb_aspt <- rbind(SiteProbabilityclasses_spr_aut_comb_aspt, a_aspt_spr_aut)
      # Add the averages of spr
      EQRAverages_aspt_spr_aut <- rbind(EQRAverages_aspt_spr_aut, eqr_av_spr_aspt)
      # bind ASPT EQRs into list dataframe column
      if( store_eqrs == T) {
      eqr <- list(c(multiYear_EQRAverages_aspt_spr_aut))
      ASPT <- rbind(ASPT, eqr)
      }
      ########  Calculate the MINTA -spring aut case  worse class = 1 i.e. min of class from NTAXA and ASPT ######

      # Do the MINTA spr_aut case
      minta_ntaxa_aspt_spr_aut <- getMINTA_ntaxa_aspt(
        as.matrix(classArray_siteOne_spr_aut_ntaxa),
        as.matrix(classArray_siteOne_spr_aut_aspt)
      )
      # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or ntaxa_spr, ntaxa_aut,
      # spr_aut_av_taxa, and spt etc
      minta_probClass_spr_aut <- matrix(0, ncol = 1, nrow = 5)

      for (i in 1:5) {
        minta_probClass_spr_aut[i] <-
          100 * sum(minta_ntaxa_aspt_spr_aut[minta_ntaxa_aspt_spr_aut == i, ] / i) / n_runs
      }

      # probabilityClass <- getProbClassLabelFromEQR()
      aa <- t(minta_probClass_spr_aut) # spr
      colnames(aa) <- getProbClassLabelFromEQR()[, 1]
      rownames(aa) <- as.character(data[j, "SITE"]) # c(paste0("TST-",j))
      # Find most probable MINTA class, i.e the maximum, and add it to the site
      mostProb <- getMostProbableClass(aa)
      aa <- data.frame(cbind(aa, mostProb))
      # Now bind the MINTA proportion to the dataframe
      SiteMINTA_whpt_spr_aut <- rbind(SiteMINTA_whpt_spr_aut, aa) # Error in match.names(clabs, names(xi)) :

      # bind EQRs into list dataframe column
      # eqr <- list(c(EQR_minta_spr))
      # MINTA <- rbind(MINTA, eqr)

      ##### MINTA ENDS HERE  #####

      # Move the pointer k to new adjusted position for j - whether multiple or not
      k <- j + 1
      #print(k)
    } # END of FOR LOOP

    # MINTA outputs
    colnames(SiteMINTA_whpt_spr_aut) <- c(paste0("mintawhpt_spr_aut_", names(SiteMINTA_whpt_spr_aut)))
    # Combine all MINTA

    allMINTA_whpt <- SiteMINTA_whpt_spr_aut

    # DO for NTAXA
    colnames(EQRAverages_ntaxa_spr_aut) <- c(paste0("NTAXA_", colnames(EQRAverages_ntaxa_spr_aut)))
    whpt_ntaxa_spr_aut_averages <- data.frame(NTAXA_aver_spr_aut = rowMeans(EQRAverages_ntaxa_spr_aut))
    # Rename column names so they dont conflict
    colnames(SiteProbabilityclasses_spr_aut_comb_ntaxa) <- paste0(
      colnames(SiteProbabilityclasses_spr_aut_comb_ntaxa),
      "_NTAXA_spr_aut"
    )
    # Colname "mostProb" doesnt appear in SiteProbabilityclasses_spr_ntaxa, so reassign them here again
    # - BUG in ML Studio R version

    # ****** FOR ASPT outputs ********
    colnames(EQRAverages_aspt_spr_aut) <- c(paste0("ASPT_", colnames(EQRAverages_aspt_spr_aut)))
    whpt_aspt_spr_aut_averages <- data.frame(ASPT_aver_spr_aut = rowMeans(EQRAverages_aspt_spr_aut))
    # Rename column names so they dont conflict
    colnames(SiteProbabilityclasses_spr_aut_comb_aspt) <- paste0(
      colnames(SiteProbabilityclasses_spr_aut_comb_aspt),
      "_ASPT_spr_aut"
    )

    ### DO FOR ALL including MINTA
    # Bind the NTAXA
    allResults <- cbind(SiteProbabilityclasses_spr_aut_comb_ntaxa, whpt_ntaxa_spr_aut_averages)
    # Bind the ASPT
    allResults <- cbind(allResults, cbind(SiteProbabilityclasses_spr_aut_comb_aspt, whpt_aspt_spr_aut_averages))
    # Change names of Sites
    namesOfSites <- data.frame(SITE = data[unlist(indicesDistinct), "SITE"])
    # Change waterbody to correct number of sites left after removing duplicate sites, do a collection of indices
    year_waterBody <- year_waterBody[unlist(indicesDistinct), ]

    # Bind waterBody, and namesOfSites
    allResults <- cbind(year_waterBody, allResults)
    allResults <- cbind(namesOfSites, allResults)

    # Bind MINTA
    # Rename columns for MINTA, so they dont conflict
    colnames(SiteMINTA_whpt_spr_aut) <- paste0(colnames(SiteMINTA_whpt_spr_aut), "_MINTA_")
    classification_results <- cbind(allResults, SiteMINTA_whpt_spr_aut)
    # bind stored eqrs
    if (store_eqrs == T) {
    ASPT <- data.frame(ASPT)
    NTAXA <- data.frame(NTAXA)
    #MINTA <-  data.frame(MINTA)
    classification_results <- cbind(classification_result[, c("SITE", "YEAR")],
                                    ASPT, NTAXA)
    # classification_results <- list(classification_results, ASPT, NTAXA)
    }
    return(classification_results)
  }
}
