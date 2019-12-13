# Begin Exclude Linting
# Classification functions for RIVPACS III+ for GB

#1. Compute the proportions of table with sumofRows divide by each element in the row
computeScoreProportions  <- function (ScoreDFrame) {
  #ScoreDFrame <- as.matrix(ScoreDFrame)
  for (i in 1:nrow(ScoreDFrame)) {
    ScoreDFrame[i, ] <- ScoreDFrame[i, ] / (rowSums(ScoreDFrame[, ])[i])
  }
  return (as.data.frame(ScoreDFrame))
}

# 2.  Multiply each sites probabilities with these scores, i.e. Pi*Qij, use computeScoreProportions() function
getWeighted_proportion_Rj <- function (allProbabilities, computedScoreProp){ # j = 1 to 5,
    return (as.matrix(allProbabilities) %*% as.matrix(computedScoreProp))
}

# 3. RjAj  - #Multiply Rj by Aj, note each row of Aj is for NTAXA, ASPT, so transpose to multiply by Rj
# This function is the same as function "getAdjustedExpected"

compute_RjAj <- function (Rj, Aj) {
      temp_mult <- as.data.frame(Rj %*% t(Aj))
      colnames(temp_mult) <- c("NTAXA", "ASPT")
      return (temp_mult)
}#

# 3.1 Write a function to simulate N values from a given list, where is the N??? multiply by N*length(DFrameValues[,j])
# This function is not used
simulateN_Values_new <- function (DFrameValues, N, Exp_ref_ntaxa) {
    result <- list()
    dframeFinal <- DFrameValues
    for(j in 1:ncol(DFrameValues)) {
      for(i in 1:N) {
        # DFrameValues[,j] <- sample(DFrameValues[,j], length(DFrameValues[,j]), replace=TRUE)
        #DFrameValues <- cbind(DFramevalues,sample(DFrameValues[,j], length(DFrameValues[,j]), replace=TRUE))
        result[[i]] <- sample(Exp_ref_ntaxa[,j], length(Exp_ref_ntaxa[,j]), replace=TRUE)
        }
      result <- as.data.frame(result)
      #print(result)
      a <- rep(paste0("sim_",colnames(DFrameValues)[j]),ncol(result))
      #print(a)
      b <- paste0(a, 1:length(a))
      #print(b)
      colnames(result) <- b
      dframeFinal <- cbind(dframeFinal,result)
    }

  #
  #Remove the first two unsimulated raw columns -c(1:2), from the input file
  return(dframeFinal[,-c(1:2)])
}

# 3.11
simulateN_Values <- function (DFrameValues, N) {
  result <- list()
  for(j in 1:ncol(DFrameValues)) {
    # DFrameValues[,j] <- sample(DFrameValues[,j], length(DFrameValues[,j]), replace=TRUE)
    result[[j]] <- sample(DFrameValues[,j], length(DFrameValues[,j]), replace=TRUE)
  }
  result <- as.data.frame(result)
  colnames(result) <- renameColsByNumbers (DFrameValues, 1)
  return (result)
}

# 3.2. Write to a file using the pathname
writeToFile <- function (toWriteFrame, pathname, filename) {

  if( file.exists(paste0(file =pathname,filename))) {
    file.remove(file = paste0(pathname,filename))
    utils::write.csv(toWriteFrame, file = paste0(pathname,filename))
  } else {
    utils::write.csv(toWriteFrame, file = paste0(pathname,filename))
  }
}

# 3.3 Write a function to return column names numbered by index position for N ssimulations = N columns by numbers

renameColsByNumbers <- function (dframe, N) {
    a <- rep(paste0("znorm_",colnames(dframe)),N)
    new_colnames <- paste0(a, 1:length(a))
    return (new_colnames)

}

# 5.2 Calculate Ubias8, for one simulation, i.e. Ubias8_8_r_equals_1, = estimated random deviate from a Poisson distribution with mean of ubs_meam = Ubias8
# This is for one sample, r=1
getUbias_8_r_equals_1 <- function (index_data, ubs_mean){
  #
  Ubias8r_whpt <- matrix(0, ncol = ncol(index_data), nrow = nrow(index_data))
  for(j in 1:ncol(Ubias8r_whpt)) {

    Ubias8r_whpt[,j] <- stats::rpois(length(index_data[,j]), ubs_mean) # With a mean of "ubs_mean" and in simulation r
    Ubias8r_whpt[,j] [Ubias8r_whpt[,j] ==0] <- 1 # Replace all ZEROS with ones or use gsub below
    #
    # Ubias8r_whpt[,j] <- as.numeric(gsub("0","1", as.character(Ubias8r_whpt[,j]), ignore.case=T)) # Replace all ZEROS with ones

  }
  return (Ubias8r_whpt)
}

# 5.3 Calculate Ubias8r = estimated random deviate from a Poisson distribution with mean of ubs_meam = Ubias8, for r samples
# N is the number of samples we need, 1<r<n. Store all the simulations in a list
getUbias_8r <- function (index_data, ubs_mean, N){
  allSamples<-list()
  for (j in 1:N){
    oneSample <-  getUbias_8_r_equals_1(index_data, ubs_mean)
    allSamples[[j]] <- oneSample
  }
  return (allSamples)
}


## WITH RALPH

# Sec 6.3.2.1

# New by Ralph
# ZObsir = = ZNormir * SDObsi # Random deviate for potential average observed value of index i in simulation r

getZObs_r_new <- function (sdobs, N_sim){ # N_sim = no. of simulations
  #set.seed(1234)
  dframe <- as.data.frame(sdobs * stats::rnorm(N_sim, 0, 1)) ## With a mean of 0.0 and SD of 1.0 for index i in simulation r
  return (dframe)
}


# Section 6.3.2.1b
# Calculation of standard deviation for SDObs_i, for single year runs
# 5.8 Get SDObs_i = sqtr((SDRep_i)^2 + (SDTSeas_i)^2)/NObsSeas
# Single-season run

sdobs_one_year_new <- function (SDRep_i, SDTSeas_i, NObs_seas) { # NObs_seas = 1
  return (
    sqrt(   (SDRep_i^2 + SDTSeas_i^2)/NObs_seas)

  )
}

# for ntaxa
getObsIDX8r <- function (ObsIDX8, znorm_ir) {# needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  # Do this and store these 10,000 times. Have a matrix array to store them
  # ZObs8r <- getZObs_ir(znorm_ir, SDObs_ir)
  return (
    (sqrt(ObsIDX8)+znorm_ir)^2)
}

# for aspt, we compute 9r
getObsIDXniner <- function (ObsIDX9, znorm_ir) {
  # needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  # Do this and store these 10,000 times. Have a matrix array to store them
  # ZObs8r <- getZObs_ir(znorm_ir, SDObs_ir)
  return (ObsIDX9 + znorm_ir)
}

# Next Bias correction foir Ubias8
getUbias8r_new <- function (N_sim, Ubias8){ # N_sim = no. of simulations, and  Ubias8 = 1.62
  #set.seed(1234)
  dframe <- as.data.frame(stats::rpois(N_sim, Ubias8)) ## With a mean of 0.0 and SD of 1.0 for index i in simulation r
  return (dframe)
}

# Bias corrected observed = Obs_rb, range/distribution of values we could have observed
# getObsIDX8rB_new <- function (ObsIDX8r_new_array, Ubias8r_array){
#    return (Ubias8r_array+ObsIDX8r_new_array)
# }


# Step 2. Now let us do the uncertainty in the reference Adjusted Expected values = Exp_ref

# use the function getZObs_r_new <- function (sdobs, N_sim), with SDExp_i of that index
# - I need to add the 10 values to each of reference Adj Expected for spr, aut for one site i.e.
# ExpeAdjRef[1,1] +getZObs_r_new (sdexp8_ntaxa, N_runs) for Spring
# - then ExpeAdjRef[1,2] +getZObs_r_new (sdexp8_ntaxa, N_runs) for autumn

# SDEXp8 <- getZObs_r_new (sdobs, N_sim)


# Function to get the classification value for the class for WHPT Abundance
# For fututre indices, we input the index name, and the limits

getClassFromEQR_ntaxa <- function (EQRValue) {
  thisEQR <- EQRValue
  if(is.na(thisEQR))
    return (5) # change AZURE!!!
  if(thisEQR>=0.0 & thisEQR<0.47) { # [0, 0.47)
    return (5) # class = B
  }
  else
    if(thisEQR>=0.47 & thisEQR<0.56){ #[0.47,0.56)
      return (4) # class = P
    }

  else
    if(thisEQR>=0.56 & thisEQR<0.68){ #[0.56,0.68)
      return (3) # class = M
    }#if
  else
    if(thisEQR>=0.68 & thisEQR<0.8){ #[0.68,0.8)
      return (2)  # class = G
    }
  else
    if(thisEQR>=0.8) {# [0.80,10]
      return (1) # class = H
    }
  return (9) # Default if no condition is satisfied

}

# for aspt,
getClassFromEQR_aspt <- function (EQRValue) {
  thisEQR <- EQRValue
  if(is.na(thisEQR))
    return (5)
  if(thisEQR>=0.0 & thisEQR<0.59) { #[0.0, 0.5946)
    return (5) # class = B
  }
  else
    if(thisEQR>=0.59 & thisEQR<0.72){ #[0.5946, 7257)
      return (4) # class = P
    }

  else
    if(thisEQR>=0.72 & thisEQR<0.86){ #[0.7257, 0.8668)
      return (3) # class = M
    }#if
  else
    if(thisEQR>=0.86 & thisEQR<0.97){ #[0.8668, 0.9772)
      return (2)  # class = G
    }
  else
    if(thisEQR>=0.97) { #[0.9772)
      return (1) # class = H
    }
  return (9) # Default if no condition is satisfied

}
# Return the class labels of probability of class i.e. use 1 = H, 2 = G, 3 = M, 4 = P, 5 = B

getProbClassLabelFromEQR <- function () {
  probClassFrame <- data.frame(class = c("H","G","M","P","B"))
  return (probClassFrame)
}


# Find tne most probable class

getMostProbableClass  <- function (dframe){
  return (colnames(dframe)[apply(dframe,1,which.max)])
}

#Find the class array of each site
getClassarray_ntaxa <- function (EQR_ntaxa) {

  # classArray_siteOne <- data.frame(nrow= nrow(EQR_ntaxa_spr))
  # # Classify these for SITE ONE using the EQR just for spring
  # classArray_siteOne <- data.frame(nrow= nrow(EQR_ntaxa))
  # for (i in 1:nrow(EQR_ntaxa)) {
  #      # print(EQR_ntaxa[i,1]) # change AZURE!!!
  #      classArray_siteOne[i,1] <- getClassFromEQR_ntaxa(EQR_ntaxa[i,1])
  # }
  EQR_ntaxa[, 1][is.na(EQR_ntaxa)] <- 5 # really?
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.8] <- 1  # class = H
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.68 & EQR_ntaxa < 0.8] <- 2  # class = G
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.56 & EQR_ntaxa < 0.68] <- 3 # class = M
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.47 & EQR_ntaxa < 0.56] <- 4 # class = P
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.0 & EQR_ntaxa < 0.47] <- 5  # class = B
  EQR_ntaxa[, 1][EQR_ntaxa < 0.0] <- 9 # Default if no condition is satisfied?
  return (EQR_ntaxa)
}

# Find the class array
getClassarray_aspt <- function (EQR_aspt) {
  #classArray_siteOne <- data.frame(nrow= nrow(EQR_ntaxa_spr))
  # Classify these for SITE ONE using the EQR just for spring
  # classArray_siteOne <- data.frame(nrow= nrow(EQR_aspt))
  # for (i in 1:nrow(EQR_aspt)) {
  #   classArray_siteOne[i,1] <- getClassFromEQR_aspt(EQR_aspt[i,1])
  # }
  EQR_aspt[, 1][is.na(EQR_aspt)] <- 5 # really?
  EQR_aspt[, 1][EQR_aspt >= 0.97] <- 1  # class = H
  EQR_aspt[, 1][EQR_aspt >= 0.86 & EQR_aspt < 0.97] <- 2  # class = G
  EQR_aspt[, 1][EQR_aspt >= 0.72 & EQR_aspt < 0.86] <- 3 # class = M
  EQR_aspt[, 1][EQR_aspt >= 0.59 & EQR_aspt < 0.72] <- 4 # class = P
  EQR_aspt[, 1][EQR_aspt >= 0.0 & EQR_aspt < 0.59] <- 5  # class = B
  EQR_aspt[, 1][EQR_aspt < 0.0] <- 9 # Default if no condition is satisfied?
  return (EQR_aspt)
}
#Find the averages of both spr and autum, declare a function to compute this
getAvgEQR_SprAut <- function (EQR_spr, EQR_aut, k, row_name = F) {
    eqr_av_spr <- colMeans(EQR_spr)
    eqr_av_aut <- colMeans(EQR_aut)
    eqr_av <- cbind(eqr_av_spr,eqr_av_aut)
    if (row_name == T) {
     rownames(eqr_av) <- c(paste0("TST-", k))
    }
    return (eqr_av)
}

# 5.4 Calculate Zbias9r - Random number deviate from a standard Normal distribution of mean 0.0 and SD of 1.0
# zbias mean and standard deviation

getZbias_9r <- function(N_sims, zbias_mean, zbias_sd) {
    #set.seed(1234)
    ZNorm_ir_whpt <- stats::rnorm(N_sims, 0, 1)
    return(ZNorm_ir_whpt)
}

#5.5 Calculating Ubias9r -abundance weighted wHPT taxa of the ubias8r, ubias_8r is a list/dframe of n simulations, so loop around

getUbias9r_new <- function (u_9a, u_9b, u_9c, obsIDX_9, N_runs, ubias_8r) {

    ubias_8r[ubias_8r == 0] <- 1
   # if(ubias_8r>0) {
    rnorm_runs <- getZbias_9r(N_runs)
    rep_u9a <- rep(u_9a, N_runs)
    rep_u9b <- rep(u_9b, N_runs)
    rep_obsIDX_9  <- rep(obsIDX_9, N_runs)
    rep_u9c <- rep(u_9c, N_runs)
    return(rep_u9a+rep_u9b*rep_obsIDX_9+rnorm_runs*(rep_u9c/sqrt(ubias_8r)))
}

# getMINTA_ntaxa_aspt() - Function to calculate MINTA - minimum taxa - he worst of spr aut EQR for NTAXA, ASPT
# Each dataframe has 10,000 simulated EQR - ntaxa and aspt

getMINTA_ntaxa_aspt <- function (ntaxa_EQR, aspt_EQR){
  localDFrame <- ntaxa_EQR #as.matrix(0, nrow=nrow(ntaxa_EQR), ncol=1)
  #localDFrame <- as.matrix(0, nrow = nrow(ntaxa_EQR), byrow = TRUE)
  for(i in 1:length(ntaxa_EQR)) {
    localDFrame[i] <- max(ntaxa_EQR[i], aspt_EQR[i])
  }

  return (localDFrame)
}# getMINTA_ntaxa_aspt

getMINTA_ntaxa_aspt_new <- function (ntaxa_val, aspt_val){
  copyDFrame <- ntaxa_val
  for(i in 1:nrow(ntaxa_val)){

    copyDFrame <- max(ntaxa_val[i],aspt_val[i] )
  }
  return (copyDFrame)
}


## ends here WITH RALPH ######


#5.6. getZNorm_ir() - Random number deviation from normal distribution with mean of zero and standard deviation of 1.0
# How many times do you want to simulate these? N tmes


getZNorm_ir <- function (index_data, zmean, zstd){
  #set.seed(1234)
  ZNorm_ir_whpt <- matrix(0, ncol = ncol(index_data), nrow = nrow(index_data))
  for(j in 1:ncol(ZNorm_ir_whpt)) {
    ZNorm_ir_whpt[,j] <- stats::rnorm(index_data[,j], zmean, zstd) # With a mean of 0.0 and SD of 1.0 for index i in simulation r
  }
  return (ZNorm_ir_whpt)
}


# 5.6.1 getZnorm_ir_N () - # Returns ZNorm scores of ntaxa, and aspt, can also compute for "score" if in dataset
# How many times do you want to simulate these? N tmes
# Returns a list of N simulations for all the sites
getZNorm_ir_N <- function (index_data, N, znorm_mean, znorm_std){
  allSamples<-list()
  for (j in 1:N){
    oneSample <-  getZNorm_ir(index_data, znorm_mean, znorm_std)
    allSamples[[j]] <- oneSample
  }

  return (allSamples)

  }


# Section 6.3.2.1b,
# 5.7 Get ZObs_ir from Znorm_ir * SDObs_ir
# Where SDObs_ir are a table of values for each index - WHPT NTAXA SDRep = 0.247
# For abundance weighted WHPT_ntaxa, SDObs_i_year(0.247, 0.211, 1) = 0.3248538
# For abundance weighted WHPt ASPT, SDObs_i_year(0.269, 0.279, 1) = 0.387559

getZObs_ir_old <- function (znorm_ir, SDObs_ir) {
  return (znorm_ir * SDObs_ir)
}

# Section 6.3.2.1b
# Calculation of standard deviation for SDObs_i, for single year runs
# 5.8 Get SDObs_i = sqtr((SDRep_i)^2 + (SDTSeas_i)^2)/NObsSeas
# Single-season run

SDObs_One_year_old <- function (SDRep_i, SDTSeas_i, NObs_seas) { # NObs_seas = 1
  return (
           sqrt(   (SDRep_i^2 + SDTSeas_i^2)/NObs_seas)

    )
}

#For abundance weighted WHPT_ntaxa, SDObs_One_year(0.247, 0.211, 1) = 0.3248538
#For abundance weighted WHPt ASPT, SDObs_One_year(0.269, 0.279, 1) = 0.387559




#5.9 Calculate for multi-year runs :: not MVP
# SDObsi  =  ???(( ( (SDRepi)2  +  (SDTSeasi)2 ) / NObsSeas +  (SDTYeari)2 * (1 - NObsYear / 3)) / NObsYear)

SDObs_myearyear <- function (SDRep_i, SDTSeas_i, SDTYear_i, NObs_seas, NObs_year) { # NObs_seas = 1
  return (
    sqrt( ( ((SDRep_i)^2 + (SDTSeas_i)^2)/NObs_seas +  (SDTYear_i)^2 * (1-NObs_year / 3))/NObs_year)
  )
}

# section 6.3.2.1b,
# 6.1 Get ObsIDX8r = (???(ObsIDX8) + ZObs8r)^2   = rth simulated value for observed weighted WHPT NTAXA , ObsIDX8 is an input value, getZObs_ir = ZObs8r

getObsIDX8rB <- function (ObsIDX8, znorm_ir) {# needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  # Do this and store these 10,000 times. Have a matrix array to store them
  # ZObs8r <- getZObs_ir(znorm_ir, SDObs_ir)
  return ((sqrt(ObsIDX8)+znorm_ir)^2)
}

# 6.2. Get ObsIDX9r = ObsIDX9 + ZObs9r, the rth simulated  for observed weighted WHPT ASPT, ObsIDX9 is user-supplied value
getObsIDX9r <- function (ObsIDX9, znorm_ir) {# needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  #Do this and store these 10,000 times. Have a matrix array to store them
  #ZObs9r <- getZObs_ir(znorm_ir, SDObs_ir)
  return ((ObsIDX9+znorm_ir))
}

# 6.3. Get ObsIDX7r = ObsIDX7r + Ubias7r , the rth simulated  for observed weighted WHPT Score
getObsIDX7rB <- function (ObsIDX7r, Ubias7r) {# needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  #Do this and store these 10,000 times. Have a matrix array to store them
  return ((ObsIDX7r+Ubias7r))
}

#6.4 Get ZExpectedAdjusted are values deviating from a normal distribution * SDExp_i then divide by , add Adjusted Expected,
getZAdjustedExpected <- function (ExpIDXi, ZExpir,SDExpi,NExpyear) {
  expIDX_ir  = ExpIDXi + ZExpir * SDExpi / sqrt(NExpyear)
  return (expIDX_ir)

}

# 6.5 Get tghe Bias corrected using OBsIDX (bias) and ()

getBiasCorrectedEQR <- function (ObsIDX_index, ExpAdjust, K) {

  return (0)
}


#####################################################    not usable functions below ##################################

#2_old. getWeighted_proportion_Rj() - Get the weighted proportion R(j) = Sum[P(i)Q(ij)]
# Q(ij) = Adjustment factors for reference site quality scores (Q1, Q2, Q3, Q4, Q5)

getWeighted_proportion_Rj_old <- function (EndGrp_predictedProbs, AdjFactorSiteScores){ # j = 1 to 5,
  endGrp_probs <- as.matrix(EndGrp_predictedProbs)
  adjSiteScores <- as.matrix(AdjFactorSiteScores)
  weighted_proportion_Rj<- rowSums(endGrp_probs  %*% adjSiteScores) # a 24 by 1 matrix, sum the rows
  return (weighted_proportion_Rj)
}


#2.1_old. getAdjustedExpected() -  Get Adjusted Expected Value E for WHPT ntaxa and ASPT from Aj values 1 to 5 below
# BMWP_AbnW_NTAXA_Adj <- c(0.996, 1.009, 1.000, 0.967, 0.926) # A1,..,A5
# BMWP_AbnW_ASPT_Adj  <- c(1.002, 1.008, 1.000, 0.977, 0.945) # A1,..,A5
# i.e. AdjParameters = A1, A2, A3, A4, A5
# E = Rj * Aj, two values (ntaxa, aspt) for each site needed

getAdjustedExpected_old <- function (AdjParameters, EndGrPredictions) {
  expected_Score <- matrix(0, nrow=nrow(AdjParameters), ncol = nrow(EndGrPredictions) )
  for(row_dfscore in 1:nrow(AdjParameters)){
    for(row_means in 1:nrow(EndGrPredictions)) {
      expected_Score[row_dfscore, row_means] <- sum(AdjParameters[row_dfscore,] * EndGrPredictions[row_means,]) # apply rowSums() or sum()
    }
  }
  return (expected_Score)
}

#4. Standard deviation of WHPT ASPT for under-estimated number M of WHPT_ntaxa input by user
#Similar to above but check with Ralph

calculate_Estimatedbias_whpt_aspt <- function (M, Obs_WHPT_ASPT){
  set.seed(1234)
  SDev_denom <- 2/sqrt(M)
  mean_of_whpt_aspt_missed <-  4.35  +  (0.271*Obs_WHPT_ASPT) # the mean
  return (stats::rnorm(Obs_WHPT_ASPT,mean_of_whpt_aspt_missed,SDev_denom)) # normal distribution, mean of "mean_of_whpt_aspt_missed",and SD of "SDev_denom"
}


#5. EQR conversion factors, Function to calculate the EQR index for the reference sites observed
# WHPTNTAXA = 1.0049, WHPT ASPT = 0.9921

getEQRValue <- function(ObsIDX9rB, Exp_ref_r){
  EQR <- ObsIDX9rB/Exp_ref_r
  #EQR <- ObsIDX9rB[,1:2] / Exp_ref_r
  #try_this <- data.frame()
#  for(i in 2:(ncol(ObsIDX9rB)-1)) {
#    EQR <- cbind(EQR, (ObsIDX9rB[,2*i-1:2*i]/Exp_ref_r) )
#  }
  return (EQR)
}

# Get an index
# Function to calculate the EQR index for the reference sites observed
# ObsIDX9_rB is 24 by ncol(ObsIDX9_rB) *N_runs, loop through the ObsIDX9_rB in pairs of two seasons each, divide by Exp_ref_ntaxa_r
# sequence is 1:2, 3:4, 5:6, 7:8, ...., (2*i-1):2*i, the odd number sequence
getEQRValue_old <- function (ObsValue, RefValue) {
  thisFrame <- data.frame(0, nrow=nrow(ObsValue), ncol = ncol(ObsValue) )
  a <- ObsValue[,1:2]*RefValue
  #print(a)
  for(i in 2:(ncol(ObsValue)-1)) {
    #print(i)
     b <- ObsValue[,(2*i-1):2*i]* RefValue
     a <- cbind(a, b)
     #print(a)
  }
  return(a)

  }

# End Exclude Linting
