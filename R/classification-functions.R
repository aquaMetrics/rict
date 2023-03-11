# Begin Exclude Linting
# Classification functions for RIVPACS III+ for GB

# 1. Compute the proportions of table with sumofRows divide by each element in the row
computeScoreProportions <- function(ScoreDFrame) {
  # ScoreDFrame <- as.matrix(ScoreDFrame)
  for (i in 1:nrow(ScoreDFrame)) {
    ScoreDFrame[i, ] <- ScoreDFrame[i, ] / (rowSums(ScoreDFrame[, ])[i])
  }
  return(as.data.frame(ScoreDFrame))
}

# 2.  Multiply each sites probabilities with these scores, i.e. Pi*Qij, use computeScoreProportions() function
getWeighted_proportion_Rj <- function(allProbabilities, computedScoreProp) { # j = 1 to 5,
  return(as.matrix(allProbabilities) %*% as.matrix(computedScoreProp))
}

# 3. RjAj  - #Multiply Rj by Aj, note each row of Aj is for NTAXA, ASPT, so transpose to multiply by Rj
# This function is the same as function "getAdjustedExpected"

compute_RjAj <- function(Rj, Aj) {
  temp_mult <- as.data.frame(Rj %*% t(Aj))
  colnames(temp_mult) <- c("NTAXA", "ASPT")
  return(temp_mult)
}

## WITH RALPH

# Sec 6.3.2.1

# New by Ralph
# ZObsir = = ZNormir * SDObsi # Random deviate for potential average observed value of index i in simulation r

getZObs_r_new <- function(sdobs, N_sim, seed, set_seed) { # N_sim = no. of simulations
  if(!seed) {
    set.seed(set_seed[2])
  }
  dframe <- as.data.frame(sdobs * stats::rnorm(N_sim, 0, 1)) ## With a mean of 0.0 and SD of 1.0 for index i in simulation r
  return(dframe)
}

# Section 6.3.2.1b
# Calculation of standard deviation for SDObs_i, for single year runs
# 5.8 Get SDObs_i = sqtr((SDRep_i)^2 + (SDTSeas_i)^2)/NObsSeas
# Single-season run

sdobs_one_year_new <- function(SDRep_i, SDTSeas_i, NObs_seas) { # NObs_seas = 1
  return(
    sqrt((SDRep_i^2 + SDTSeas_i^2) / NObs_seas)
  )
}

# for ntaxa
getObsIDX8r <- function(ObsIDX8, znorm_ir) { # needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  # Do this and store these 10,000 times. Have a matrix array to store them
  # ZObs8r <- getZObs_ir(znorm_ir, SDObs_ir)
  return(
    (sqrt(ObsIDX8) + znorm_ir)^2
  )
}

# for aspt, we compute 9r
getObsIDXniner <- function(ObsIDX9, znorm_ir) {
  # needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  # Do this and store these 10,000 times. Have a matrix array to store them
  # ZObs8r <- getZObs_ir(znorm_ir, SDObs_ir)
  return(ObsIDX9 + znorm_ir)
}

# Next Bias correction foir Ubias8
getUbias8r_new <- function(N_sim, Ubias8, seed, set_seed) { # N_sim = no. of simulations, and  Ubias8 = 1.62
  if(!seed) {
    set.seed(set_seed[3])
  }
  dframe <- as.data.frame(stats::rpois(N_sim, Ubias8)) ## With a mean of 0.0 and SD of 1.0 for index i in simulation r
  return(dframe)
}

# Step 2. Now let us do the uncertainty in the reference Adjusted Expected values = Exp_ref
# use the function getZObs_r_new <- function (sdobs, N_sim), with SDExp_i of that index

# Function to get the classification value for the class for WHPT Abundance
# Return the class labels of probability of class i.e. use 1 = H, 2 = G, 3 = M, 4 = P, 5 = B
getProbClassLabelFromEQR <- function(area) {
  if(area == "iom") {
    probClassFrame <- data.frame(class = c("E", "G", "M", "P", "B"))
  } else {
    probClassFrame <- data.frame(class = c("H", "G", "M", "P", "B"))
    }
  return(probClassFrame)
}

# Find tne most probable class
getMostProbableClass <- function(dframe) {
  return(colnames(dframe)[apply(dframe, 1, which.max)])
}

# Find the class array of each site
getClassarray_ntaxa <- function(EQR_ntaxa) {
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.8] <- 1 # class = H
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.68 & EQR_ntaxa < 0.8] <- 2 # class = G
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.56 & EQR_ntaxa < 0.68] <- 3 # class = M
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.47 & EQR_ntaxa < 0.56] <- 4 # class = P
  EQR_ntaxa[, 1][EQR_ntaxa >= 0.0 & EQR_ntaxa < 0.47] <- 5 # class = B
  EQR_ntaxa[, 1][EQR_ntaxa < 0.0] <- 9 # Default if no condition is satisfied?
  EQR_ntaxa[, 1][is.na(EQR_ntaxa)] <- 9 # Default if observation is missing
  return(EQR_ntaxa)
}

# Find the class array
getClassarray_aspt <- function(EQR_aspt) {
  EQR_aspt[, 1][EQR_aspt >= 0.97] <- 1 # class = H
  EQR_aspt[, 1][EQR_aspt >= 0.86 & EQR_aspt < 0.97] <- 2 # class = G
  EQR_aspt[, 1][EQR_aspt >= 0.72 & EQR_aspt < 0.86] <- 3 # class = M
  EQR_aspt[, 1][EQR_aspt >= 0.59 & EQR_aspt < 0.72] <- 4 # class = P
  EQR_aspt[, 1][EQR_aspt >= 0.0 & EQR_aspt < 0.59] <- 5 # class = B
  EQR_aspt[, 1][EQR_aspt < 0.0] <- 9 # Default if no condition is satisfied?
  EQR_aspt[, 1][is.na(EQR_aspt)] <- 9 # Default if observation is missing
  return(EQR_aspt)
}
# Find the averages of both spr and autum, declare a function to compute this
getAvgEQR_SprAut <- function(EQR_spr, EQR_aut, k, row_name = FALSE) {
  eqr_av_spr <- colMeans(EQR_spr)
  eqr_av_aut <- colMeans(EQR_aut)
  eqr_av <- cbind(eqr_av_spr, eqr_av_aut)
  if (row_name == TRUE) {
    rownames(eqr_av) <- c(paste0("TST-", k))
  }
  return(eqr_av)
}

# 5.4 Calculate Zbias9r - Random number deviate from a standard Normal distribution of mean 0.0 and SD of 1.0
# zbias mean and standard deviation
getZbias_9r <- function(N_sims, zbias_mean, zbias_sd, seed, set_seed) {
  if(!seed) {
    set.seed(set_seed[1])
  }
  ZNorm_ir_whpt <- stats::rnorm(N_sims, 0, 1)
  return(ZNorm_ir_whpt)
}

# 5.5 Calculating Ubias9r -abundance weighted wHPT taxa of the ubias8r, ubias_8r is a list/dframe of n simulations, so loop around
getUbias9r_new <- function(u_9a, u_9b, u_9c, obsIDX_9, N_runs, ubias_8r, seed, set_seed) {
  ubias_8r[ubias_8r == 0] <- 1
  # if(ubias_8r>0) {
  rnorm_runs <- getZbias_9r(N_runs, seed = seed, set_seed = set_seed)
  rep_u9a <- rep(u_9a, N_runs)
  rep_u9b <- rep(u_9b, N_runs)
  rep_obsIDX_9 <- rep(obsIDX_9, N_runs)
  rep_u9c <- rep(u_9c, N_runs)
  return(rep_u9a + rep_u9b * rep_obsIDX_9 + rnorm_runs * (rep_u9c / sqrt(ubias_8r)))
}

# getMINTA_ntaxa_aspt() - Function to calculate MINTA - minimum taxa - he worst of spr aut EQR for NTAXA, ASPT
# Each dataframe has 10,000 simulated EQR - ntaxa and aspt
getMINTA_ntaxa_aspt <- function(ntaxa_EQR, aspt_EQR) {
  localDFrame <- ntaxa_EQR # as.matrix(0, nrow=nrow(ntaxa_EQR), ncol=1)
  # localDFrame <- as.matrix(0, nrow = nrow(ntaxa_EQR), byrow = TRUE)
  for (i in 1:length(ntaxa_EQR)) {
    localDFrame[i] <- max(ntaxa_EQR[i], aspt_EQR[i])
  }

  return(localDFrame)
} # getMINTA_ntaxa_aspt

# section 6.3.2.1b,
# 6.1 Get ObsIDX8r = (???(ObsIDX8) + ZObs8r)^2   = rth simulated value for observed weighted WHPT NTAXA , ObsIDX8 is an input value, getZObs_ir = ZObs8r

getObsIDX8rB <- function(ObsIDX8, znorm_ir) { # needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  # Do this and store these 10,000 times. Have a matrix array to store them
  # ZObs8r <- getZObs_ir(znorm_ir, SDObs_ir)
  return((sqrt(ObsIDX8) + znorm_ir)^2)
}

# 6.2. Get ObsIDX9r = ObsIDX9 + ZObs9r, the rth simulated  for observed weighted WHPT ASPT, ObsIDX9 is user-supplied value
getObsIDX9r <- function(ObsIDX9, znorm_ir) { # needs 10,000 simulations, and store these for each index(ntaxa, aspt) for each season (autumn, spring)
  # Do this and store these 10,000 times. Have a matrix array to store them
  # ZObs9r <- getZObs_ir(znorm_ir, SDObs_ir)
  return((ObsIDX9 + znorm_ir))
}

# End Exclude Linting
