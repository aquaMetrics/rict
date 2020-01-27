# Compare the statistical significance of differences between classifications
#
# The Compare function allows the user to assess whether there is a real
# difference in EQR values and/or status class between a pair of samples and/or
# sites and/or time periods

rict_compare <- function(eqr_a = 0.8, eqr_b = 0.6, n = 10000) {
  message("Just demo-ing ASPT for now - not doing NTAXA yet")
  # SD for simulating EQRs taking into accountant sampling error
  sd_obs_aspt <- sdobs_one_year_new(0.269, 0.279, 1)
  #sd_obs_ntaxa <- sd_obs_one_year_new(0.247, 0.211, 1)

  ### Simulate EQRs - not required get sims from classification function?  ----------------
  simulate <- function(eqr, n = 10000, sd_obs_aspt = sd_obs_aspt) {
    # Simulate EQRs
    set.seed(42)
    z_obs <- sd_obs_aspt * stats::rnorm(n, mean = 0, sd = 1) # copied from classification helper funciton
    ObsIDX8rB <- (sqrt(eqr) + z_obs) ^ 2  # copied from classificariton helper function

    ObsIDX8rB[ObsIDX8rB > 1.0] <- 1.0 # rounding for cut function later on
    ObsIDX8rB[ObsIDX8rB < 0] <- 0

    return(ObsIDX8rB)
  }

  ## Simulate EQRs confidence ---------------------------------------------------------
   eqr_a_sim <- simulate(eqr_a, sd_obs_aspt = sd_obs_aspt)
   eqr_b_sim <- simulate(eqr_b, sd_obs_aspt = sd_obs_aspt)

   #diffEQR_one <- eqr_a_sim - eqr_b_sim
  # diffEQR_two <- eqr_a_sim - eqr_b_sim
  # 2 * min(diffEQR_one, diffEQR_two)

  # Get diff EQR values between 97.5% and 2.5% - is this a t.test?
  # Get P value here? Statisical difference between the two simulate datasets?
  # t.test(diff)
  # Find 97.5% and 2.5% percentile values
  # percentiles <- quantile(diff, probs = c(2.5, 97.5) / 100)
  #eqr_percentile <- diff[diff > percentiles[1] & diff < percentiles[2]]

  ### Create class proportion "confusion table"
  # EQR breaks - not needed!! get form classification function???
  eqr_bands <- c(0.0, 0.59, 0.72, 0.86, 0.97, 1.0)
  labels <-  5:1

  # cut eqr - get this from classification function???
  Sample_A <- cut(eqr_a_sim, breaks = eqr_bands, labels, right = F, include.lowest = T)
  Sample_B <- cut(eqr_b_sim, breaks = eqr_bands, labels, right = F, include.lowest = T)
  # table proportion
  Sample_A <- factor(Sample_A, levels = 1:5)
  Sample_B <- factor(Sample_B, levels = 1:5)

  table_a <- table(Sample_A)
  table_b <- table(Sample_B)

  table <- table(Sample_A, Sample_B)
  table_prop <- round(prop.table(table(Sample_A, Sample_B)), 3)

  # Format table
  is.na(table) <- sapply(table, is.infinite)
  table[is.na(table) | table == 0] <- "-"

  return(list(probability = table_prop, count = table))
}