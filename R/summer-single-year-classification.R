#--- ###########################################################
#--- title: "RICT IV WHPT SUMMER ONLY CLASSIFICATION version 2"#
#--- output: html_document                                     #
#--- Author: Dr K. M Muyeba aka Maybin                         #
#--- ###########################################################

# ################################################################################################################
# This Script reads WHPT prediction indices for classification, and works with RICT_GB_summerOnlyPredictionr.Rmd #
# We only use column "SITE", p1-p43, SuitCode", "SuitText", "BelongsTo_endGrp", "TL2_WHPT_NTAXA_AbW_DistFam_spr",#
# "TL2_WHPT_ASPT_AbW_DistFam_spr"  "TL2_WHPT_NTAXA_AbW_DistFam_aut" "TL2_WHPT_ASPT_AbW_DistFam_aut"              #
# ################################################################################################################


#Part 1: This Script reads all prediction indices for classification
# Map 1-based optional input ports to variables
summer_single_year_classification <- function(predictions, store_eqrs = FALSE, area = NULL, seasons = 4) {

  set.seed(1234)
  # Part 1: This Script reads all prediction indices for classification
  GB685_Ass_score  <- utils::read.csv(system.file("extdat", "end-grp-assess-scores.csv", package = "rict"))
  Aj  <- utils::read.csv(system.file("extdat", "adjust-params-ntaxa-aspt.csv", package = "rict"))

  if (area == "ni") {
    GB685_Ass_score  <- utils::read.csv(system.file("extdat", "EndGrp_AssessScoresNI.csv", package = "rict"))
  }
#Enter source files
set.seed (1234) #(2345)
# **** DEAL wiTH ALL INPUTS *****

# Remove the raw_data log inputs, including the counting column 1
# print(names(predictions))

# Choose all classification biological variables
namesBiological <-    c("SPR_SEASON_ID", "SPR_TL2_WHPT_ASPT (ABW,DISTFAM)",
                        "SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SPR_NTAXA_BIAS",
                        "SUM_SEASON_ID", "SUM_TL2_WHPT_ASPT (ABW,DISTFAM)",
                        "SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SUM_NTAXA_BIAS",
                        "AUT_SEASON_ID", "AUT_TL2_WHPT_ASPT (ABW,DISTFAM)",
                        "AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)","AUT_NTAXA_BIAS")


# Choose all classification biological variables - for SUMMER
# namesBiological <-    c("SUM_SEASON_ID", "SUM_TL2_WHPT_ASPT..ABW.DISTFAM.","SUM_TL2_WHPT_NTAXA..ABW.DISTFAM.", "SUM_NTAXA_BIAS")
biologicalData <- predictions[,  namesBiological] # c(56:67)] # change AZURE

# print(head(biologicalData,3))

# Keep YEAR, WATERBODY
year_waterBody <- predictions[,c("YEAR","WATERBODY")]
# print(head(year_waterBody,5))
# Keep predictions data only
# predictions <- predictions[,c(1:55)] # Change AZURE

# Extract Ubias8 from Biological data #
UBIAS_main <- biologicalData[,"SUM_NTAXA_BIAS"][1] # Put new AZURE. Use the default as 1.62. "SPR_NTAXA_BIAS"][1]
# print(head(UBIAS_main))
# Put the UBIAS_main default value of 1.68 if the user does not enter any value or entersa -9
if(is.na(UBIAS_main) | UBIAS_main==-9) { # For NI model, the default is ZERO
  UBIAS_main <- 1.68
}

# OBSERVED NTAXA SUMMER
Obs_ntaxa_sum    <- biologicalData[,"SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)"] # change AZURE
# print(head(Obs_ntaxa_sum,2))

# OBSERVED NTAXA

# Obs_ntaxa_spr    <- biologicalData[,"SPR_TL2_WHPT_NTAXA..ABW.DISTFAM."] # change AZURE
# Obs_ntaxa_aut    <- biologicalData[,"AUT_TL2_WHPT_NTAXA..ABW.DISTFAM."] # change AZURE

# OBSERVED ASPT SUMMER
Obs_aspt_sum   <- biologicalData[,"SUM_TL2_WHPT_ASPT (ABW,DISTFAM)"] # change AZURE
# OBSERVED ASPT
# print(head(Obs_aspt_sum,2))
# Obs_aspt_spr   <- biologicalData[,"SPR_TL2_WHPT_ASPT..ABW.DISTFAM."] # change AZURE
# Obs_aspt_aut   <- biologicalData[,"AUT_TL2_WHPT_ASPT..ABW.DISTFAM."] # change AZURE


# Keep all predictions data

# predictions <- predictions[,c(1:55)] # change AZURE

# Remove the "_CompFarm_" columns
predictions <- select(predictions, -matches("_CompFam_") ) # use the "-" with "match" from dplyr

# Also remove biological data, # Put AZURE

predictions <- predictions[,!names(predictions) %in% namesBiological  ]

# Store allProbabilities in one dataframe. Use p1,p2,... etc in case data column positions change in future

probNames <- c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16","p17","p18","p19","p20","p21","p22","p23","p24","p25","p26","p27","p28","p29","p30","p31","p32","p33","p34","p35","p36","p37","p38","p39","p40","p41","p42","p43")

allProbabilities <- predictions[, probNames]
# print(names(allProbabilities))
# print(head(allProbabilities,7))
# Compute Qij
Qij <- computeScoreProportions(GB685_Ass_score[,-1]) # Remove the first Column


# Compute Rj = sum(Pi*Qij)
Rj <- as.matrix(getWeighted_proportion_Rj(allProbabilities, Qij))# We should havew five of these

# head(Rj)

# Multiply Rj by Aj, note each row of Aj is for NTAXA, ASPT, so transpose to multiply by Rj

RjAj <- compute_RjAj(Rj, Aj)

One_over_RjAj <- 1/RjAj

#
# writeToFile(One_over_RjAj, path, "/Results/1_over_RjAj.csv")

# Write a function that computes aspt, ntaxa adjusted (1 = "NTAXA", 2="ASPT") or select them by name as declared in the classification functions
ntaxa_Adjusted <- select(predictions, matches("_NTAXA_")) / RjAj[,"NTAXA"]
aspt_Adjusted  <- select(predictions, matches("_ASPT_")) / RjAj[,"ASPT"] #Compute AdjExpected as E=predictions/Sum(Rj*Aj)
# All ok

Adjusted_Expected <- cbind(ntaxa_Adjusted, aspt_Adjusted)
Adjusted_Expected_new <- cbind(as.data.frame(predictions[,1]), Adjusted_Expected) # Include site names from predictions

# ******* FOR ASPT ************
Exp_ref_aspt  <- aspt_Adjusted/0.9921

Ubias8 <- UBIAS_main

# run simulations from here
N_runs <- 10000 # 10000

# find the non-bias corrected  EQR = Obs/ExpRef
# nonBiasCorrected_WHPT_aspt_spr <- Obs_aspt_spr/select(Exp_ref_aspt, matches("_spr"))
# nonBiasCorrected_WHPT_aspt_aut <- Obs_aspt_aut/select(Exp_ref_aspt, matches("_aut"))
nonBiasCorrected_WHPT_aspt_sum   <- Obs_aspt_sum/select(Exp_ref_aspt, matches("_sum"))
print("nonBiasCorrected_WHPT_aspt_sum")
head(nonBiasCorrected_WHPT_aspt_sum)

# Now do the Obs_rb with ONE SITE Obs_aspt_spr[1]
sdobs_aspt <- sdobs_one_year_new(0.269, 0.279, 1)

SiteProbabilityclasses_spr_aspt <- data.frame() # Store site probabilities in a dataframe
SiteProbabilityclasses_aut_aspt <- data.frame() # Store site probabilities in a dataframe
SiteProbabilityclasses_spr_aut_comb_aspt <- data.frame()

# summer
SiteProbabilityclasses_sum_aspt <- data.frame() # Store site probabilities in a dataframe

EQRAverages_aspt_spr <- data.frame() # Store average EQRs for spr in a dataframe
EQRAverages_aspt_aut <- data.frame() # Store average EQRs for spr in a dataframe

#Summer
EQRAverages_aspt_sum <- data.frame() # Store average EQRs for spr in a dataframe


# ******** FOr NTAXA ***************
Exp_ref_ntaxa <- ntaxa_Adjusted/1.0049 # select(Adjusted_Expected_new, matches("_NTAXA_"))/1.0049



# summer
nonBiasCorrected_WHPT_ntaxa_sum <- Obs_ntaxa_sum/select(Exp_ref_ntaxa, matches("_sum"))

# Now do the Obs_rb with ONE SITE Obs_ntaxa_spr[1]
sdobs_ntaxa <- sdobs_one_year_new(0.247, 0.211, 1)

SiteProbabilityclasses_spr_ntaxa <- data.frame() # Store site probabilities in a dataframe
SiteProbabilityclasses_aut_ntaxa <- data.frame() # Store site probabilities in a dataframe
SiteProbabilityclasses_spr_aut_comb_ntaxa <- data.frame()
SiteProbabilityclasses_sum_sum_comb_ntaxa <- data.frame()

# Summer
SiteProbabilityclasses_sum_ntaxa <- data.frame() # Store site probabilities in a dataframe

SiteMINTA_whpt_spr <- data.frame()
SiteMINTA_whpt_aut <- data.frame()
SiteMINTA_whpt_spr_aut <- data.frame()

# Summer
SiteMINTA_whpt_sum <- data.frame()


# Summer
EQRAverages_ntaxa_sum <- data.frame()  # Store average EQRs for spr in a datafram

Ubias8r_spr <-  getUbias8r_new (N_runs, Ubias8)
Ubias8r_aut <-  getUbias8r_new (N_runs, Ubias8)

# Summer
Ubias8r_sum <-  getUbias8r_new (N_runs, Ubias8)
#k<- 1
#
for (k in 1:nrow(predictions)) {
  #for (k in 1:5) {
  # LOOP all the sites from here
  # Part 1. Adjust the Observed values
  # Loop starts from here with site = k, i.e. sqr (sqrt(Obs) + ZObs) + Ubias8r


  #Summer
  ObsIDX8r_sum  <- getObsIDX8r(Obs_ntaxa_sum[k],getZObs_r_new(sdobs_ntaxa,N_runs)) # Obs_ntaxa_spr[k] used instead of Obs_ntaxa_sum[k] ****** !!!!

  #Summer
  Obs_site1_ntaxa_sum <- ObsIDX8r_sum + Ubias8r_sum # rename "Obs_site1_ntaxa_aut" to ObsIDX8rb_aut

  # Part 2 . Do the RefAdjExpected bias

  sdexp8_ntaxa <- 0.53 # For aspt we use a different valsue

  # Summer
  ExpIDX8r_ntaxa_sum <- data.frame(val = (Exp_ref_ntaxa[k,1]+ getZObs_r_new (sdexp8_ntaxa, N_runs)))


  #Summer
  EQR_ntaxa_sum <- as.data.frame(Obs_site1_ntaxa_sum/ExpIDX8r_ntaxa_sum[,1] )


  # Part 1: for "Spring" - DO FOR NTAXA

  #Find the averages of both spr and autum, declare a function to compute this
  #

  #Summer
  eqr_av_sum  <- getAvgEQR_SprAut (EQR_ntaxa_sum,EQR_ntaxa_sum ) # CHECK this mean function !!!!!

  #change to 1 value. Function "getAvgEQR_SprAut" is meant to compute for spr, aut
  a <-data.frame(eqr_av_sum=eqr_av_sum[,1])
  rownames(a) <- rownames(eqr_av_sum)
  eqr_av_sum <- a

  #print(eqr_av_spr)


  # Classify these for each SITE using the EQR just for spring and autumn

  #Summer
  classArray_siteOne_sum_ntaxa <- getClassarray_ntaxa(EQR_ntaxa_sum)

  # define an array to hold probability of class for each site- how much of the site belongs to each classes, adds up to 100%
  # NEXT!!


  #summer
  probClass_sum <- matrix(0, ncol = 1, nrow = 5)


  for(i in 1:5) {
    # probClass_spr[i] <- 100*sum(classArray_siteOne_spr_ntaxa[classArray_siteOne_spr_ntaxa==i,]/i)/N_runs
    # probClass_aut[i] <- 100*sum(classArray_siteOne_aut_ntaxa[classArray_siteOne_aut_ntaxa==i,]/i)/N_runs

    #Summer
    probClass_sum[i] <- 100*sum(classArray_siteOne_sum_ntaxa[classArray_siteOne_sum_ntaxa==i,]/i)/N_runs
  }

  # Summer
  probabilityClass <- getProbClassLabelFromEQR()
  a_ntaxa_sum <- t(probClass_sum) # spr, need a_ntaxa_spr
  colnames(a_ntaxa_sum) <- getProbClassLabelFromEQR()[,1]
  rownames(a_ntaxa_sum) <- c(paste0("TST-",k))


  #Find most probable class, i.e the maximum, and add it to the site

  #Summer
  mostProb <- getMostProbableClass(a_ntaxa_sum)
  a_ntaxa_sum <- cbind(a_ntaxa_sum, mostProb) # add the site to the dataframe
  SiteProbabilityclasses_sum_ntaxa<- rbind(SiteProbabilityclasses_sum_ntaxa,a_ntaxa_sum)

  #Add the averages of spr,aut

  #Summer NTAXA. Do similar one for ASPT
  EQRAverages_ntaxa_sum <- rbind(EQRAverages_ntaxa_sum, eqr_av_sum)

  # Part 3:: Do combined spr, aut processing
  #First find the row averages of all the 10,000 simulations
  # rowAverage_spr_aut  <-data.frame(rowMeans(cbind(EQR_ntaxa_spr, EQR_ntaxa_aut)))

  #Summer: NO NEED
  #First find the row averages of all the 10,000 simulations
  rowAverage_sum_sum  <- data.frame(rowMeans(cbind(EQR_ntaxa_sum, EQR_ntaxa_sum)))

  # NTAXA is done up here for SUMMER, including averages

  # REST of codes for combining



  # **** Workout FOR ASPT STARTS HERE

  ## RALPH
  u_9a <- 4.35
  u_9b <- 0.271
  u_9c <- 2.5

  #### RALPH

  #Summer
  Ubias9r_sum <- getUbias9r_new (u_9a, u_9b, u_9c,Obs_aspt_sum[k], N_runs, Ubias8r_sum)


  #Summer
  Ubias7r_sum <- Ubias8r_sum*Ubias9r_sum


  #summer
  ObsIDX9r_sum  <- getObsIDX9r (Obs_aspt_sum[k],getZObs_r_new(sdobs_aspt,N_runs))


  #Summer
  ObsIDX7r_sum <-  ObsIDX8r_sum* ObsIDX9r_sum

  #Summer
  ObsIDX7rb_sum <- ObsIDX7r_sum+Ubias7r_sum
  # Summer
  ObsIDX8rb_sum <- ObsIDX8r_sum+Ubias8r_sum

  # summer
  ObsIDX9rb_sum <- ObsIDX7rb_sum/ObsIDX8rb_sum

  # Part 2 . Do the RefAdjExpected bias

  # Expected reference adjusted , as an array , ONE SITE, site 14

  sdexp9_aspt <- 0.081 # For aspt we use a different value, 0.081

  ExpIDX9r_aspt_sum <- data.frame(val = (Exp_ref_aspt[k,1]+ getZObs_r_new (sdexp9_aspt, N_runs)))

  # Calculating simulated EQR

  # Summer
  EQR_aspt_sum <- as.data.frame(ObsIDX9rb_sum/ExpIDX9r_aspt_sum[,1])

  # Part 1: for "Spring"

  #Find the averages of both spr and autum, declare a function to compute this
  #


  # Summer
  eqr_av_sum_aspt  <- getAvgEQR_SprAut (EQR_aspt_sum,EQR_aspt_sum ) #
  #print(eqr_av_spr)

  #change to 1 value. Function "getAvgEQR_SprAut" is meant to compute for spr, aut
  a <-data.frame(eqr_av_sum_aspt=eqr_av_sum_aspt[,1])
  rownames(a) <- rownames(eqr_av_sum_aspt)
  eqr_av_sum_aspt <- a

  #eqr_av_aut  <- getAvgEQR_SprAut (EQR_ntaxa_spr,EQR_ntaxa_aut )

  # Classify these for each SITE using the EQR just for spring
  # classArray_siteOne_spr_aspt <- getClassarray_aspt(EQR_aspt_spr)
  #  classArray_siteOne_aut_aspt <- getClassarray_aspt(EQR_aspt_aut)
  # Summer
  classArray_siteOne_sum_aspt <- getClassarray_aspt(EQR_aspt_sum)

  # define an array to hold probability of class for each site- how much of the site belongs to each classes, adds up to 100%


  # Summer
  probClass_sum <- matrix(0, ncol = 1, nrow = 5) # 5 is the number of classes- H, G, M, B, P, ncol=1 or 2 for two seasons or ntaxa_spr, ntaxa_aut, spr_aut_av_taxa, and spt etc

  for(i in 1:5) {
    #probClass_spr[i] <- 100*sum(classArray_siteOne_spr_aspt[classArray_siteOne_spr_aspt==i,]/i)/N_runs
    #probClass_aut[i] <- 100*sum(classArray_siteOne_aut_aspt[classArray_siteOne_aut_aspt==i,]/i)/N_runs
    #summer
    probClass_sum[i] <- 100*sum(classArray_siteOne_sum_aspt[classArray_siteOne_sum_aspt==i,]/i)/N_runs
  }

  # Work out ASPT probability of classes

  # Summer
  a_aspt_sum <- t(probClass_sum) # spr
  colnames(a_aspt_sum) <- getProbClassLabelFromEQR()[,1]
  rownames(a_aspt_sum) <- c(paste0("TST-",k))

  # Summer
  #Find most probable class, i.e the maximum, and add it to the site
  mostProb <- getMostProbableClass(a_aspt_sum)
  # add the site to the dataframe
  a_aspt_sum <- cbind(a_aspt_sum, mostProb)

  # Prob class
  # Summer

  SiteProbabilityclasses_sum_aspt<- rbind(SiteProbabilityclasses_sum_aspt,a_aspt_sum)
  #Add# the averages of sum for aspt
  EQRAverages_aspt_sum <- rbind(EQRAverages_aspt_sum, eqr_av_sum_aspt)


  ########  Calculate the MINTA -spring case  worse class = 1 i.e. min of class from NTAXA and ASPT ######
  # Summer
  matrix_ntaxa_sum <- as.matrix(classArray_siteOne_sum_ntaxa)
  matrix_aspt_sum <- as.matrix(classArray_siteOne_sum_aspt)
  #minta_ntaxa_aspt_spr <- getMINTA_ntaxa_aspt (matrix_ntaxa_spr, matrix_aspt_spr)

  #Summer
  minta_ntaxa_aspt_sum <- getMINTA_ntaxa_aspt (as.matrix(classArray_siteOne_sum_ntaxa),
                                               as.matrix(classArray_siteOne_sum_aspt))

  # Now calculate proportion of each class H to B for MINTA
  #
  #     minta_probClass_spr <- matrix(0, ncol = 1, nrow = 5) # 5 is the number
  #of classes-H,G,M, B, P, ncol=1 or 2 for two seasons or ntaxa_spr,ntaxa_aut, spr_aut_av_taxa, and spt etc
  # Summer
  minta_probClass_sum <- matrix(0, ncol = 1, nrow = 5)

  for(i in 1:5) {
    minta_probClass_sum[i] <- 100*sum(minta_ntaxa_aspt_sum[minta_ntaxa_aspt_sum==i,]/i)/N_runs
  }

  # probabilityClass <- getProbClassLabelFromEQR()
  aa <- t(minta_probClass_sum) # spr
  colnames(aa) <- getProbClassLabelFromEQR()[,1]
  rownames(aa) <- as.character(predictions[k,"SITE"]) #c(paste0("TST-",k))
  # Find most probable MINTA class, i.e the maximum, and add it to the site
  mostProb <- getMostProbableClass(aa)
  aa <- cbind(aa, mostProb)
  # Now bind the MINTA proportion to the dataframe
  SiteMINTA_whpt_sum <- rbind(SiteMINTA_whpt_sum, aa)   # ## STORE , APPEND THIS

  ##### MINTA ENDS HERE  #############

}# END of FOR LOOP

# **** FOR NTAXA outputs **********

# Summer
colnames(EQRAverages_ntaxa_sum) <- c(paste0("NTAXA_",colnames(EQRAverages_ntaxa_sum)))

# Summer NO NEEd ,its the same as  "EQRAverages_ntaxa_sum"
whpt_ntaxa_sum_sum_averages <- data.frame(NTAXA_aver_sum_sum=rowMeans(EQRAverages_ntaxa_sum))
#Change row names
rownames(whpt_ntaxa_sum_sum_averages) <- predictions[,"SITE"]

# Rename column names so they dont conflict
# Summer
colnames(SiteProbabilityclasses_sum_ntaxa) <- paste0(colnames(SiteProbabilityclasses_sum_ntaxa), "_NTAXA_sum")

# Summer
averages_sum_ntaxa <- cbind(EQRAverages_ntaxa_sum[1],SiteProbabilityclasses_sum_ntaxa) #

## NEXT!! #Change row names
# Summer
rownames(averages_sum_ntaxa) <- predictions[,"SITE"] ## predictions[,"SITE"] [1]


allProbClasses_ave_ntaxa <- averages_sum_ntaxa
#allResults <- cbind(year_waterBody,allProbClasses_ave_ntaxa)   ## cbind(year_waterBody[1,],allProbClasses_ave_ntaxa)
allResults <- allProbClasses_ave_ntaxa

# change column names for MINTA in SiteMINTA_whpt_sum
colnames(SiteMINTA_whpt_sum) <- paste0(colnames(SiteMINTA_whpt_sum),"_MINTA")

# writeToFile(SiteProbabilityclasses_spr, path, "/SiteProbabilityclasses_spr.csv")
# writeToFile(EQRAverages_ntaxa_sum, path, "/Results/EQRAverages_ntaxa_sum.csv")
# writeToFile(allResults, path, "/Results/whpt_ntaxa_allResults.csv")
all_minta <- cbind(year_waterBody, SiteMINTA_whpt_sum) ## cbind(year_waterBody[1,],SiteMINTA_whpt_sum )
# writeToFile(all_minta, path,"/Results/ALL_whpt_MINTA.csv")
allResults <- cbind(allResults,SiteMINTA_whpt_sum)
#writeToFile(allResults ,path,"/Results/ALL_whpt_NTAXA_MINTA.csv")
## uphere deal with column names for all_minta,and rownames rom predictions[,"SITE"]

# Start for aspt
# ****** FOr ASPT outputs ********

# Find the averages of these across seasons aver#(spr, aut)
#summer
#colnames(EQRAverages_aspt_sum) <- c(paste0("ASPT_",colnames(EQRAverages_aspt_sum)))

# Rename column names so they dont conflict
# Summer
colnames(SiteProbabilityclasses_sum_aspt) <- paste0(colnames(SiteProbabilityclasses_sum_aspt), "_ASPT_sum")


# Summer
averages_sum_aspt <- cbind(EQRAverages_aspt_sum[1],SiteProbabilityclasses_sum_aspt)

allResults_aspt <- averages_sum_aspt
rownames(allResults_aspt) <- predictions[,"SITE"]    ##predictions[,"SITE"][1]

# Add waterbody, and YEAR
allResults_aspt <- cbind(year_waterBody,averages_sum_aspt)
rownames(allResults_aspt) <- NULL
site <- data.frame(SITE = predictions[,"SITE"])
#
#Add a column of SITES
allResults_aspt <- cbind(site, allResults_aspt)
print(head(allResults_aspt,5))


#Write all Results
all_summer<- cbind(allResults_aspt,allResults)

return(all_summer)

}