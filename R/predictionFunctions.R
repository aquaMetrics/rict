#setwd("C:/Users/Maybin/Documents/Rict/codes_JimKing")
 
# getDFScores: Make sure the dimensions DFcoeff (m x n) maps to dimensions of EnvValues (n x k), result will be (m x k)
# Convert DFCoeff and EnvValues to matrix, finalCopy <- as.matrix(final.predictors[,-c(1)]), removing first and last column
# Similarly newDFcoeff <- as.matrix(DFCoeff_gb685[,-1])
# Return finalCopy %*% newDFcoeff

getDFScores  <- function (EnvValues, DFCoeff) {
      Env_i<- as.matrix(EnvValues[,-c(1)])
      Coeff_j <-  as.matrix(DFCoeff[,-1])
      return (Env_i %*% Coeff_j)
}

# Returns the sums of all DFscores per site g
getDFScoresTotal <- function (EnvValues, DFCoeff) {
    return (rowSums(getDFScores(EnvValues, DFCoeff)))
}

# Calculate the Mahanalobis distance of point x from site g

getMahDist <- function (DFscore, meanvalues) {
    mah_Score <- matrix(0, nrow=nrow(DFscore), ncol = nrow(meanvalues) )
    for(row_dfscore in 1:nrow(DFscore)){
      for(row_means in 1:nrow(meanvalues)) {  
        mah_Score[row_dfscore, row_means] <- rowSums((DFscore[row_dfscore,] - meanvalues[row_means,])^2) # apply rowSums() or sum()
      }
    }
    #l_mah_dist <- (meansA-valuesB)^2
    return(mah_Score)
}

# Calculate the minimum Mahanalobis distance of point x from site g
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

# Multiply end-group probabilities with IDXmean, Taxapr, Taxaab, 
# Similar to DFScores() - combine them 

getProbScores  <- function (Proball, IDXMean) {
  Env_i<- as.matrix(Proball)
  Coeff_j <-  as.matrix(IDXMean)
  return (Env_i %*% Coeff_j)
}


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

#Calculate the Probability distribution PDist_g for each site
PDist <- function (nmref_sites, mahdist) {
  return (nmref_sites*exp(-mahdist/2))
}

# Calculate Total probabilities of all sites, bind the row sums to the last column

PDistTotal <- function (distr_g){
  return (cbind(distr_g/rowSums(distr_g),rowSums(distr_g)))
}

#Suitability code - input from getMahDist_min, and suitability codes
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
getProbEndGroup_old <- function (DFCoeff, EnvValues, DFMean, NRef_g) {
  DFScore_d <- data.frame(matrix(0, nrow=nrow(DFCoeff) )) # make a dataframe
  MahDist_g <- data.frame(matrix(0, nrow=nrow(DFCoeff) ))
  PDist_g   <- data.frame(matrix(0, nrow=nrow(DFCoeff) ))
  Prob_g    <-  data.frame(matrix(0, nrow=nrow(DFCoeff) ))
  for(j in 2:nrow(DFCoeff)) {
    DFScore_d [j-1,] <- sum(DFCoeff[,i] * EnvValues[i,-1])
    MahDist_g [j-1,] <- sum((DFScore_d[,i]-DFMean[i,])^2)
  }
  #All should be in loop of end group = g
  MahDist_min <- min(MahDist_g)
  PDist_g     <- NRef_g*exp(-MahDist_g/2)
  PDist_total <-  sum(PDist_g)
  Prob_g      <- PDist_g/PDist_total 
  return (0)
}

#Calculate Mahalabois distance 
getMahDist_old <- function (meansA, valuesB) {
  l_mah_dist <- 0
   for (i in 1: ncol(meansA)) {
       l_mah_dist <- l_mah_dist + (valuesB - meansA)^2;
       l_pDist    <- l_NRef * exp( (-1*l_mah_dist) /2);  
  }
  return (l_mah_dist)
}