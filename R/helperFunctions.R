require(rnrfa)

 
# Helper functions for RIVPACS III+ for GB

# 1. getLogAlkalinity
# This function gets Alkalinity from hardness, calcium, conduct when supplied, else return the alkalinity rovided as log10
# Returns log10 of the raw value. Use mapply as a "m"ultivariate version of sapply, applies to each element
# Can use "switch"" for better readability. 
# Produce dataframe with value of alkalinity in bounds [1.2, 366] as warning, and a message "Succ" or "Fail" to remove record from dataset for prediction

getLogAlkalinity <- function (hardness, calcium, conduct, alkal, lower_b, upper_b) {
  #lower_b = 1.2, upper_b = 366, i.e. the log10 boundary values
    msg <- "Succ"
    ALK <- alkal
    # Case none provided
    if(is.null(hardness) & is.null(calcium) & is.null(conduct) & is.null(ALK)) { #MVP but.. or is.na(hardness) & is.na(calcium) etc etc
      msg <- "Fail: one of hard, Calc, cond, alk is null, bounds exceeded"
      alk_frame <- data.frame(log10(0), msg)
      colnames(alk_frame) <- c("log","msg")
      return (alk_frame)
    }#
    #Case provided, start with calculations
    if(!is.null(hardness) & (!is.na(hardness))) { #' dont use is.na()
      ALK = 4.677 + 0.6393*hardness
      print(!is.na(hardness))
      print("hardness not null")
    }
    else
      if(!is.null(calcium) & (!is.na(calcium))) { 
        ALK = 14.552 + 1.7606*calcium 
        print("calcium not null")
      }
    else 
      if(!is.null(conduct) & (!is.na(conduct))) {
        ALK = 0.3201*conduct - 8.0593 
        print("Conduct not null")
      }
    
  print(ALK)
  if(!is.null(ALK) & (!is.na(ALK))){
     ALK.LOG <- log10(ALK)
     
     #First check Fail conditions  Case 2
     if( (  ALK<lower_b)| (ALK>upper_b) ) { # Completely out of bounds, should fail with null value for "alt"
       msg <- "Fail: ALKALINITY bounds exceeded"
       alk_frame <- data.frame(ALK.LOG, msg) # A null value means nothing or zero
       colnames(alk_frame) <- c("log","msg")
       return (alk_frame)
     }
     # Do the normal things
     if(ALK>=lower_b & ALK<=upper_b) {
       if(ALK==lower_b | ALK==upper_b){
         msg <- "Warn: ALKALINITY bounds reached"
         alk_frame <- data.frame(ALK.LOG, msg)
         colnames(alk_frame) <- c("log","msg") # past0 to "log" for name of attribute/parameter
         return (alk_frame) # Return both message and log value
       }
       else {
         alk_frame <- data.frame(ALK.LOG, msg)
         colnames(alk_frame) <- c("log","msg")
         return(alk_frame)
       }
     }# endif
  }
}
#OLD: Test in next block
#Usage: Apply the mapply multivariate function
    # copy_data$ALKALINITY <- with(copy_data, mapply(getAlkalinity, HARDNESS, CALCIUM, CONDUCTIVITY, ALKALINITY))
    # head(copy_data$ALKALINITY,length(copy_data$ALKALINITY))
 # 2.The function getValidEnvInput() returns a dataframe of log10 value and a message, given limit bound [lower_b, upper_b]
# This function is generic to these input environment variables: 
    # MEAN_WIDTH, # MEAN_DEPTH, # SLOPE, # DIST_FROM_SOURCE, # DISCHARGE
#  Use "New GB (43 Group level)
#  Check if it is "Fail" or "Warn", substr(getValidEnvInput(0)[2][[1]], 1,4)

getValidEnvInput <- function(env_var, lower_b, upper_b, envName) {
  # lower_b <- 1
  # upper_b <- 590
  msg <- "Succ"
  #First check Fail conditions  Case 1
  if(is.null(env_var) | lower_b<0 | is.null(upper_b) ) { #Should fail with null value for "alt"
    msg <- paste("Fail: ",envName," bounds exceeded with null values")
    env_frame <- data.frame(-999, msg) # A null value means nothing or -999
    colnames(env_frame) <- c("log","msg")
    return (env_frame)
  }
  
  #First check Fail conditions  Case 2
  if( (0<env_var & env_var<lower_b)| (env_var>upper_b) ) { # Completely out of bounds, should fail 
    msg <- paste("Fail: ",envName," bounds exceeded")
    env_frame <- data.frame(log10(env_var), msg) # A null value means nothing or zero
    colnames(env_frame) <- c("log","msg")
    return (env_frame)
  }
  # Do the normal things
  if(env_var>=lower_b & env_var<=upper_b) {
      if(env_var==lower_b | env_var==upper_b){
        msg <- paste("Warn: ",envName," bounds reached")
        env_frame <- data.frame(log10(env_var), msg)
        colnames(env_frame) <- c("log","msg") # past0 to "log" for name of attribute/parameter
        return (env_frame) # Return both message and log value
      }
      else {
            env_frame <- data.frame(log10(env_var), msg)
            colnames(env_frame) <- c("log","msg")
            return(env_frame)
      }
  }# endif
}# end getValidEnvInput 

# 3. Function getAltitude(), change to base 10, Use "New GB (43 Group level)
# The bounds are different: Warn [1,590], Fail [0,1345]. Value is less than 0 or greater than 1345, in which case "Fail".
# Value <1 or value >590, in whihc case "Warn"
# if value is 0-1 or 590-1345, "Warn".
# CASE 1: 0<x<1 or 590<x<1345, msg = "Warn"
# CASE 2: 590<x<1345, msg = "Fail"
# CASE 3: else msg = "Succ"

getAltitude <- function(alt, lower_b, upper_b) {
  lower_lowb <- 0
  upper_highb <-1345
  msg <- "Succ"
  if(is.null(alt) | is.na(alt)) { # Null cant be evaluated, return log10(0)
    msg <- "Fail: Altitude bounds exceeded with null values"
    alt_frame <- data.frame(log10(0), msg)
    return (alt_frame)
  }
  else {##Fail case
      if(alt<lower_lowb | alt>upper_highb){ # Completely out of bounds, should fail with null value for "alt"
        msg <- "Fail: Altitude bounds exceeded"
        alt_frame <- data.frame(log10(0), msg)
        colnames(alt_frame) <- c("alt_log","alt_msg")
        return (alt_frame)
      }
      else {
            if((alt>lower_lowb & alt<lower_b) | (alt>upper_b & alt<upper_highb)){ # Deal with warn cases, otherwise normal cases
                msg <- "Warn: Altitude bounds reached"
                alt_frame <- data.frame(log10(alt), msg) 
                colnames(alt_frame) <- c("alt_log","alt_msg")
                return (alt_frame)
            }
            else { # normal case
                  alt_frame <- data.frame(log10(alt), msg)  # msg = "Succ"
                  colnames(alt_frame) <- c("alt_log","alt_msg")
                  return (alt_frame)
            }# end else
    }# end else
   }# end first else
}#end getAltitude

# 3. Change "Distance from Source" to base 10
getDistanceFromSource <- function(dist_source) {
  return (log10(dist_source))
} 

 # 4. Change "mean width of river" to base 10, and check bounds [a,b]. Use "New GB (43 Group level)
getMeanWidth <- function(mean_width) {
  lower_b <- 0.4
  upper_b <- 117
  if(mean_width>=lower_b & mean_width<=upper_b) {
      return (log10(mean_width))
  }
  else
    return (log10(lower_b))  # could be log10(mean(lower_b, upper_b))
} 

# 5. Change "mean depth of river" to base 10, check bounds [a,b]. Use "New GB (43 Group level)

getMeanDepth <- function(mean_depth) {
  lower_b <- 1.7
  upper_b <- 300
  if(mean_depth >=lower_b & mean_depth<=upper_b){
      return (log10(mean_depth))
  }
  else
    return (log10(lower_b))
} 

# 6. Change "slope of river" to base 10, check bounds [a,b]. Use "New GB (43 Group level)

getSlope_old <- function(slope) {
  lower_b <- 0.1
  upper_b <- 150
  
  if(slope >= lower_b & slope<=upper_b){
    return (log10(slope))
  }
  else
    return (log10(lower_b))
}   

# 7. Calculate Substrate , use mapply
# Check that the input files - bould_cob+pebbpes_gr+snd+silt_cl=[97,103], and MSUBST = [-8,8], the latter is hard coded
getSubstrate <- function(bould_cob, pebbles_gr, snd, silt_cl, lower_b, upper_b) {
  TOTSUB <- sum(bould_cob, pebbles_gr, snd, silt_cl)
  msg    <- "Succ"
  MSUBST <- -999 #Error that no calculation takes place
  if(TOTSUB<lower_b | TOTSUB > upper_b) { # Check bounds [97,103] for TOTSUB
      msg  <- "Fail: TOTSUB bounds exceeded for substrate, "
      msubst_frame <- data.frame(MSUBST, msg)
      colnames(msubst_frame) <- c("log","msg")
    return (msubst_frame)
  }
  else { #Check normal way
    if(TOTSUB>=lower_b & TOTSUB <= upper_b) {
      MSUBST <- ((-7.75 * bould_cob) - (3.25 * pebbles_gr) + (2 * snd) + (8 * silt_cl)) / TOTSUB 
      print(MSUBST)
      
      if(MSUBST< -8.0 | MSUBST >8.0) { # Check if bounds broken, #hard coded for now
          msg  <- "Fail: MSUBST bounds exceeded for substrate"
          msubst_frame <- data.frame(MSUBST, msg)
          colnames(msubst_frame) <- c("log","msg")
        return (msubst_frame )
      }
      else {# Warn case
        if(TOTSUB==lower_b | TOTSUB==upper_b) {
              msg  <- "Warn: bounds reached for substrate"
              msubst_frame <- data.frame(MSUBST, msg)
              colnames(msubst_frame) <- c("log","msg")
          return (msubst_frame )
        }
        else {# Succ case
            msubst_frame <- data.frame(MSUBST, msg)
            colnames(msubst_frame) <- c("log","msg")
          return (msubst_frame )
        }
      }#end else
   }# endif
  }# endelse
}# end getSubstrate 

getSubstrate_old <- function (bould_cob, pebbles_gr, snd, silt_cl) {
  lower_b <- 97
  upper_b <- 103
  subst_total <- sum(bould_cob, pebbles_gr, snd, silt_cl)
  if(subst_total>=lower_b & subst_total <= upper_b) {
    return ( ((-7.75 * bould_cob) - (3.25 * pebbles_gr) + (2 * snd) + (8 * silt_cl)) / subst_total)
  }
  else
    return(-999)
}

#Test getsubstrate()
# Usage: 
    # copy_data$MEAN.SUBSTRATUM <- with(copy_data, mapply(getSubstrate_old, BOULDER_COBBLES, PEBBLES_GRAVEL, SAND, SILT_CLAY))

# 8. Get Discharge 
getRawDischarge <- function(discharge) {
  
  return (discharge)
}
 
# 9. Get Raw Alkalinity 
getRawAlkalinity <- function (alkalinity){
  
  return (alkalinity)
}

# 10. Get latLong, use package rnrfa for function osg_parse
#library(rnrfa)

getLatLong <- function (nat_grid_ref, easting, northing,coordsys_latlon) {
  lat_long    <- NA
  concat_bng  <- paste(nat_grid_ref, substr(easting,1,3), substr(northing,1,3), sep="") 
  lat_long    <- osg_parse (concat_bng, CoordSystem = coordsys_latlon)
  return (lat_long)
}

# NOte :
# Can view all Grid references with "SE..." from  allStations <- catalogue(), using the rNRFA package
# this <- filter(allStations, grepl("SE", gridReference, fixed = TRUE))$gridReference
# sort(this, decresing  = TRUE)
# this
# BNG <- osg_parse(concatenatedNGR, CoordSystem = "BNG")
# lat long is RICT explanatory variables. 
# Easting and Northing used for temperature lookup
# lat.long <- osg_parse(concatenatedNGR, CoordSystem = "WGS84")

# 11. Get BNG

getBNG <- function (nat_grid_ref, easting, northing,coordsys_bng) {
  lat_long    <- NA
  concat_bng  <- paste(nat_grid_ref, substr(easting,1,3), substr(northing,1,3), sep="") 
  new_bng    <- osg_parse (concat_bng, CoordSystem = coordsys_bng)
  return (new_bng)
}

# 12. sPECIAL CASE OF discharge from VELOCITY 
# Note the bounds for Discharge are [0,10], and bounds for velocity ar [1]

getLogDischarge <- function (depth, width, discharge, velocity, lower_b, upper_b) {
  vel_lower <- 1
  vel_upper_b <- 5 
  msg <- "Succ"
 
  if( is.na(discharge)) { # Case no discharge provided 
    # Check velocity
    if(!is.na(velocity) & !is.na(depth) & !is.na(width) ){ #do calculations 
      #Check bounds for non-null velocity
      if(velocity <vel_lower | velocity>vel_upper_b){#out of bounds
        msg <- "Fail: bounds exceeded velocity with null values"
        disch_frame <- data.frame(log10(0), msg)
        colnames(disch_frame) <- c("log","msg")
        return (disch_frame)
      }
      else  #Warn case
        if (velocity ==vel_lower | velocity==vel_upper_b){
          msg <- "Warn: bounds reached velocity"
          disch <- depth/100 * width * velocity/100
          disch_frame <- data.frame(log10(disch), msg)
          colnames(disch_frame) <- c("log","msg")
          return (disch_frame)
        } else {# normal velocity, do calculations
            disch <- depth/100 * width * velocity/100
            disch_frame <- data.frame(log10(disch), msg)
            colnames(disch_frame) <- c("log","msg")
            return (disch_frame)
      }#end else
    }#end if
    else { # In case all are NA including depth, width and velocity
      msg <- "Fail: bounds exceeded with  everything null"
      disch_frame <- data.frame(log10(0), msg)
      colnames(disch_frame) <- c("log","msg")
      return (disch_frame)
    }
  }#
  else{ # Normal service resumes, discharge not null and provided
    #check bounds, then do a log if ok
    if(lower_b>discharge  | upper_b < discharge){
      msg <- "Fail: bounds exceeded discharge"
      disch_frame <- data.frame(log10(discharge), msg)
      colnames(disch_frame) <- c("log","msg")
      return (disch_frame)
    }
    else # bounds reached
      if (lower_b==discharge  | upper_b == discharge){
        msg <- "Warn: bounds reached discharge"
        disch_frame <- data.frame(log10(discharge), msg)
        colnames(disch_frame) <- c("log","msg")
        return (disch_frame)
    }# Success
    else {# All normal, discharge provided
      disch_frame <- data.frame(log10(discharge), msg)
      colnames(disch_frame) <- c("log","msg")
      return (disch_frame)
    }
  }#end else
}