# Begin Exclude Linting
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
      alk_frame <- data.frame(0, msg)
      colnames(alk_frame) <- c("log","msg")
      return (alk_frame)
    }#
    #Case provided, start with calculations
    if(!is.null(hardness) & (!is.na(hardness))) { #' dont use is.na()
      ALK = 4.677 + 0.6393*hardness
      #print(!is.na(hardness))
      #print("hardness not null")
    }
    else
      if(!is.null(calcium) & (!is.na(calcium))) {
        ALK = 14.552 + 1.7606*calcium
        #print("calcium not null")
      }
    else
      if(!is.null(conduct) & (!is.na(conduct))) {
        ALK = 0.3201*conduct - 8.0593
        #print("Conduct not null")
      }

  #print(ALK)
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
    # MEAN_WIDTH, # MEAN_DEPTH, # SLOPE, # DIST_FROM_SOURCE, # DISCHARGE, #ALTITUDE
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
    msg <- paste("Warn: ",envName," bounds exceeded")
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

getAltitude <- function(alt, lower_b, upper_b) { # CHANGE AZURE
  lower_lowb <- 0
  upper_highb <-1345
  msg <- "Succ"
  if(is.null(alt) | is.na(alt)) { # Null cant be evaluated, return 0
    msg <- "Fail: Altitude bounds exceeded with null values"
    alt_frame <- data.frame(0, msg)
    colnames(alt_frame) <- c("log","msg")
    return (alt_frame)
  }
  else {##Fail case
      if(alt<=lower_lowb | alt>=upper_highb){ # Completely out of bounds, should fail with null value for "alt"
        msg <- "Fail: Altitude bounds exceeded"
        alt_frame <- data.frame(0 , msg)
        colnames(alt_frame) <- c("log","msg")
        return (alt_frame)
      }
      else {
            if((alt>lower_lowb & alt<lower_b) | (alt>upper_b & alt<upper_highb)){ # Deal with warn cases, otherwise normal cases
                msg <- "Warn: Altitude bounds reached"
                alt_frame <- data.frame(log10(alt), msg)
                colnames(alt_frame) <- c("log","msg")
                return (alt_frame)
            }
            else { # normal case
                  alt_frame <- data.frame(log10(alt), msg)  # msg = "Succ"
                  colnames(alt_frame) <- c("log","msg")
                  return (alt_frame)
            }# end else
    }# end else
   }# end first else
}#end getAltitude

# 4. Change "Distance from Source" to base 10
getDistanceFromSource <- function(dist_source) {
  return (log10(dist_source))
}

 # 5. Change "mean width of river" to base 10, and check bounds [a,b]. Use "New GB (43 Group level)
getMeanWidth <- function(mean_width) {
  lower_b <- 0.4
  upper_b <- 117
  if(mean_width>=lower_b & mean_width<=upper_b) {
      return (log10(mean_width))
  }
  else
    return (log10(lower_b))  # could be log10(mean(lower_b, upper_b))
}

# 6. Change "mean depth of river" to base 10, check bounds [a,b]. Use "New GB (43 Group level)

getMeanDepth <- function(mean_depth) {
  lower_b <- 1.7
  upper_b <- 300
  if(mean_depth >=lower_b & mean_depth<=upper_b){
      return (log10(mean_depth))
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
      #print(MSUBST)

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

getLatLong <- function (nat_grid_ref, easting, northing, coordsys_latlon, area) {
  lat_long    <- NA
  concat_bng  <- paste(nat_grid_ref, substr(easting,1,3), substr(northing,1,3), sep="")
  if(area == "ni") {
    # Add "I" for NOrthern Ireland
    lat_long <- osg_parse (paste0("I",concat_bng), coord_system = coordsys_latlon)
  } else {
    lat_long <- osg_parse(concat_bng, coord_system = coordsys_latlon)
  }
  return (lat_long)
}

getLatLong_NI <- function (x, y) {
  # Declare the lat_long
  lat_long_all <- NULL
  # It is assumed the East/North have 5 digits, just add a ZERO at the end
  xx <- as.numeric(paste0(x, 0))
  yy <- as.numeric(paste0(y, 0))

  # Loop throuhg the codes to extract the Easting and Northing

  for(i in 1:length(x)) {
    xy = data.frame(easting_x=xx[i], northing_y=yy[i]) # Edited, just to give site identifier
    # 1. create sf object in Irish National Grid (CRS 29903)
    irish.ng <- sf::st_as_sf(xy, coords = c("easting_x", "northing_y"), crs = 29903)
    lat_long <- sf::st_transform(irish.ng, crs = 4326)
    results <- c(lat_long$geometry[[1]][[2]], lat_long$geometry[[1]][1])
    lat_long_all  <- rbind(lat_long_all,results)
  }
  #Remove rownames
  rownames(lat_long_all) <- c()
  #names columns appropriately as "Easting",and "Northing"
  colnames(lat_long_all) <- c("Latitude","Longitude")
  return (as.data.frame(lat_long_all))
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

getBNG <- function(nat_grid_ref, easting, northing, coordsys_bng) {
  lat_long <- NA
  concat_bng  <- paste(nat_grid_ref, substr(easting, 1, 3), substr(northing, 1, 3), sep = "")
  new_bng <- osg_parse(grid_refs = concat_bng, coord_system = coordsys_bng)
  return(new_bng)
}

# 12. sPECIAL CASE OF discharge from VELOCITY
# Note the bounds for Discharge are [0,10], and bounds for velocity ar [1]

getLogDischarge <- function (depth, width, discharge, velocity, lower_b, upper_b) {
  vel_lower <- 1
  vel_upper_b <- 5
  msg <- "Succ"
  #check value of velocity to be ZERO or not supplied (NULL), if so assign 1
  if(is.na(velocity) | velocity==0)
    velocity <- 1

  if( is.na(discharge)) { # Case no discharge provided
    # Check velocity
    if(!is.na(velocity) & !is.na(depth) & !is.na(width) ){ #do calculations
      #Check bounds for non-null velocity
      if(velocity <vel_lower | velocity>vel_upper_b){#out of bounds
        msg <- "Fail: bounds exceeded velocity with null values"
        disch_frame <- data.frame(0, msg)
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
      disch_frame <- data.frame(0, msg)
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

#13. Special writeTo File function - replaces old file if one exists aleardy
# 3.2. Write to a file using the pathname
writeToFile <- function (toWriteFrame, pathname, filename) {

  if( file.exists(paste0(file =pathname,filename))) {
    file.remove(file = paste0(pathname,filename))
    utils::write.csv(toWriteFrame, file = paste0(pathname,filename))
  } else {
    utils::write.csv(toWriteFrame, file = paste0(pathname,filename))
  }
}


#14. Function to return 5 character long Northing, Easting with leading ZEROS if short of 5 characters
# Alternative function used previously - this only replaces one zero, we need multiple application for more zero replacements
#         mycopyEasting <- apply(mycopyEasting, 1, function(x) ifelse(nchar(x)<5, paste0(0, x), x)) # Use "1" in apply() for row
#

getCorrectCodes <- function (coordinates) {

  localCopy <- coordinates
  for(i in 1:length(coordinates)){
    if(5-nchar(localCopy[i])==1)
      localCopy[i] <- paste0(0, localCopy[i])
    if(5-nchar(localCopy[i])==2)
      localCopy[i] <- paste0(00, localCopy[i])
    if(5-nchar(localCopy[i])==3)
      localCopy[i] <- paste0(000, localCopy[i])
    if(5-nchar(localCopy[i])==4)
      localCopy[i] <- paste0(0000, localCopy[i])
    if(5-nchar(localCopy[i])==5)
      localCopy[i] <- paste0(0000, localCopy[i])
  }

  return (localCopy)
}


#15. Validate the longitude

getLongitude <- function(longtde, lower_b, upper_b) { # CHANGE AZURE
  lower_lowb <-  -11.0
  upper_highb <- 2
  msg <- "Succ"
  if(is.null(longtde) | is.na(longtde)) { # Null cant be evaluated, return 0
    msg <- "Fail: lLongitude bounds exceeded with null values"
    #print(msg)
    longtde_frame <- data.frame(0, msg)
    colnames(longtde_frame) <- c("log","msg")
    return (longtde_frame)
  }
  else {##Fail case
    if(longtde<=lower_lowb | longtde>=upper_highb){ # Completely out of bounds, should fail with null value for "longtde"
      msg <- "Fail: Longitude bounds exceeded"
      longtde_frame <- data.frame(0, msg)
      colnames(longtde_frame) <- c("log","msg")
      #print(msg)
      return (longtde_frame)
    }
    else {
      if((longtde>lower_lowb & longtde<lower_b) | (longtde>upper_b & longtde<upper_highb)){ # Deal with warn cases, otherwise normal cases
        msg <- "Warn: Longitude bounds reached"
        longtde_frame <- data.frame( longtde, msg)
        colnames(longtde_frame) <- c("log","msg")
        #print(msg)
        return (longtde_frame)
      }
      else { # normal case
        longtde_frame <- data.frame(longtde, msg)  # msg = "Succ"
        colnames(longtde_frame) <- c("log","msg")
        #print(msg)
        return (longtde_frame)
      }# end else
    }# end else
  }# end first else
}#end getLongitude


#16. Validate the latitude

getLatitude <- function(latde, lower_b, upper_b) { # CHANGE AZURE
  lower_lowb <-  49
  upper_highb <- 71
  msg <- "Succ"
  if(is.null(latde) | is.na(latde)) { # Null cant be evaluated, return 0
    msg <- "Fail: Latitude bounds exceeded with null values"
    #print(msg)
    latde_frame <- data.frame(0, msg)
    colnames(latde_frame) <- c("log","msg")
    return (latde_frame)
  }
  else {##Fail case
    if(latde<=lower_lowb | latde>=upper_highb){ # Completely out of bounds, should fail with null value for "latde"
      msg <- "Fail: Latitude bounds exceeded"
      latde_frame <- data.frame(0, msg)
      colnames(latde_frame) <- c("log","msg")
      #print(msg)
      return (latde_frame)
    }
    else {
      if((latde>lower_lowb & latde<lower_b) | (latde>upper_b & latde<upper_highb)){ # Deal with warn cases, otherwise normal cases
        msg <- "Warn: Latitude bounds reached"
        latde_frame <- data.frame( latde, msg)
        colnames(latde_frame) <- c("log","msg")
        #print(msg)
        return (latde_frame)
      }
      else { # normal case
        latde_frame <- data.frame(latde, msg)  # msg = "Succ"
        colnames(latde_frame) <- c("log","msg")
        #print(msg)
        return (latde_frame)
      }# end else
    }# end else
  }# end first else
}#end getLatitude

###### old functions ########
getSlope_old <- function(slope) {
  lower_b <- 0.1
  upper_b <- 150

  if(slope >= lower_b & slope<=upper_b){
    return (log10(slope))
  }
  else
    return (log10(lower_b))
}


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

# End Exclude Linting
