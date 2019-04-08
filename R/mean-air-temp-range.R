# Begin Exclude Linting
#
#
# R version originally written by by C. Laize CEH in early 2012
# Based on FORTRAN PROGRAM TEMPGRID
# For QC and traceability, followed the FORTRAN code as closely as possible; not necessarily best R code
# Converted to calcTemps function by M Dunbar, Environment Agency April 2018
############################################
#


#SUBROUTINE AVCALL
#implemented in R as a function

AVCALL <- function(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE){
  np<-0
  dsum<-0
  smean<-0
  srange<-0

  for (i in c(ME1:ME2)) { #start loop 1
    for (j in c(MN1:MN2)) { #start loop 2

      ij <- c(i," ,", j)
      #print(ij)
      #print(VMEAN[j,i])

      #print(i)

      if(!is.na(VMEAN[j,i])){

        if (i<1 | i>140 | j<1 | j>260) {

          #print(paste(coordinates$Site_ID[l], " ", "VMEAN is NA", sep=""))
          if(np!=0){smean<-smean/dsum;srange<-srange/dsum}
        } else {
          if (VMEAN[j,i]==0) {
            if(np!=0){smean<-smean/dsum;srange<-srange/dsum}
          } else {
            np<-np+1
            d<-(i*50-KE)^2+(j*50-KN)^2
            if (d==0) {d<-0.01}
            dsum<-dsum+1/d
            #dsum<-dsum+round((1/d),digit=2)
            smean<-smean+VMEAN[j,i]/d
            #smean<-smean+round(VMEAN[j,i]/d, digit=2)
            srange<-srange+VRANGE[j,i]/d
            #srange<-srange+round(VRANGE[j,i]/d, digit=2)
          }
        }

      }

    }#end loop 2
  }#end loop 1

  if(np!=0){smean<-smean/dsum;srange<-srange/dsum}
  c(np, smean, srange)

}

#Program to calculate mean temperature and annual temperature

calcTemps <- function(coordinates, air_temp_grid){
  # coordinates is a data frame with three columns
  # SITE_ID
  # EASTING4
  # NORTHING4
  # air_temp_grid is data found in rict package

  NP <- NULL
  SMEAN <- NULL
  SRANGE <- NULL
  ME1 <- NULL
  ME2 <- NULL
  KE <- NULL
  MN1 <- NULL
  MN2 <- NULL
  KN <- NULL
  I <- (air_temp_grid$Easting - 25) / 50
  J <- (air_temp_grid$Northing - 25) / 50
  VMEAN  <- matrix(data = NA, nrow = max(J), ncol = max(I))
  VRANGE <- matrix(data = NA, nrow = max(J), ncol = max(I))

  for (k in c(1:nrow(air_temp_grid))) {
    VMEAN[J[k], I[k]] <- air_temp_grid$TEMPM[k]
    VRANGE[J[k], I[k]] <- air_temp_grid$TEMPR[k]
  }

  temp_grid_out <- NULL

  for (l in c(1:nrow(coordinates))) {
    if(nchar(coordinates$Site_ID[l])<4) {
      for(z in c(1:(4-nchar(coordinates$Site_ID[l])))){
        coordinates$Site_ID[l]<-paste("0", coordinates$Site_ID[l],sep="")
      }
    }

    IGEAST <- coordinates$Easting4[l]
    IGNORTH <- coordinates$Northing4[l]

    KE <- IGEAST - 25
    KN <- IGNORTH - 25

    #find nearest 5km-point to sw and distances e and n from that
    KSQE <- trunc(KE / 50)
    KSQN <- trunc(KN / 50)
    IREME <- KE - 50 * KSQE
    IREMN <- KN - 50 * KSQN

    #test if at a 5km-point or a vertical or horizontal between
    if (IREME==0 & IREMN==0) {#1
      #print("if 1")
      ME1 <- KSQE
      ME2 <- ME1
      MN1 <- KSQN
      MN2 <- MN1
      avcall <- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
      if(NP==1){#2
        #print("if 2")
        TMEAN<-SMEAN
        TRANGE<-SRANGE
      } else {#2
        #print("else 2")
        ME1<-ME1-1
        ME2<-ME2+1
        MN1<-MN1-1
        MN2<-MN2+1
        avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
        if(NP>3){#3
          #print("if 3")
          TMEAN<-SMEAN
          TRANGE<-SRANGE
        } else {#3
          #print("else 3")
          ME1<-ME1-1
          ME2<-ME2+1
          MN1<-MN1-1
          MN2<-MN2+1
          avcall<-AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
          if(NP<4){TMEAN<-0.0;TRANGE<-0.0} else {TMEAN<-SMEAN;TRANGE<-SRANGE}#4
        }#3
      }#2
    } else {#1
      if (IREME==0) {#5
        #print("if 5")
        ME1<-KSQE
        ME2<-ME1
        MN1<-KSQN
        MN2<-MN1+1
        avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
        if(NP==2){#6
          #print("if 6")
          TMEAN<-SMEAN
          TRANGE<-SRANGE
        } else {#6
          #print("else 6")
          ME1<-ME1-1
          ME2<-ME2+1
          avcall<-AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
          if(NP>3){#7
            #print("if 7")
            TMEAN<-SMEAN
            TRANGE<-SRANGE
          } else {#7
            #print("else 7")
            MN1<-MN1-1
            MN2<-MN2+1
            avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
            if(NP<4){
              #print("if 8");
              TMEAN<-0.0;TRANGE<-0.0} else {
                #print("else 8");
                TMEAN<-SMEAN;TRANGE<-SRANGE}#8
          }#7
        }#6
      } else {#5
        if (IREMN==0) {#9

          #print("if 9")
          ME1<-KSQE
          ME2<-ME1+1
          MN1<-KSQN
          MN2<-MN1
          avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
          if(NP==2){#10
            #print("if 10")
            TMEAN<-SMEAN
            TRANGE<-SRANGE
          } else {#10
            #print("else 10")
            MN1<-MN1-1
            MN2<-MN2+1
            avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
            if(NP>3){#11
              #print("if 11")
              TMEAN<-SMEAN
              TRANGE<-SRANGE
            } else {#11
              #print("else 11")
              ME1<-ME1-1
              ME2<-ME2+1
              avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
              if(NP<4){
                #print("if 12");
                TMEAN<-0.0;TRANGE<-0.0} else {
                  #print("else 12");
                  TMEAN<-SMEAN;
                  TRANGE<-SRANGE
                }#12
            }#11
          }#10
        } else {#9
          #must interpolate between 4 values
          ME1<-KSQE
          ME2<-ME1+1
          MN1<-KSQN
          MN2<-MN1+1
          avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
          if(NP>2) {#13
            #print("if 13")
            TMEAN<-SMEAN
            TRANGE<-SRANGE
          } else {#13
            #print("else 13")
            ME1<-ME1-1
            ME2<-ME2+1
            MN1<-MN1-1
            MN2<-MN2+1
            avcall<- AVCALL(ME1, ME2, KE, MN1, MN2, KN, VMEAN, VRANGE);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
            if(NP<4){
              #print("if 14");
              TMEAN<-0.0;TRANGE<-0.0} else {
                #print("else 14");
                TMEAN<-SMEAN;TRANGE<-SRANGE}#14
          }#13
        }#9
      }#5
    }#1
    #print(paste("NP = ", NP, sep=""))
    temp_grid_out<-rbind(temp_grid_out, cbind(coordinates[l,], TMEAN, TRANGE))
  }#0
  temp_grid_out<-as.data.frame(temp_grid_out)
  #names(temp_grid_out)<-c("Site", "East", "North", "TMEAN", "TRANGE") # this line removed as it's stopping the function return working
  #so we deal with the names back in the calling code


}

# End Exclude Linting
