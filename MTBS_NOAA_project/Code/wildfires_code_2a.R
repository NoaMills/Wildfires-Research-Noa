

#library(rio)
library(dplyr)
library(tidyr)
#library(rgdal)
library(base)
#library(profvis)
#library(rnoaa)
library(lubridate)
#library(sf)
#library(spData)
#library(tidyverse)
library(data.table)
#library(ggplot2)
library(foreach)
#library(parallel)
library(doParallel)
library(doMC)
library(matrixStats)

stationsUS <- read.csv("Data/noaa/stationsUS.csv")

earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  d <- round(d, 5)
  return(d)
}
iteration <- 0
if(file.exists("Data/firedata2c_1.csv")){
  files <- list.files(path="Data/", pattern="firedata2c_[0123456789]+.csv", recursive=FALSE, no.. = TRUE)
  for(file in files){
    fileval <- gsub("firedata2c_", "", file)
    fileval <- gsub(".csv", "", fileval)
    fileval <- as.integer(fileval)
    if(fileval > iteration){
      iteration <- fileval
    }
  }
  firedata <- read.csv(paste0("Data/firedata2c_", iteration, ".csv"), stringsAsFactors = FALSE)
  
  #firedata <- firedata[1:1000,]
  #firedata$closestStnID_PRCP <- as.character(closestStnID_PRCP)
  #Find next nearest stations
  
  if(nrow(firedata[which(firedata$UpdateTMAX == TRUE),]) > 0){
    firedata[which(firedata$UpdateTMAX == TRUE),]$closestStnDist_TMAX <- 10000
    firedata[which(firedata$UpdateTMAX == TRUE),]$closestStnID_TMAX <- "dummy station"
    firedata[which(firedata$UpdateTMAX == TRUE),]$closestStnLong_TMAX <- 90
    firedata[which(firedata$UpdateTMAX == TRUE),]$closestStnLat_TMAX <- -30
  }
  if(nrow(firedata[which(firedata$UpdateTMIN == TRUE),]) > 0){
    firedata[which(firedata$UpdateTMIN == TRUE),]$closestStnDist_TMIN <- 10000
    firedata[which(firedata$UpdateTMIN == TRUE),]$closestStnID_TMIN <- "dummy station"
    firedata[which(firedata$UpdateTMIN == TRUE),]$closestStnLong_TMIN <- 90
    firedata[which(firedata$UpdateTMIN == TRUE),]$closestStnLat_TMIN <- -30
  }
  if(nrow(firedata[which(firedata$UpdatePRCP == TRUE),]) > 0){
    firedata[which(firedata$UpdatePRCP == TRUE),]$closestStnDist_PRCP <- 10000
    firedata[which(firedata$UpdatePRCP == TRUE),]$closestStnID_PRCP <- "dummy station"
    firedata[which(firedata$UpdatePRCP == TRUE),]$closestStnLong_PRCP <- 90
    firedata[which(firedata$UpdatePRCP == TRUE),]$closestStnLat_PRCP <- -30
  }
  
  
  #find next nearest stations for wildfires that have update flags
  registerDoParallel()
  ptm <- proc.time()
  firedataOut <- foreach (row=1:nrow(firedata), .combine = rbind) %dopar% {
    if (row %% 50 == 0) {print(paste0("Row: ",row))}
    newRow <- firedata[row,]
    if(newRow$UpdateTMAX[1] == TRUE | newRow$UpdateTMIN[1] == TRUE | 
       newRow$UpdatePRCP[1] == TRUE){
      print(row)
      for (stn in 1:nrow(stationsUS)){
        #find closest station with TMAX observations during time period
        if(newRow$UpdateTMAX[1] == TRUE){
          prevDist <- newRow$closestStnDist_TMAX[1]
          newDist <- earth.dist(newRow$BurnBndLon[1], newRow$BurnBndLat[1], stationsUS$LON[stn], stationsUS$LAT[stn])
          minDist <- newRow$minDistTMAX[1]
          if (newRow$IG_YEAR[1] <= stationsUS$endTMAX[stn] & newRow$IG_YEAR[1] >= stationsUS$startTMAX[stn] &
              newDist < prevDist & newDist > minDist){
            #print(paste0("Updating TMAX, stn: ", stationsUS$ID[stn], " row: ", row))
            newRow$closestStnDist_TMAX[1] <- newDist
            newRow$closestStnID_TMAX[1] <- toString(stationsUS$ID[stn])
            newRow$closestStnLong_TMAX[1] <-  stationsUS$LON[stn]
            newRow$closestStnLat_TMAX[1] <-  stationsUS$LAT[stn]
            print(paste0("row: ", row, " Stn: ", stn, " ID: ", newRow$closestStnID_TMAX[1], " TMAX"))
          }
        }
        
        #TMIN
        if(newRow$UpdateTMIN[1] == TRUE){
          prevDist <- newRow$closestStnDist_TMIN[1]
          newDist <- earth.dist(newRow$BurnBndLon[1], newRow$BurnBndLat[1], stationsUS$LON[stn], stationsUS$LAT[stn])
          minDist <- newRow$minDistTMIN[1]
          if (newRow$IG_YEAR[1] <= stationsUS$endTMIN[stn] & newRow$IG_YEAR[1] >= stationsUS$startTMIN[stn] &
              newDist < prevDist & newDist > minDist){
            #print(paste0("Updating TMIN, stn: ", stationsUS$ID[stn], " row: ", row))
            newRow$closestStnDist_TMIN[1] <- newDist
            newRow$closestStnID_TMIN[1] <- toString(stationsUS$ID[stn])
            newRow$closestStnLong_TMIN[1] <-  stationsUS$LON[stn]
            newRow$closestStnLat_TMIN[1] <-  stationsUS$LAT[stn]
            print(paste0("row: ", row, " ID: ", newRow$closestStnID_TMAX[1], " TMIN"))
          }
        }
        
        #PRCP
        if(newRow$UpdatePRCP[1] == TRUE){
          prevDist <- newRow$closestStnDist_PRCP[1]
          newDist <- earth.dist(newRow$BurnBndLon[1], newRow$BurnBndLat[1], stationsUS$LON[stn], stationsUS$LAT[stn])
          minDist <- newRow$minDistPRCP[1]
          if (newRow$IG_YEAR[1] <= stationsUS$endPRCP[stn] & newRow$IG_YEAR[1] >= stationsUS$startPRCP[stn] &
              newDist < prevDist & newDist > minDist){
            #print(paste0("Updating PRCP, stn: ", stationsUS$ID[stn], " row: ", row))
            newRow$closestStnDist_PRCP[1] <- newDist
            newRow$closestStnID_PRCP[1] <- toString(stationsUS$ID[stn])
            newRow$closestStnLong_PRCP[1] <-  stationsUS$LON[stn]
            newRow$closestStnLat_PRCP[1] <-  stationsUS$LAT[stn]
            print(paste0("row: ", row, " ID: ", newRow$closestStnID_TMAX[1], " PRCP"))
          }
        }
      }
    }
    newRow
  }
  proc.time()-ptm
  #Time local, 500 rows: 56.71, 26.41
  write.csv(firedataOut, paste0("Data/firedata2a_", iteration + 1, ".csv"))
}else{
  firedata <- read.csv("Data/firedata1.csv")
  write.csv(firedata, "Data/firedata2a_1.csv")
}

print(paste0("Done with 2a, iteration: ", iteration + 1))
