library(rio)
library(dplyr)
library(tidyr)
library(rgdal)
library(base)
library(profvis)
library(rnoaa)
library(lubridate)
library(sf)
library(spData)
library(tidyverse)
library(data.table)
library(ggplot2)
library(foreach)



#########
#Section 1: Download MTBS fire occurence data, download NOAA stations and inventory data
#firedata dataframe contains data on when and wear fires occured in the US, and how many acres they burned
#stations dataframe contains data on where NOAA weather stations are
#inventory dataframe contains data on what weather variables each station observes,
#   and the timeframes of observation for each variable
#########

#code do download mtbs data modified from https://github.com/mbjoseph/mtbs-data/blob/master/get-mtbs-data.R
#make data directory
if(!dir.exists("Data")){
  dir.create("Data")
}
if(!dir.exists("Data/mtbs")){
  dir.create("Data/mtbs")
}
if(!file.exists('Data/mtbs/mtbs_data.csv')){
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip"
  dest <- paste0('Data/mtbs/mtbs_data.zip')
  download.file(loc, dest)
  unzip(dest, exdir="Data/mtbs")
  unlink(dest)
  convert('Data/mtbs/mtbs_FODpoints_DD.dbf', 'Data/mtbs/mtbs_data.csv')
  #remove unnecessary files from zip
  file.remove('Data/mtbs/mtbs_FODpoints_DD.cpg')
  file.remove('Data/mtbs/mtbs_FODpoints_DD_metadata.xml')
  file.remove('Data/mtbs/mtbs_FODpoints_DD.prj')
  file.remove('Data/mtbs/mtbs_FODpoints_DD.sbn')
  file.remove('Data/mtbs/mtbs_FODpoints_DD.sbx')
  file.remove('Data/mtbs/mtbs_FODpoints_DD.shp')
  file.remove('Data/mtbs/mtbs_FODpoints_DD.shp.xml')
  file.remove('Data/mtbs/mtbs_FODpoints_DD.shx')
  file.remove('Data/mtbs/mtbs_FODpoints_DD.dbf')
}

#create firedata dataframe
firedata <- read.csv('Data/mtbs/mtbs_data.csv')

#convert ignition date variable from factor to date and store values numerically
firedata$IG_DATE <- as.Date(firedata$Ig_Date, format = '%Y-%m-%e')
firedata$IG_YEAR <- as.numeric(format(firedata$IG_DATE, '%Y'))
firedata$IG_MONTH <- as.numeric(format(firedata$IG_DATE, '%m'))
firedata$IG_DAY <- as.numeric(format(firedata$IG_DATE, '%d'))

#create noaa data directories
if(!dir.exists("Data/noaa")){
  dir.create("Data/noaa")
}
if(!dir.exists("Data/noaa/noaadata")){
  dir.create("Data/noaa/noaadata")
}
#download noaa weather stations file
if(!file.exists('Data/noaa/stations.txt')){
  download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 'Data/noaa/stations.txt')
}

#download noaa weather inventory file
if(!file.exists('Data/noaa/inventory.txt')){
  download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 'Data/noaa/inventory.txt')
}

#create stations dataframe
typedcols <- c( "A11", "F9", "F10", "F7", "X1","A2",
                "X1","A30", "X1", "A3", "X1", "A3", "X1", "A5" )
stations <- read.fortran("Data/noaa/stations.txt",
                         typedcols, 
                         comment.char="")
hdrs <- c("ID", "LAT", "LON", "ELEV", "ST", "NAME","GSN", "HCN", "WMOID")
names(stations) <- hdrs

#create inventory dataframe
invcols <- c( "A11", "X1", "F8", "X1", "F9", "X1","A4",
              "X1","I4", "X1", "I4" )
inv <- read.fortran("Data/noaa/inventory.txt",
                    invcols,
                    comment.char="")
invhdrs <- c("ID", "LAT", "LON", "ELEM" , "FIRST", "LAST")
names(inv) <- invhdrs

#only include inventory rows for desired observations
inv$ELEM <- as.character(inv$ELEM)
inv <- inv[which(inv$ELEM == "TMAX" | inv$ELEM == "TMIN" | inv$ELEM == "PRCP"),]

#create subset dataframe containing only stations in the US
statelist <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
stationsUS<-stations[which(stations$ST %in% statelist),]

#include station start and stop dates of variable records
#further updated below
stationsUS$startTMAX <- 0
stationsUS$startTMIN <- 0
stationsUS$startPRCP <- 0
stationsUS$endTMAX <- 0
stationsUS$endTMIN <- 0
stationsUS$endPRCP <- 0

#remove from inv the stations that are not in stationsUS
inv$inStnUS <- inv$ID %in% stationsUS$ID
inv <- inv[which(inv$inStnUS),]

#remove from inv any stations that do not ever take observations for TMAX, TMIN and PRCP
for (stn in 1:nrow(stationsUS)){
  #print(stn)
  if(!"TMAX" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM |
     !"TMIN" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM |
     !"PRCP" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM){
    inv <- inv[which(inv$ID != stationsUS$ID[stn]),]
  }
}

#remove from stationsUS the stations that are not in inv
stationsUS$inInv <- stationsUS$ID %in% inv$ID
stationsUS <- stationsUS[which(stationsUS$inInv),]

#include start and stop times of variable obs
#current time:  720.69   46.93  771.09
ptm <- proc.time()
for (stn in 1:nrow(stationsUS)){
  if (stn%%100 == 0){
    print(stn)
  }
  stationsUS$startTMAX[stn] <- inv[which(inv$ID == stationsUS[stn,]$ID & inv$ELEM == "TMAX"),]$FIRST[1]
  stationsUS$endTMAX[stn] <- inv[which(inv$ID == stationsUS[stn,]$ID & inv$ELEM == "TMAX"),]$LAST[1]
  stationsUS$startTMIN[stn] <- inv[which(inv$ID == stationsUS[stn,]$ID & inv$ELEM == "TMIN"),]$FIRST[1]
  stationsUS$endTMIN[stn] <- inv[which(inv$ID == stationsUS[stn,]$ID & inv$ELEM == "TMIN"),]$LAST[1]
  stationsUS$startPRCP[stn] <- inv[which(inv$ID == stationsUS[stn,]$ID & inv$ELEM == "PRCP"),]$FIRST[1]
  stationsUS$endPRCP[stn] <- inv[which(inv$ID == stationsUS[stn,]$ID & inv$ELEM == "PRCP"),]$LAST[1]
}
proc.time()-ptm

#Add year each station was decomissioned from inventory to stationsUS dataframe
#current time elapsed: 219.14
ptm <- proc.time()
stationsUS$LAST <- 0
for(stn in 1:nrow(stationsUS)){
  stationsUS[stn,]$LAST <- inv[which(inv$ID == stationsUS[stn,]$ID),][1,]$LAST
  #print(paste0(stn, " ", stationsUS[stn,]$LAST))
}
proc.time()-ptm
stationsUS$FIRST <- 0
for(stn in 1:nrow(stationsUS)){
  stationsUS[stn,]$FIRST <- inv[which(inv$ID == stationsUS[stn,]$ID),][1,]$FIRST
}

#remove stations from stationsUS that were decomissioned before 1984
ptm <- proc.time()
stationsUS$decom <- FALSE
for(i in 1:nrow(inv)){
  if(inv[i,]$LAST < 1984){
    stationsUS[which(stationsUS$ID == inv[i,]$ID),]$decom = TRUE
    #print(i)
  }
}
stationsUS <- stationsUS[which(stationsUS$decom == FALSE),]
proc.time() - ptm

#save stations dataframe
write.csv(stationsUS, "Data/noaa/stationsUS.csv")
stationsUS <- read.csv("Data/noaa/stationsUS.csv")

firedata <- firedata[,which(!names(firedata) %in% c("X.1", "X", "irwinID", "Incid_Name", "Map_ID", "Map_Prog", "Pre_ID", "Post_ID", "Perim_ID", "dNBR_offst", "dNBR_stdDv", "NoData_T", "IncGreen_T", "Low_T", "Mod_T", "High_T", "ORIG_FID"))]

#save fire data
otherFiredata <- firedata[which(!firedata$Incid_Type %in% c("Prescribed Fire", "Wildfire", "Wildland Fire Use")),]
#remove from firedata all fires that are not wildfires
firedata <- firedata[which(firedata$Incid_Type %in% c("Wildfire", "Prescribed Fire", "Wildland Fire Use")),]
#write.csv(firedata, "Data/mtbs/firedata1.csv")
write.csv(otherFiredata, "Data/mtbs/otherFiredata.csv")

##########
#Section 2: For each wildfire, identify the closest weather station
##########

#add closest station fields to mtbs firedata
#fields initiated as a dummy station located outside the US
firedata$closestStnDist_TMAX <- 10000
firedata$closestStnID_TMAX <- "dummy station"
firedata$closestStnLong_TMAX <- 90
firedata$closestStnLat_TMAX <- -30
firedata$closestStnDist_TMIN <- 10000
firedata$closestStnID_TMIN <- "dummy station"
firedata$closestStnLong_TMIN <- 90
firedata$closestStnLat_TMIN <- -30
firedata$closestStnDist_PRCP <- 10000
firedata$closestStnID_PRCP <- "dummy station"
firedata$closestStnLong_PRCP <- 90
firedata$closestStnLat_PRCP <- -30

#define function to calculate distance between two lat long points in kilometers
#code from https://www.google.com/url?q=https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/&sa=D&ust=1590514403133000&usg=AFQjCNFQnMg9-YSmAMVK-nLN2eHnJSEQTQ
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

#identify closest stations for wildfire data
registerDoParallel()
ptm <- proc.time()
firedataOut <- foreach (row=1:nrow(firedata), .combine = rbind) %dopar% {
  if (row %% 50 == 0) {print(paste0("Row: ",row))}
  newRow <- firedata[row,]
  for (stn in 1:nrow(stationsUS)){
    #find closest station with TMAX observations during time period

    prevDist <- newRow$closestStnDist_TMAX[1]
    newDist <- earth.dist(newRow$BurnBndLon[1], newRow$BurnBndLat[1], stationsUS$LON[stn], stationsUS$LAT[stn])
    if (newRow$IG_YEAR[1] <= stationsUS$endTMAX[stn] & newRow$IG_YEAR[1] >= stationsUS$startTMAX[stn] &
        newDist < prevDist){
      #print(paste0("Updating TMAX, stn: ", stationsUS$ID[stn], " row: ", row))
      newRow$closestStnDist_TMAX[1] <- newDist
      newRow$closestStnID_TMAX[1] <- toString(stationsUS$ID[stn])
      newRow$closestStnLong_TMAX[1] <-  stationsUS$LON[stn]
      newRow$closestStnLat_TMAX[1] <-  stationsUS$LAT[stn]
      #print(paste0("row: ", row, " ID: ", newRow$closestStnID_TMAX[1], " TMAX"))
    }
    
    #TMIN
    prevDist <- newRow$closestStnDist_TMIN[1]
    newDist <- earth.dist(newRow$BurnBndLon[1], newRow$BurnBndLat[1], stationsUS$LON[stn], stationsUS$LAT[stn])
    if (newRow$IG_YEAR[1] <= stationsUS$endTMIN[stn] & newRow$IG_YEAR[1] >= stationsUS$startTMIN[stn] &
        newDist < prevDist){
      #print(paste0("Updating TMIN, stn: ", stationsUS$ID[stn], " row: ", row))
      newRow$closestStnDist_TMIN[1] <- newDist
      newRow$closestStnID_TMIN[1] <- toString(stationsUS$ID[stn])
      newRow$closestStnLong_TMIN[1] <-  stationsUS$LON[stn]
      newRow$closestStnLat_TMIN[1] <-  stationsUS$LAT[stn]
      #print(paste0("row: ", row, " ID: ", newRow$closestStnID_TMAX[1], " TMIN"))
    }
    
    #PRCP
    
    prevDist <- newRow$closestStnDist_PRCP[1]
    newDist <- earth.dist(newRow$BurnBndLon[1], newRow$BurnBndLat[1], stationsUS$LON[stn], stationsUS$LAT[stn])
    if (newRow$IG_YEAR[1] <= stationsUS$endPRCP[stn] & newRow$IG_YEAR[1] >= stationsUS$startPRCP[stn] &
        newDist < prevDist){
      #print(paste0("Updating PRCP, stn: ", stationsUS$ID[stn], " row: ", row))
      newRow$closestStnDist_PRCP[1] <- newDist
      newRow$closestStnID_PRCP[1] <- toString(stationsUS$ID[stn])
      newRow$closestStnLong_PRCP[1] <-  stationsUS$LON[stn]
      newRow$closestStnLat_PRCP[1] <-  stationsUS$LAT[stn]
      #print(paste0("row: ", row, " ID: ", newRow$closestStnID_TMAX[1], " PRCP"))
    }
  }
  newRow
}
proc.time()-ptm

firedata <- firedataOut

write.csv(firedata, "Data/firedata1.csv")