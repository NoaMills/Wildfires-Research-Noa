library(rio)
library(dplyr)
library(tidyr)
library(rgdal)
library(profvis)
library(rnoaa)

#download mtbs file
#code modified from https://github.com/mbjoseph/mtbs-data/blob/master/get-mtbs-data.R
if(!file.exists('mtbs_data.csv')){
loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip"
dest <- paste0('mtbs_data.zip')
download.file(loc, dest)
unzip(dest)
unlink(dest)
convert('mtbs_fod_pts_DD.dbf', 'mtbs_data.csv')
#remove unnecessary files from zip
file.remove('mtbs_fod_pts_DD.cpg')
file.remove('mtbs_fod_pts_DD.html')
file.remove('mtbs_fod_pts_DD.prj')
file.remove('mtbs_fod_pts_DD.sbn')
file.remove('mtbs_fod_pts_DD.sbx')
file.remove('mtbs_fod_pts_DD.shp')
file.remove('mtbs_fod_pts_DD.shp.xml')
file.remove('mtbs_fod_pts_DD.shx')
}

#create firedata dataframe
firedata <- read.csv('mtbs_data.csv')

#convert ignition date variable from factor to date and store year numerically
firedata$IG_DATE <- as.Date(firedata$Ig_Date)
firedata$IG_YEAR <- as.numeric(format(firedata$IG_DATE, '%Y'))

#download stations file
if(!file.exists('stations.txt')){
  download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 'stations.txt')
}

#download inventory file
if(!file.exists('inventory.txt')){
  download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 'inventory.txt')
}

#create stations dataframe
typedcols <- c( "A11", "F9", "F10", "F7", "X1","A2",
                "X1","A30", "X1", "A3", "X1", "A3", "X1", "A5" )
stations <- read.fortran("stations.txt",
                         typedcols, 
                         comment.char="")
hdrs <- c("ID", "LAT", "LON", "ELEV", "ST", "NAME","GSN", "HCN", "WMOID")
names(stations) <- hdrs

#create inventory dataframe
invcols <- c( "A11", "X1", "F8", "X1", "F9", "X1","A4",
              "X1","I4", "X1", "I4" )
inv <- read.fortran("inventory.txt",
                    invcols,
                    comment.char="")
invhdrs <- c("ID", "LAT", "LON", "ELEM" , "FIRST", "LAST")
names(inv) <- invhdrs

#only include inventory rows for desired observations
inv$ELEM <- as.factor(inv$ELEM)
inv <- inv[which(inv$ELEM == "TMAX" | inv$ELEM == "TMIN" | inv$ELEM == "PRCP"),]

#create subset dataframe containing only stations in the US
statelist <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
attach(stations)
stationsUS<-stations[which(ST %in% statelist),]
detach(stations)

#include station start and stop dates of variable records
stationsUS$startTMAX <- 0
stationsUS$startTMIN <- 0
stationsUS$startPRCP <- 0
stationsUS$endTMAX <- 0
stationsUS$endTMIN <- 0
stationsUS$endPRCP <- 0

#remove from inv the stations that are not in stationsUS
inv$inStnUS <- inv$ID %in% stationsUS$ID
inv <- inv[which(inv$inStnUS),]

#remove from stationsUS the stations that are not in inv
stationsUS$inInv <- stationsUS$ID %in% inv$ID
stationsUS <- stationsUS[which(stationsUS$inInv),]

#remove from inv any stations that do not ever take observations for TMAX, TMIN and PRCP
inv2 <- inv
for (stn in 1:nrow(stationsUS)){
  print(stn)
  if(!"TMAX" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM ||
     !"TMIN" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM ||
     !"PRCP" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM){
      inv2 <- inv2[which(inv$ID != stationsUS$ID[stn]),]
    }
}
inv <- inv2

#include start and stop times of variable obs
#current time:  720.69   46.93  771.09
ptm <- proc.time()
for (stn in 1:nrow(stationsUS)){
  if (stn%%100 == 0){
    print(stn)
  }
  stationsUS$startTMAX <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMAX"),][1]$FIRST
  stationsUS$endTMAX <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMAX"),][1]$LAST
  stationsUS$startTMIN <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMIN"),][1]$FIRST
  stationsUS$endTMIN <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMIN"),][1]$LAST
  stationsUS$startPRCP <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "PRCP"),][1]$FIRST
  stationsUS$endPRCP <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "PRCP"),][1]$FIRST
}
proc.time()-ptm

#define function to calculate distance between two lat long points
#code from https://www.google.com/url?q=https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/&sa=D&ust=1590514403133000&usg=AFQjCNFQnMg9-YSmAMVK-nLN2eHnJSEQTQ
earth.dist <- function (long1, lat1, long2, lat2)
{
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
  return(d)
}


#see what stations are listed in the stations database but not the inventory database, and remove these stations
stnList <- pull(stationsUS, ID)
invList <- pull(inv, ID)
invList <- unique(invList)
stnsNotInInv <- setdiff(stnList, invList)
stationsUS <- stationsUS[which(!stationsUS$ID %in% stnsNotInInv),]

#Add year each station was decomissioned from inventory to stationsUS dataframe
#current time elapsed: 219.14
ptm <- proc.time()
stationsUS$LAST <- 0
for(stn in 1:nrow(stationsUS)){
  stationsUS[stn,]$LAST <- inv[which(inv$ID == stationsUS[stn,]$ID),][1,]$LAST
  #print(paste0(stn, " ", stationsUS[stn,]$LAST))
}
proc.time()-ptm

#remove stations from stationsUS that were decomissioned before 1984
#could make this run faster by looping through stationsUS dataframe instead? Combine with next loop?
#current time elapsed: 245.42    2.05  245.71
ptm <- proc.time()
stationsUS$decom <- FALSE
for(i in 1:nrow(inv)){
  if(inv[i,]$LAST < 1984){
    stationsUS[which(stationsUS$ID == inv[i,]$ID),]$decom = TRUE
    print(i)
  }
}
stationsUS <- stationsUS[which(stationsUS$decom == FALSE),]
proc.time() - ptm

#add fields to mtbs firedata
#dummy station created with coordinates outside the US
firedata$closestStnID <- '0'
firedata[which(firedata$Fire_Type == "Wildfire"),]$closestStnID <- "dummy station"
firedata$closestStnLong <- 0
firedata[which(firedata$Fire_Type == "Wildfire"),]$closestStnLong <- 90
firedata$closestStnLat <- 0
firedata[which(firedata$Fire_Type == "Wildfire"),]$closestStnLat <- -30

#identify closest stations
ptm <- proc.time()

  firedata$closestStnDist <- 0
  for (row in 1:nrow(firedata)){
    if (row %% 10 == 0) {print(row)}
    if (firedata$Fire_Type[row] == "Wildfire"){
      for (stn in 1:nrow(stationsUS)){
        if (firedata$IG_YEAR[row] <= stationsUS$LAST[stn] & earth.dist(firedata$Long[row], firedata$Lat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
          earth.dist(firedata$Long[row], firedata$Lat[row], firedata$closestStnLong[row], firedata$closestStnLat[row])){
          firedata$closestStnDist[row] <- earth.dist(firedata$Long[row], firedata$Lat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
          firedata$closestStnID[row] <- stationsUS$ID[stn]
          firedata$closestStnLong[row] <-  stationsUS$LON[stn]
          firedata$closestStnLat[row] <-  stationsUS$LAT[stn]
        }
      }
    }
  }
proc.time()-ptm

#save closest station data
write.csv(firedata, "mtbs_data.csv")

#to upload the dataframe, if the mtbs_data.csv file has already been created
firedata <- read.csv("mtbs_data.csv")

#create a vector of the closest stations

listOfStns <- firedata$closestStnID
listOfStns <- listOfStns[listOfStns != '0']
listOfStns <- unique(listOfStns)

#download noaa data
for(i in 1:length(listOfStns)){
  if(!file.exists(paste0('noaa/noaaout/', listOfStns[i], ".csv"))){
    outfile <- paste0('noaa/noaaout/', listOfStns[i], ".csv")
    df<-ghcnd(listOfStns[i], refresh=TRUE)
    write.csv(df, outfile) 
    print("written file")
  }
  print(i)
}

numOfTmax = 0;
for (i in 1:length(listOfStns)){
  if(file.exists(paste0('noaa/noaaout/', listOfStns[i], ".csv"))){
    df<-read.csv(paste0('noaa/noaaout/', listOfStns[i], ".csv"))
    if (!"TMAX" %in% df$element){
      print(listOfStns[i])
      numOfTmax = numOfTmax + 1
    }
  }
}

