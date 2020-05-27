library(rio)
library(dplyr)
library(tidyr)
library(rgdal)

#download mtbs file
#code modified from https://github.com/mbjoseph/mtbs-data/blob/master/get-mtbs-data.R
if(!file.exists('mtbs_data.csv')){
loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip"
dest <- paste0('mtbs_data.zip')
download.file(loc, dest)
unzip(dest)
unlink(dest)
convert('mtbs_fod_pts_DD.dbf', 'mtbs_data.csv')
}

#create firedata dataframe
firedata <- read.csv('mtbs_data.csv')

#download stations file
if(!file.exists('stations.txt')){
  download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 'stations.txt')
}

#create stations dataframe
typedcols <- c( "A11", "F9", "F10", "F7", "X1","A2",
                "X1","A30", "X1", "A3", "X1", "A3", "X1", "A5" )
stations <- read.fortran("stations.txt",
                         typedcols, 
                         comment.char="")
hdrs <- c("ID", "LAT", "LON", "ELEV", "ST", "NAME","GSN", "HCN", "WMOID")
names(stations) <- hdrs


#create subset dataframe containing only stations in the US
statelist <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
attach(stations)
stationsUS<-stations[which(ST %in% statelist),]
detach(stations)

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



#add fields to mtbs firedata
firedata$closestStnID <- '0'
firedata[which(firedata$Fire_Type == "Wildfire"),]$closestStnID <- stationsUS[1,]$ID
firedata$closestStnLong <- 0
firedata[which(firedata$Fire_Type == "Wildfire"),]$closestStnLong <- stationsUS[1,]$LON
firedata$closestStnLat <- 0
firedata[which(firedata$Fire_Type == "Wildfire"),]$closestStnLat <- stationsUS[1,]$LAT

#identify closest stations
firedata$closestStnDist <- 0
for (row in 1:nrow(firedata)){
  print(row)
  if (firedata$Fire_Type[row] == "Wildfire"){
    for (stn in 1:nrow(stationsUS)){
      if (earth.dist(firedata$Long[row], firedata$Lat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
           earth.dist(firedata$Long[row], firedata$Lat[row], firedata$closestStnLong[row], firedata$closestStnLat[row])){
        firedata$closestStnID[row] <- stationsUS$ID[stn]
        firedata$closestStnLong[row] <-  stationsUS$LON[stn]
        firedata$closestStnLat[row] <-  stationsUS$LAT[stn]
        firedata$closestStnDist[row] <- earth.dist(firedata$Long[row], firedata$Lat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      }
    }
  }
}
