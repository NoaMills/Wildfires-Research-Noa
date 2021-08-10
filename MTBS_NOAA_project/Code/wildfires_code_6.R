#Incorporate drought data into firedata dataframe
library(ncdf4)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
firedata <- read.csv("Data/firedata3.csv")
firedata <- firedata %>% filter(IG_YEAR <= 2014)
filename <- "Data/droughtData.nc"
if(!file.exists(filename)){
  download.file("https://downloads.psl.noaa.gov/Datasets/dai_pdsi/pdsi.mon.mean.selfcalibrated.nc", method="curl", filename)
  
}
droughtData <- nc_open(filename)
print(droughtData)

latData <- ncvar_get(droughtData, "lat")
lonData <- ncvar_get(droughtData, "lon")
dateData <- ncvar_get(droughtData, "time")
pdsiData <- ncvar_get(droughtData, "pdsi")

nc_close(droughtData)

#PDSI Data encodes time as number of hours since 1800-01-01
#Here we add a column to the firedata dataframe that represents the number of hours
#since 1800-01-01

date1 <- as.Date(ymd("1800-01-01"))
nHours <- function(x){
  return(interval(date1, x)%/%hours(1))
}
#Future edit: ensure that the input can be parsed as a date
firedata$numHours <- sapply(firedata$IG_DATE, nHours)

#create functions to identify the closest lon/lat/time point in lonData/latData/dateData
#to a given point
fnLat <- function(x){
  return(which.min(abs(latData-x)))
}
fnLon <- function(x){
  return(which.min(abs(lonData-x)))
}
fnDate <- function(x){
  return(which.min(abs(dateData-x)))
}
firedata$latIndex <- sapply(firedata$BurnBndLat, fnLat)
firedata$lonIndex <- sapply(firedata$BurnBndLon, fnLon)
firedata$dateIndex <- sapply(firedata$numHours, fnDate)

firedata$PDSI <- NA

registerDoParallel()
firedata <- foreach(i=1:nrow(firedata), .combine=rbind) %dopar%{
  newrow <- firedata[i,]
  newrow$PDSI[1] <- pdsiData[newrow$lonIndex[1], newrow$latIndex[1], newrow$dateIndex[1]]
  newrow
}

firedataWf <- firedata %>% filter(Incid_Type == "Wildfire")
print("Correlation between PDSI and wildfire acres burned:")
cor(log(firedataWf$BurnBndAc), firedataWf$PDSI, use="pairwise.complete.obs")

write.csv(firedata, "Data/firedata6.csv")

pdsiInFiredata <- "PDSI" %in% names(firedata)
print(paste0("PDSI in firedata: ", pdsiInFiredata))
