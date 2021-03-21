#wildfires_code_2b to be run on remote computer
#Run wildfires_code_2a on local computer first

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

firedata <- read.csv("Data/firedata1.csv")
#for practice run on local:
firedatabackup <- firedata
firedata <- firedata[1:50,]

noaaExtract <- function(stn, date, elm){
  #print(paste0(stn, " ", elm, " ", as.Date(date)))
  #upload dataframe, select rows of the given year and month
  if(!file.exists(paste0("Data/noaa/noaadata/", stn, ".csv"))){
    print(paste0("File missing: ", stn))
    return(-9999)
  }
  if(file.exists(paste0("Data/noaa/noaadata/", stn, ".csv"))){
    df <- read.csv(paste0("Data/noaa/noaadata/", stn, ".csv"))
    df <- df[which(df$year == lubridate::year(date) & df$month == lubridate::month(date) & df$element == elm),]
    if(nrow(df) == 0){return(NA)}
    day = day(date)
    #check for quality flags
    flag_col_name <- paste0("QFLAG", day)
    df1 <- select(df, all_of(flag_col_name))
    if(!is.na(df1[1,1]) & as.character(df1[1,1]) != "" & as.character(df1[1,1]) != " "){
      print(paste0("Flagged: ", stn, " ", date, " ", elm))
      return("flagged")
    }
    #if no quality flags, return the observation value
    value_col_name <- paste0("VALUE", day)
    df2 <- select(df, value_col_name)
    out <- df2[1,1]
    return(as.numeric(out))
  }
}
vNoaaExtract <- Vectorize(noaaExtract, vectorize.args = c("stn", "date"))

##########
#Input weather data for each fire, a week before and a week after the fire
#TMAX3 represents the TMAX value 3 days before the fire started
#TMAX.3 represents value 3 days after the fire started

ptm <- proc.time()
for(i in -21:35){
  print(paste0("TMAX", i))
  if(i<0){
    varname_TMAX <- paste0("TMAX.", -i)
  }else{
    varname_TMAX <- paste0("TMAX", i)
  }
  firedata[[varname_TMAX]]<-vNoaaExtract(firedata$closestStnID_TMAX, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "TMAX")
  print(paste0("TMIN", i))
  if(i<0){
    varname_TMIN <- paste0("TMIN.", -i)
  }else{
    varname_TMIN <- paste0("TMIN", i)
  }
  firedata[[varname_TMIN]]<-vNoaaExtract(firedata$closestStnID_TMIN, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "TMIN")
  print(paste0("PRCP", i))
  if(i<0){
    varname_PRCP <- paste0("PRCP.", -i)
  }else{
    varname_PRCP <- paste0("PRCP", i)
  }
  firedata[[varname_PRCP]]<-vNoaaExtract(firedata$closestStnID_PRCP, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "PRCP")
}
ptm-proc.time()

#Count the number of flagged and NA observations for each fire and for each variable
firedata$TMAXflag <- rowSums((firedata %>% select(starts_with("TMAX"))) == "flagged", na.rm = TRUE)
firedata$TMINflag <- rowSums((firedata %>% select(starts_with("TMIN"))) == "flagged", na.rm = TRUE)
firedata$PRCPflag <- rowSums((firedata %>% select(starts_with("PRCP"))) == "flagged", na.rm = TRUE)
firedata$TMAXna <- rowSums(is.na(firedata %>% select(starts_with("TMAX"))))
firedata$TMINna <- rowSums(is.na(firedata %>% select(starts_with("TMIN"))))
firedata$PRCPna <- rowSums(is.na(firedata %>% select(starts_with("PRCP"))))

#Construct flaggedObs dataframe which contains only relevant information needed
#to identify next nearest stations with valid data

#flaggedData is a subset of firedata which contains rows that have enough flags
#or NA observations to warrant identifying the next nearest station

flaggedObs<-data.frame(fireID=character(), elm=character(), stnID=list(c(character())), fireLong=numeric(), fireLat=numeric(), stnLong=numeric(), stnLat = numeric(), fireDate=character(), i=integer(), newStn = character(), Dist=numeric(), stringsAsFactors = FALSE)
class(flaggedObs$fireDate) <- "Date"
colnames(flaggedObs)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "newStn", "Dist")

flaggedData <- firedata[which(firedata$TMAXflag >= 10 | firedata$TMINflag >= 10 |
                                firedata$PRCPflag >= 10 | firedata$TMAXna >= 15 |
                                firedata$TMINna >= 15 | firedata$PRCPna >= 15),]
for(row in 1:nrow(flaggedData)){
  if (flaggedData$TMAXflag[row] >= 10 | flaggedData$TMAXna[row] >= 15){
    stnLongVarname <- paste0("closestStnLong_", "TMAX")
    stnLatVarname <- paste0("closestStnLat_", "TMAX")
    newrow <- data.frame(fireID = as.character(flaggedData$Event_ID[row]), elm = "TMAX", stnID = c(as.character(flaggedData$closestStnID_TMAX[row])), fireLong = as.numeric(flaggedData$BurnBndLon[row]), fireLat = as.numeric(flaggedData$BurnBndLat[row]), stnLong = as.numeric(flaggedData[[stnLongVarname]][row]), stnLat = as.numeric(flaggedData[[stnLatVarname]][row]),fireDate = as.Date(as.character(flaggedData$IG_DATE[row]), format='%Y-%m-%e'), newStn = as.character(flaggedData$closestStnID_TMAX[row]), Dist = 9999, stringsAsFactors = FALSE)
    colnames(newrow)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "newStn", "Dist")
    flaggedObs<-rbind(flaggedObs, newrow)
  }
  if (flaggedData$TMINflag[row] >= 10 | flaggedData$TMINna[row] >= 15){
    stnLongVarname <- paste0("closestStnLong_", "TMIN")
    stnLatVarname <- paste0("closestStnLat_", "TMIN")
    newrow <- data.frame(fireID = as.character(flaggedData$Event_ID[row]), elm = "TMIN", stnID = c(as.character(flaggedData$closestStnID_TMIN[row])), fireLong = as.numeric(flaggedData$BurnBndLon[row]), fireLat = as.numeric(flaggedData$BurnBndLat[row]), stnLong = as.numeric(flaggedData[[stnLongVarname]][row]), stnLat = as.numeric(flaggedData[[stnLatVarname]][row]),fireDate = as.Date(as.character(flaggedData$IG_DATE[row]), format='%Y-%m-%e'), newStn = as.character(flaggedData$closestStnID_TMIN[row]), Dist = 9999, stringsAsFactors = FALSE)
    colnames(newrow)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "newStn", "Dist")
    flaggedObs<-rbind(flaggedObs, newrow)
  }
  if (flaggedData$PRCPflag[row] >= 10 | flaggedData$PRCPna[row] >= 15){
    stnLongVarname <- paste0("closestStnLong_", "PRCP")
    stnLatVarname <- paste0("closestStnLat_", "PRCP")
    newrow <- data.frame(fireID = as.character(flaggedData$Event_ID[row]), elm = "PRCP", stnID = c(as.character(flaggedData$closestStnID_PRCP[row])), fireLong = as.numeric(flaggedData$BurnBndLon[row]), fireLat = as.numeric(flaggedData$BurnBndLat[row]), stnLong = as.numeric(flaggedData[[stnLongVarname]][row]), stnLat = as.numeric(flaggedData[[stnLatVarname]][row]),fireDate = as.Date(as.character(flaggedData$IG_DATE[row]), format='%Y-%m-%e'), newStn = as.character(flaggedData$closestStnID_PRCP[row]), Dist = 9999, stringsAsFactors = FALSE)
    colnames(newrow)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "newStn", "Dist")
    flaggedObs<-rbind(flaggedObs, newrow)
  }
}

#identify flagged and NA weather observations
write.csv(firedata, "Output/firedata2b.csv")