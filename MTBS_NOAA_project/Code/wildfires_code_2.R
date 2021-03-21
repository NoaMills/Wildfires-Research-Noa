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

#This version (superseded by versions a and b) is to be run locally, and only
#extracts weather data 3 days before until 3 days after each fire
#Updated versions (2a and 2b sequentially) extract data 21 days before until
#35 days after each fire


#########
#Section 1: Download noaa weather data for the stations identified in wildfires_code_1.R
#########
firedata <- read.csv("Data/firedata1.csv")
#rxFiredata <- read.csv("Data/mtbs/prescribed_stn.csv")
stationsUS <- read.csv("Data/noaa/stationsUS.csv")

#create a vector of the closest stations
listOfStns <- unique(union(firedata$closestStnID_TMAX, firedata$closestStnID_TMIN))
listOfStns <- unique(union(listOfStns, firedata$closestStnID_PRCP))
listOfStns <- listOfStns[listOfStns != '0']

#download noaa data
for(i in 1:length(listOfStns)){
  if(!file.exists(paste0('Data/noaa/noaadata/', listOfStns[i], ".csv"))){
    outfile <- paste0('Data/noaa/noaadata/', listOfStns[i], ".csv")
    df<-ghcnd(listOfStns[i], refresh=TRUE)
    write.csv(df, outfile) 
    print("written file")
  }
  print(i)
}

######################################
#In downloading the noaa weather files, I have noticed many corrupted files that have
#html code fragments in the csv file, instead of the data values.
#Run these code blocks to identify if there are any corrupted files 
#and to redownload them.

corruptedFiles <- vector()
#identify which files are corrupted
for (i in 1:length(listOfStns)){
  print(i)
  if(file.exists(paste0('Data/noaa/noaadata/', listOfStns[i], ".csv"))){
    df<-read.csv(paste0('Data/noaa/noaadata/', listOfStns[i], ".csv"))
    if (!"TMAX" %in% df$element | !"TMIN" %in% df$element | !toString(listOfStns[i])  %in% df$id){
      corruptedFiles <- c(corruptedFiles, toString(listOfStns[i]))
    }
  }else{
    print(paste0("file missing: ", listOfStns[i]))
  }
}

while(length(corruptedFiles) > 0){
  print(paste0("number of corrupted files: ", length(corruptedFiles)))
  #download corrupted files
  for(str in corruptedFiles){
    outfile <- paste0('Data/noaa/noaadata/', str, ".csv")
    df<-ghcnd(str, refresh=TRUE)
    write.csv(df, outfile) 
    print(paste0(i, ": ", str))
  }
  #identify which files are corrupted
  for (i in 1:length(listOfStns)){
    if(file.exists(paste0('Data/noaa/noaadata/', listOfStns[i], ".csv"))){
      df<-read.csv(paste0('Data/noaa/noaadata/', listOfStns[i], ".csv"))
      if (!"TMAX" %in% df$element | ! "TMIN" %in% df$element | !listOfStns[i]  %in% df$id){
        corruptedFiles <- c(corruptedFiles, toString(listOfStns[i]))
      }
    }
  }
}
#end of code blocks to remove corrupted files

#########
#Section 2: extract weather data and input to firedata dataframe
#########
#Define function to extract weather data and store in firedata dataframe
#ELMn refers to the ELM element n days before the fire
#ELM.n refers to the ELM element n days after the fire (can't use '-' symbol in column name so it's converted to '.', corrected later)
#Ex: TMAX0 refers to the TMAX the day of the fire
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
for(i in -3:3){
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

############

#Section 3
#Next, go through all the fires with measurements that were flagged and find the next closest stations

#Build the flagged observations datafarme
#stnID contains all previous stations that had flagged values
#stations are consistent across days but not across variable types.
#for example, if for a given fire, TMAX1 was flagged, then all values of TMAX will be replaced with values from the next nearest station, and TMAX and PRCP will remain the same
flaggedObs<-data.frame(fireID=character(), elm=character(), stnID=list(c(character())), fireLong=numeric(), fireLat=numeric(), stnLong=numeric(), stnLat = numeric(), fireDate=character(), i=integer(), newStn = character(), Dist=numeric(), stringsAsFactors = FALSE)
class(flaggedObs$fireDate) <- "Date"
colnames(flaggedObs)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i", "newStn", "Dist")
#flaggedObs<-rbind(flaggedObs, c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i"))
for(i in -3:3){
  #identify all fires where TMAXi was flagged add to flagged obs data frame
  if(i<0){
    varname_TMAX <- paste0("TMAX.", -i)
  }else{
    varname_TMAX <- paste0("TMAX", i)
  }
  
  print(varname_TMAX)
  df <- firedata[which(firedata[[varname_TMAX]]=="flagged"),]
  #dfrx <- rxFiredata[which(rxFiredata[[varname_TMAX]]=="flagged"),]
  #df <-rbind(df, dfrx)
  stnLongVarname <- paste0("closestStnLong_", "TMAX")
  stnLatVarname <- paste0("closestStnLat_", "TMAX")
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fireID = as.character(df$Event_ID[row]), elm = "TMAX", stnID = c(as.character(df$closestStnID_TMAX[row])), fireLong = as.numeric(df$BurnBndLon[row]), fireLat = as.numeric(df$BurnBndLat[row]), stnLong = as.numeric(df[[stnLongVarname]][row]), stnLat = as.numeric(df[[stnLatVarname]][row]),fireDate = as.Date(as.character(df$IG_DATE[row]), format='%Y-%m-%e'), i = as.numeric(i), newStn = as.character(df$closestStnID_TMAX[row]), Dist = 9999, stringsAsFactors = FALSE)
      colnames(newrow)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i", "newStn", "Dist")
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
  
  #same for tmin
  if(i<0){
    varname_TMIN <- paste0("TMIN.", -i)
  }else{
    varname_TMIN <- paste0("TMIN", i)
  }
  print(varname_TMIN)
  df <- firedata[which(firedata[[varname_TMIN]]=="flagged"),]
  #dfrx <- rxFiredata[which(rxFiredata[varname_TMIN]=="flagged"),]
  #df <-rbind(df, dfrx)
  stnLongVarname <- paste0("closestStnLong_", "TMIN")
  stnLatVarname <- paste0("closestStnLat_", "TMIN")
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fireID = as.character(df$Event_ID[row]), elm = "TMIN", stnID = c(as.character(df$closestStnID_TMIN[row])), fireLong = as.numeric(df$BurnBndLon[row]), fireLat = as.numeric(df$BurnBndLat[row]), stnLong = as.numeric(df[[stnLongVarname]][row]), stnLat = as.numeric(df[[stnLatVarname]][row]),fireDate = as.Date(as.character(df$IG_DATE[row]), format='%Y-%m-%e'), i = as.numeric(i), newStn = as.character(df$closestStnID_TMIN[row]), Dist = 9999, stringsAsFactors = FALSE)
      colnames(newrow)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i", "newStn", "Dist")
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
  
  #same for prcp
  if(i<0){
    varname_PRCP <- paste0("PRCP.", -i)
  }else{
    varname_PRCP <- paste0("PRCP", i)
  }
  print(varname_PRCP)
  df <- firedata[which(firedata[[varname_PRCP]]=="flagged"),]
  #dfrx <- rxFiredata[which(rxFiredata[[varname_PRCP]]=="flagged"),]
  #df <-rbind(df, dfrx)
  stnLongVarname <- paste0("closestStnLong_", "PRCP")
  stnLatVarname <- paste0("closestStnLat_", "PRCP")
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fireID = as.character(df$Event_ID[row]), elm = "PRCP", stnID = c(as.character(df$closestStnID_PRCP[row])), fireLong = as.numeric(df$BurnBndLon[row]), fireLat = as.numeric(df$BurnBndLat[row]), stnLong = as.numeric(df[[stnLongVarname]][row]), stnLat = as.numeric(df[[stnLatVarname]][row]),fireDate = as.Date(as.character(df$IG_DATE[row]), format='%Y-%m-%e'), i = as.numeric(i), newStn = as.character(df$closestStnID_TMIN[row]), Dist = 9999, stringsAsFactors = FALSE)
      colnames(newrow)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i", "newStn", "Dist")
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
}

#add to flaggedObs the data rows where all observations of one type are NA (ie TMAX-3 through TMAX3 are all NA)
dfnaTMAX<-firedata[which(is.na(firedata$TMAX.3) & is.na(firedata$TMAX.2) & is.na(firedata$TMAX.1) & is.na(firedata$TMAX0) & is.na(firedata$TMAX1) & is.na(firedata$TMAX2) & is.na(firedata$TMAX3)),]
dfnaTMAX$elmNA <-"TMAX"
dfnaTMIN<-firedata[which(is.na(firedata$TMIN.3) & is.na(firedata$TMIN.2) & is.na(firedata$TMIN.1) & is.na(firedata$TMIN0) & is.na(firedata$TMIN1) & is.na(firedata$TMIN2) & is.na(firedata$TMIN3)),]
dfnaTMIN$elmNA <- "TMIN"
dfnaPRCP<-firedata[which(is.na(firedata$PRCP.3) & is.na(firedata$PRCP.2) & is.na(firedata$PRCP.1) & is.na(firedata$PRCP0) & is.na(firedata$PRCP1) & is.na(firedata$PRCP2) & is.na(firedata$PRCP3)),]
dfnaPRCP$elmNA <- "PRCP"

dfna<-rbind(dfnaTMAX, dfnaTMIN)
dfna<-rbind(dfna, dfnaPRCP)

#Add rows from dfna to flaggedObs
for (row in 1:nrow(dfna)){
  print(row)
  stnvarname = paste0("closestStnID_", dfna$elmNA[row])
  stnLongVarname = paste0("closestStnLong_", dfna$elmNA[row])
  newrow <- data.frame(fireID = as.character(dfna$Event_ID[row]), elm = dfna$elmNA[row], stnID = c(as.character(dfna[[stnvarname]][row])), fireLong = as.numeric(dfna$BurnBndLon[row]), fireLat = as.numeric(dfna$BurnBndLat[row]), stnLong = as.numeric(dfna[[stnLongVarname]][row]), stnLat = as.numeric(dfna[[stnLatVarname]][row]),fireDate = as.Date(as.character(dfna$IG_DATE[row]), format='%Y-%m-%e'), i = as.numeric(i), newStn = as.character(dfna$closestStnID_TMIN[row]), Dist = 9999, stringsAsFactors = FALSE)
  colnames(newrow)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i", "newStn", "Dist")
  flaggedObs<-rbind(flaggedObs, newrow)
}

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

#Now, flaggedObs contains all the flagged observations.
#Next we iterate through the data frame, and for each observation, we find the next nearest station,
#and pull the weather values from that station for the given element.
#If the observations are not flagged, we remove the row from the data frame.
#If the observations are flagged, we add the station name to stnID, which contains all the flagged stations.

flaggedObs$stnLat <- -30
flaggedObs$stnLong <- 90
flaggedObs$Dist <- 9999
flaggedObs$newStn <- ""
while (nrow(flaggedObs) > 0){
  print(paste0("Number of flagged observations: ", nrow(flaggedObs)))
  #flaggedObsNext contains the rows that are still flagged.
  flaggedObsNext<-data.frame(fireID=character(), elm=character(), stnID=list(c(character())), fireLong=numeric(), fireLat=numeric(), stnLong=numeric(), stnLat = numeric(), fireDate=character(), i=integer(), newStn = character(), Dist=numeric(), stringsAsFactors = FALSE)
  class(flaggedObsNext$fireDate) <- "Date"
  colnames(flaggedObsNext)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i", "newStn", "Dist")
  flaggedObs$Dist <- 9999
  for (fire in 1:nrow(flaggedObs)){
    if(fire %% 10 == 0){print(paste0("finding station for fire ", fire, " out of ", nrow(flaggedObs)))}
    #find the next nearest station
    for (stn in 1:nrow(stationsUS)){
      #find closest station with TMAX observations during time period
      #print(stn)
      if ((!stationsUS$ID[stn] %in% unlist(flaggedObs$stnID[fire])) & year(as.Date(flaggedObs$fireDate[fire])+flaggedObs$i[fire]) <= stationsUS$endTMAX[stn] & year(as.Date(flaggedObs$fireDate[fire])+flaggedObs$i[fire]) >= stationsUS$startTMAX[stn] &
          earth.dist(flaggedObs$fireLong[fire], flaggedObs$fireLat[fire], stationsUS$LON[stn], stationsUS$LAT[stn])  < flaggedObs$Dist[fire]){
        #print(stn)
        flaggedObs$newStn[fire] <- toString(stationsUS$ID[stn])
        flaggedObs$stnLat[fire] <- stationsUS$LAT[stn]
        flaggedObs$stnLong[fire] <- stationsUS$LON[stn]
        flaggedObs$Dist[fire] <- earth.dist(flaggedObs$fireLong[fire], flaggedObs$fireLat[fire], flaggedObs$stnLong[fire], flaggedObs$stnLat[fire])
      }
    }
  }
  
  #Download noaa files if not already downloaded
  print("closest stations found")
  for (fire in 1:nrow(flaggedObs)){
    if(!file.exists(paste0('Data/noaa/noaadata/', flaggedObs$newStn[fire], ".csv"))){
      outfile <- paste0('Data/noaa/noaadata/', flaggedObs$newStn[fire], ".csv")
      df<-ghcnd(toString(flaggedObs$newStn[fire]), refresh=TRUE)
      write.csv(df, outfile) 
      print("written file")
    }
    #print(fire)
  }
  #next closest station found
  #now extract new noaadata value to firedata. If new noaadata values are not flagged, do nothing.
  #If new noaadata values are flagged, add to flaggedObsNext
  #add new station to list of stns
  for (fire in 1:nrow(flaggedObs)){
    varname <- paste0(flaggedObs$elm[fire], flaggedObs$i[fire])
    newval<-noaaExtract(flaggedObs$newStn[fire], as.Date(flaggedObs$fireDate[fire]+flaggedObs$i[fire]), flaggedObs$elm[fire])
    #print(newval)
    flagged = FALSE
    na_count=0
    for(i in -3:3){
      if(is.na(noaaExtract(flaggedObs$newStn[fire], as.Date(flaggedObs$fireDate[fire]+flaggedObs$i[fire]), flaggedObs$elm[fire]))){
        na_count=na_count+1
        if(na_count == 7){
          flagged=TRUE
        }
      }
      else if (noaaExtract(flaggedObs$newStn[fire], as.Date(flaggedObs$fireDate[fire]+flaggedObs$i[fire]), flaggedObs$elm[fire]) == "flagged"){
        flagged = TRUE
      }
    }
    if (flagged){
      #add station name to stnID list, add row to flaggedObsNext
      flaggedObs[fire, "stnID"][[1]]=list(c(unlist(flaggedObs$stnID[fire]), flaggedObs$newStn[fire]))
      flaggedObsNext <- rbind(flaggedObsNext, flaggedObs[fire,])
    }
    if (!flagged){
      for (i in -3:3){
        if(i<0){
          varname=paste0(flaggedObs$elm[fire], ".", -i)
        }else{
          varname=paste0(flaggedObs$elm[fire], i)
        }
        firedata[which(firedata$Event_ID == flaggedObs$fireID[fire]),][[varname]] <- noaaExtract(flaggedObs$newStn[fire], as.Date(flaggedObs$fireDate[fire] + i), flaggedObs$elm[fire])
      }
    }
    #does newval in next line need to be double bracketed?
    firedata[which(firedata$Event_ID == flaggedObs$fireID[fire]),varname]<-newval
    flaggedObs[fire, "stnID"][[1]]=list(c(unlist(flaggedObs$stnID[fire]), flaggedObs$newStn))
  }
  
  #flaggedObs<-rbind(flaggedObs, c("fireID", "elm", "stnID", "fireLong", "fireLat", "stnLong", "stnLat", "fireDate", "i"))
  
  print(paste0("previous flagged observations: ", nrow(flaggedObs)))
  print(paste0("current flagged observations: ", nrow(flaggedObsNext)))
  flaggedObs <- flaggedObsNext
}

#Find the average TMAX, TMIN and PRCP from the time period from 3 days before the fire started to 3 days after the fire started
for(i in 1:nrow(firedata)){
  print(i)
  firedata$TMAXavg[i] <- mean(c(firedata$TMAX.3[i], firedata$TMAX.2[i], firedata$TMAX.1[i], firedata$TMAX0[i], firedata$TMAX1[i], firedata$TMAX2[i], firedata$TMAX3[i]), na.rm = TRUE)
  firedata$TMINavg[i] <- mean(c(firedata$TMIN.3[i], firedata$TMIN.2[i], firedata$TMIN.1[i], firedata$TMIN0[i], firedata$TMIN1[i], firedata$TMIN2[i], firedata$TMIN3[i]), na.rm = TRUE)
  firedata$PRCPavg[i] <- mean(c(firedata$PRCP.3[i], firedata$PRCP.2[i], firedata$PRCP.1[i], firedata$PRCP0[i], firedata$PRCP1[i], firedata$PRCP2[i], firedata$PRCP3[i]), na.rm = TRUE)
}

write.csv(firedata, "Data/firedata2.csv")