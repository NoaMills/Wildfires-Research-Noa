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

#########
#Section 1: Download MTBS fire occurence data, download NOAA stations and inventory data
#firedata dataframe contains data on when and wear fires occured in the US, and how many acres they burned
#stations dataframe contains data on where NOAA weather stations are
#inventory dataframe contains data on what weather variables each station observes,
#   and the timeframes of observation for each variable
#########

#code modified from https://github.com/mbjoseph/mtbs-data/blob/master/get-mtbs-data.R
#make mtbs directory
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
  unzip(dest)
  unlink(dest)
  convert('Data/mtbs/mtbs_fod_pts_DD.dbf', 'Data/mtbs/mtbs_data.csv')
  #remove unnecessary files from zip
  file.remove('Data/mtbs/mtbs_fod_pts_DD.cpg')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.html')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.prj')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.sbn')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.sbx')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.shp')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.shp.xml')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.shx')
  file.remove('Data/mtbs/mtbs_fod_pts_DD.dbf')
}

#create firedata dataframe
firedata <- read.csv('Data/mtbs/mtbs_data.csv')

#convert ignition date variable from factor to date and store values numerically
firedata$IG_DATE <- as.Date(firedata$Ig_Date, format = '%m/%e/%Y')
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

############
#Section 2: Transform data. Remove irrelevant data, so the dataframes only include data in the US, data featuring
#desired weather variables, and move variable observation timeframes from the inv dataframe into the stations dataframe
############

#Possible future modification: allow user to specify which variables they want to track
#Limitations of this modification: the code would take forever to run again, and there is less
#   data observed for other variables so there would be more NAs

#only include inventory rows for desired observations
inv$ELEM <- as.character(inv$ELEM)
inv <- inv[which(inv$ELEM == "TMAX" | inv$ELEM == "TMIN" | inv$ELEM == "PRCP"),]

#create subset dataframe containing only stations in the US
statelist <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
stationsUS<-stations[which(stations$ST %in% statelist),]

#include station start and stop dates of variable records
stationsUS$startTMAX <- 0
stationsUS$startTMIN <- 0
stationsUS$startPRCP <- 0
stationsUS$endTMAX <- 0
stationsUS$endTMIN <- 0
stationsUS$endPRCP <- 0


#remove from inv any stations that do not ever take observations for TMAX, TMIN and PRCP
for (stn in 1:nrow(stationsUS)){
  print(stn)
  if(!"TMAX" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM ||
     !"TMIN" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM ||
     !"PRCP" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM){
    inv <- inv[which(inv$ID != stationsUS$ID[stn]),]
  }
}

#remove from inv the stations that are not in stationsUS
inv$inStnUS <- inv$ID %in% stationsUS$ID
inv <- inv[which(inv$inStnUS),]


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
  stationsUS$endPRCP[stn] <- inv[which(inv$ID == stationsUS[stn,]$ID & inv$ELEM == "PRCP"),]$FIRST[1]
}
proc.time()-ptm

#see what stations are listed in the stations database but not the inventory database, and remove these stations
#stnList <- pull(stationsUS, ID)
#invList <- pull(inv, ID)
#invList <- unique(invList)
#stnsNotInInv <- setdiff(stnList, invList)
#stationsUS <- stationsUS[which(!stationsUS$ID %in% stnsNotInInv),]
#commented out because not necessary, alternate code above

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

#save prescribed fire data separately
rxFiredata <- firedata[which(firedata$Fire_Type == "Prescribed"),]
#remove all fires that are not wildfires
firedata <- firedata[which(firedata$Fire_Type == "Wildfire"),]

##########
#Section 3: For each wildfire, identify the closest weather station
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

#identify closest stations
ptm <- proc.time()
for (row in 1:nrow(firedata)){
  #if (row %% 10 == 0) {print(paste0("Row: ",row))}
  for (stn in 1:nrow(stationsUS)){
    #find closest station with TMAX observations during time period
    if (firedata$IG_YEAR[row] <= stationsUS$endTMAX[stn] & firedata$IG_YEAR[row] >= stationsUS$startTMAX[stn] & earth.dist(firedata$Long[row], firedata$Lat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
        earth.dist(firedata$Long[row], firedata$Lat[row], firedata$closestStnLong_TMAX[row], firedata$closestStnLat_TMAX[row])){
      firedata$closestStnDist_TMAX[row] <- earth.dist(firedata$Long[row], firedata$Lat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      firedata$closestStnID_TMAX[row] <- stationsUS$ID[stn]
      firedata$closestStnLong_TMAX[row] <-  stationsUS$LON[stn]
      firedata$closestStnLat_TMAX[row] <-  stationsUS$LAT[stn]
      print(paste0("row: ", row, " ID: ", firedata$closestStnID_TMAX[row], " stn: ", stn))
    }
    #TMIN
    if (firedata$IG_YEAR[row] <= stationsUS$endTMIN[stn] & firedata$IG_YEAR[row] >= stationsUS$startTMIN[stn] & earth.dist(firedata$Long[row], firedata$Lat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
        earth.dist(firedata$Long[row], firedata$Lat[row], firedata$closestStnLong_TMIN[row], firedata$closestStnLat_TMIN[row])){
      firedata$closestStnDist_TMIN[row] <- earth.dist(firedata$Long[row], firedata$Lat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      firedata$closestStnID_TMIN[row] <- stationsUS$ID[stn]
      firedata$closestStnLong_TMIN[row] <-  stationsUS$LON[stn]
      firedata$closestStnLat_TMIN[row] <-  stationsUS$LAT[stn]
    }
    #PRCP
    if (firedata$IG_YEAR[row] <= stationsUS$endTMIN[stn] & firedata$IG_YEAR[row] >= stationsUS$startPRCP[stn] & earth.dist(firedata$Long[row], firedata$Lat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
        earth.dist(firedata$Long[row], firedata$Lat[row], firedata$closestStnLong_PRCP[row], firedata$closestStnLat_PRCP[row])){
      firedata$closestStnDist_PRCP[row] <- earth.dist(firedata$Long[row], firedata$Lat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      firedata$closestStnID_PRCP[row] <- stationsUS$ID[stn]
      firedata$closestStnLong_PRCP[row] <-  stationsUS$LON[stn]
      firedata$closestStnLat_PRCP[row] <-  stationsUS$LAT[stn]
    }
  }
}
proc.time()-ptm

#save closest station data
write.csv(firedata, "Data/mtbs/mtbs_data_stn.csv")

#########
#Section 4: Download noaa weather data for the stations identified in section 3
#########

#to upload the dataframe, if the mtbs_data.csv file has already been created
firedata <- read.csv("Data/mtbs/mtbs_data_stn.csv")

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
    if (!"TMAX" %in% df$element | !"TMAX" %in% df$element | !toString(listOfStns[i])  %in% df$id){
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
      if (!"TMAX" %in% df$element | ! "TMAX" %in% df$element | !listOfStns[i]  %in% df$id){
        corruptedFiles <- c(corruptedFiles, toString(listOfStns[i]))
      }
    }
  }
}
########################################
#end of code blocks to remove corrupted files

#########
#Section 5: extract weather data and input to firedata dataframe
#########

#Define function to extract weather data and store in firedata dataframe
#ELMn refers to the ELM element n days before the fire
#ELM.n refers to the ELM element n days after the fire (can't use '-' symbol in column name so it's converted to '.')
#Ex: TMIN0 refers to the TMIN the day of the fire
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
    if(nrow(df) == 0){return("NA")}
    day = day(date)
    #check for quality flags
    flag_col_name <- paste0("QFLAG", day)
    df1 <- select(df, flag_col_name)
    if(!is.na(df1[1,1]) & as.character(df1[1,1]) != "" & as.character(df1[1,1]) != " "){
      print(paste0("Flagged: ", stn, " ", date, " ", elm))
      return("flagged")
    }
    #if no quality flags, return the observation value
    value_col_name <- paste0("VALUE", day)
    df2 <- select(df, value_col_name)
    if(nrow(df2) == 0){return("NA")}
    out <- df2[1,1]
    #print(strtoi(out))
    return(strtoi(out))
  }
}
vNoaaExtract <- Vectorize(noaaExtract, vectorize.args = c("stn", "date"))

##########
#Input weather data for each fire, a week before and a week after the fire
#TMAX4 represents the TMAX value 4 days before the fire started
#TMAX.4 represents value 4 days after the fire started
ptm <- proc.time()
#case 1: columns ELMi not yet defined
for(i in -7:7){
  print(paste0("TMAX", i))
  if(!(paste0("TMAX", i) %in% names(firedata))){
    firedata[,ncol(firedata) + 1] <- vNoaaExtract(firedata$closestStnID_TMAX, firedata$IG_DATE+i,"TMAX")
    names(firedata)[ncol(firedata)]<-paste0("TMAX", i)
  }
  print(paste0("TMIN", i))
  if(!(paste0("TMIN", i) %in% names(firedata))){
    firedata[,ncol(firedata) + 1] <- vNoaaExtract(firedata$closestStnID_TMIN, as.Date(firedata$IG_DATE)+i, "TMIN")
    names(firedata)[ncol(firedata)]<-paste0("TMIN", i) 
  }
  print(paste0("PRCP", i))
  if(!(paste0("PRCP", i) %in% names(firedata))){
    firedata[,ncol(firedata) + 1] <- vNoaaExtract(firedata$closestStnID_PRCP, as.Date(firedata$IG_DATE)+i, "PRCP")
    names(firedata)[ncol(firedata)]<-paste0("PRCP", i) 
  }
}
ptm - proc.time()
write.csv(firedata, "Data/mtbs/mtbs_noaa.csv")
#case 2: columns ELMi already defined
#test if this one works on its own and can supersede the above case
for(i in -3:3){
  print(paste0("TMAX", i))
  varname_TMAX <- paste0("TMAX", i)
  firedata[[varname_TMAX]]<-vNoaaExtract(firedata$closestStnID_TMAX, as.Date(firedata$IG_DATE, format = '%m/%e/%Y')+days(i), "TMAX")
  print(paste0("TMIN", i))
  varname_TMIN <- paste0("TMIN", i)
  firedata[[varname_TMIN]]<-vNoaaExtract(firedata$closestStnID_TMIN, as.Date(firedata$IG_DATE, format = '%m/%e/%Y')+days(i), "TMIN")
  print(paste0("PRCP", i))
  varname_PRCP <- paste0("PRCP", i)
  firedata[[varname_PRCP]]<-vNoaaExtract(firedata$closestStnID_PRCP, as.Date(firedata$IG_DATE, format = '%m/%e/%Y')+days(i), "PRCP")
}

write.csv(firedata, 'Data/mtbs/mtbs_noaa_data.csv')
############
#Next, go through all the fires with measurements that were flagged and find the next closest stations
flaggedObs<-data.frame(fireID=character(0), elm=character(0), stnID=character(0), fireLong=numeric(0), fireLat=numeric(0), fireDate=character(0), i=integer(0), stringsAsFactors = FALSE)
class(flaggedObs$fireDate) <- "Date"
colnames(flaggedObs)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "fireDate", "i")
#flaggedObs<-rbind(flaggedObs, c("fireID", "elm", "stnID", "fireLong", "fireLat", "fireDate", "i"))
for(i in -3:3){
  varname_TMAX <- paste0("TMAX", i)
  print(varname_TMAX)
  df <- firedata[which(firedata[[varname_TMAX]]=="flagged"),]
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fire_ID = as.character(df$Fire_ID[row]), elm = "TMAX", stnID = as.character(df$closestStnID_TMAX[row]), fireLong = as.numeric(df$Long[row]), fireLat = as.numeric(df$Lat[row]), fireDate = as.Date(as.character(df$IG_DATE[row]), format='%m/%e/%Y'), i = as.numeric(i))
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
  varname_TMIN <- paste0("TMIN", i)
  print(varname_TMIN)
  df <- firedata[which(firedata[[varname_TMIN]]=="flagged"),]
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fire_ID = as.character(df$Fire_ID[row]), elm = "TMAX", stnID = as.character(df$closestStnID_TMAX[row]), fireLong = as.numeric(df$Long[row]), fireLat = as.numeric(df$Lat[row]), fireDate = as.Date(as.character(df$IG_DATE[row]), format='%m/%e/%Y'), i = as.numeric(i))
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
  varname_PRCP <- paste0("PRCP", i)
  print(varname_PRCP)
  df <- firedata[which(firedata[[varname_PRCP]]=="flagged"),]
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fire_ID = as.character(df$Fire_ID[row]), elm = "TMAX", stnID = as.character(df$closestStnID_TMAX[row]), fireLong = as.numeric(df$Long[row]), fireLat = as.numeric(df$Lat[row]), fireDate = as.Date(as.character(df$IG_DATE[row]), format='%m/%e/%Y'), i = as.numeric(i))
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
}

#TEST VERSION OF FIRST 200 ROWS
#before running, check how many flagged values there are
#NO LONGER NEEDED******
firedataTest<-firedata[1:200,]
flaggedObs<-data.frame(fireID=character(0), elm=character(0), stnID=character(0), fireLong=numeric(0), fireLat=numeric(0), fireDate=character(0), i=integer(0), stringsAsFactors = FALSE)
class(flaggedObs$fireDate) <- "Date"
colnames(flaggedObs)<-c("fireID", "elm", "stnID", "fireLong", "fireLat", "fireDate", "i")
#flaggedObs<-rbind(flaggedObs, c("fireID", "elm", "stnID", "fireLong", "fireLat", "fireDate", "i"))
for(i in -3:3){
  varname_TMAX <- paste0("TMAX", i)
  print(varname_TMAX)
  df <- firedataTest[which(firedataTest[[varname_TMAX]]=="flagged"),]
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fire_ID = as.character(df$Fire_ID[row]), elm = "TMAX", stnID = as.character(df$closestStnID_TMAX[row]), fireLong = as.numeric(df$Long[row]), fireLat = as.numeric(df$Lat[row]), fireDate = as.Date(as.character(df$IG_DATE[row]), format='%m/%e/%Y'), i = as.numeric(i))
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
  varname_TMIN <- paste0("TMIN", i)
  print(varname_TMIN)
  df <- firedataTest[which(firedataTest[[varname_TMIN]]=="flagged"),]
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fire_ID = as.character(df$Fire_ID[row]), elm = "TMAX", stnID = as.character(df$closestStnID_TMAX[row]), fireLong = as.numeric(df$Long[row]), fireLat = as.numeric(df$Lat[row]), fireDate = as.Date(as.character(df$IG_DATE[row]), format='%m/%e/%Y'), i = as.numeric(i))
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
  varname_PRCP <- paste0("PRCP", i)
  print(varname_PRCP)
  df <- firedataTest[which(firedataTest[[varname_PRCP]]=="flagged"),]
  print(paste0("Number of flagged obs: ", nrow(df)))
  if(nrow(df)>0){
    for(row in 1:nrow(df)){
      newrow <- data.frame(fire_ID = as.character(df$Fire_ID[row]), elm = "TMAX", stnID = as.character(df$closestStnID_TMAX[row]), fireLong = as.numeric(df$Long[row]), fireLat = as.numeric(df$Lat[row]), fireDate = as.Date(as.character(df$IG_DATE[row]), format='%m/%e/%Y'), i = as.numeric(i))
      flaggedObs<-rbind(flaggedObs, newrow)
    } 
  }
}
#******

#Then, update the observation values. Loop until no more flagged observations.


#Find the average TMAX, TMIN and PRCP from the time period from 3 days before the fire started to 3 days after the fire started
for(i in 1:nrow(firedata)){
  print(i)
  firedata$TMAXavg[i] <- mean(c(firedata$TMAX.1[i], firedata$TMAX.2[i], firedata$TMAX.3[i], firedata$TMAX0[i], firedata$TMAX1[i], firedata$TMAX2[i], firedata$TMAX3[i]), na.rm = TRUE)
  firedata$TMINavg[i] <- mean(c(firedata$TMIN.1[i], firedata$TMIN.2[i], firedata$TMIN.3[i], firedata$TMIN0[i], firedata$TMIN1[i], firedata$TMIN2[i], firedata$TMIN3[i]), na.rm = TRUE)
  firedata$PRCPavg[i] <- mean(c(firedata$PRCP.1[i], firedata$PRCP.2[i], firedata$PRCP.3[i], firedata$PRCP0[i], firedata$PRCP1[i], firedata$PRCP2[i], firedata$PRCP3[i]), na.rm = TRUE)
}
#When a given fire has all NA's for all of a given element, the average is returned as NaN. We convert these to NA
firedata$TMAXavg[is.nan(firedata$TMAXavg)] <- NA
firedata$TMINavg[is.nan(firedata$TMINavg)] <- NA
firedata$PRCPavg[is.nan(firedata$PRCPavg)] <- NA

write.csv(firedata, "Data/mtbs/mtbs_data_stn_noaa.csv")

#Want to add wind to data:
#add this later
for(i in 1:length(listOfStns)){
  
}

#########
#Section 6: Identify state of each fire. Group fires by state regions with common climates as described by NOAA: https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php
#########
firedata <- read.csv("Data/mtbs/mtbs_data_stn_noaa.csv")

#Add code to automatically download NWS state and territory shapefiles
if(!dir.exists("Data/nws")){
  dir.create("Data/nws")
}

if(!file.exists('Data/nws/s_11au16.shp')){
  download.file("https://www.weather.gov/source/gis/Shapefiles/County/s_11au16.zip", 'Data/nws/states_and_territories.zip')
  unzip('Data/nws/states_and_territories.zip', exdir = "Data/nws")
  unlink('Data/nws/states_and_territories.zip')
}

states_and_territories <- st_read("Data/nws/s_11au16.shp")

#Code to convert long/lat to state provided by https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r


## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC. //editted to use nws multipolygon
##
## name_col: Name of a column in `states` that supplies the states'
##           names.
lonlat_to_state <- function(pointsDF,
                            states = states_and_territories,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}
#end of code block borrowed from stack to convert long/lat to state

#identify the state of each fire in firedata and group by region
lonlatvals<-select(firedata, Long, Lat)
firedata$state <- lonlat_to_state(lonlatvals)
firedata$region <- "region"

#28 lat/long coordinates are either just out of the boundary of the US, on a body of water, or otherwise not assigned their given state
#These observations were given state values individually
attach(firedata)
firedata[which(Lat > 60),]$state <- "Alaska"
firedata[which((Lat == 49.021 & Long == -113.601)),]$state <- "Montana"
firedata[which((Lat == 49.017 & Long == -119.596)),]$state <- "Washington"
firedata[which((Lat == 49.009 & Long == -96.568) | (Lat == 49.003 & Long == -96.878) | (Lat == 48.225 & Long == -90.782) | (Lat == 48.156 & Long == -90.762)),]$state <- "Minnesota"
firedata[which(Lat == 42.501 & Long == -71.656),]$state <- "Massachusetts"
firedata[which((Lat == 32.609 & Long == -116.057) | (Lat == 32.589 & Long == -116.443) | (Lat == 32.585 & Long == -116.514) | (Lat == 32.579 & Long == -116.264)),]$state <- "California"
firedata[which((Lat == 31.346 & Long == -111.12) | (Lat == 31.299 & Long == -110.357)),]$state <- "Arizona"
firedata[which((Lat == 31.331 & Long == -108.9) | (Lat == 31.329 & Long == -108.998) | (Lat == 31.312 & Long == -108.834)),]$state <- "New Mexico"
firedata[which(Lat == 27.012 & Long == -97.373),]$state <- "Texas"
firedata[which(is.na(firedata$state)),] <- "Florida"
detach(firedata)

#faster version, fix
firedata[which(firedata$state %in% c("California", "Nevada")),]$region <- "West"
firedata[which(firedata$state %in% c("Utah", "Colorado", "Arizona", "New Mexico")),]$region <- "Southwest"
firedata[which(firedata$state %in% c("Washington", "Oregon", "Idaho")),]$region <- "Northwest"
firedata[which(firedata$state %in% c("Montana", "North Dakota", "South Dakota", "Wyoming", "Nebraska")),]$region <- "Northern Rockies and Plains"
firedata[which(firedata$state %in% c("Kansas", "Oklahoma", "Texas", "Arkansas", "Louisiana", "Mississippi")),]$region <- "South"
firedata[which(firedata$state %in% c("Minnesota", "Wisconsin", "Michigan", "Iowa")),]$region <- "Upper Midwest"
firedata[which(firedata$state %in% c("Missouri", "Illinois", "Indiana", "Ohio", "West Virginia", "Kentucky", "Tennessee")),]$region <- "Ohio Valley"
firedata[which(firedata$state %in% c("Virginia", "North Carolina", "South Carolina", "Georgia", "Alabama", "Florida")),]$region <- "Southeast"
firedata[which(firedata$state %in% c("Maryland", "Delaware", "New Jersey", "Pennsylvania", "Connecticut", "Rhode Island", "New York", "Vermont", "Massachusetts", "New Hampshire", "Maine", "DC")),]$region <- "Northeast"
firedata[which(firedata$state == "Alaska"),]$region <- "Alaska"
firedata[which(firedata$state == "Hawaii"),]$region <- "Hawaii"
firedata[which(firedata$state == "Puerto Rico"),]$region <- "Puerto Rico"


write.csv(firedata, "Data/mtbs/mtbs_data_region.csv")

#########
#Section 7: Exploratory data analysis and data visualization
#########
firedata <- read.csv("Data/mtbs/mtbs_data_region.csv")

#only analyze fires whose closest weather station is within 25 km
#this includes 1,617 fires
firedataDist <- firedata[which(firedata$closestStnDist < 25),]

#The assessment type (Initial vs Extended) was determined based on the type of ecosystem.
#Extended assessments were used for forests, woodlands, shrublands, etc. where wildfires have longer lasting impacts
#Initial assessments were used for grasslands where ecosystems tend to recover by the next growing season

#For each region and assessment type, we wish to compare the weather variables with the number of acres burned
#For each region, we use the following abbreviated region names:

#SW = Southwest
#SE = Southeast
#S = South
#W = West
#NRP = Northern Rockies and Plains
#OV = Ohio Valley
#NW = Northwest
#NE = Northeast
#UM = Upper Midwest
#AL = Alaska

#For these models, we temporarily exclude Hawaii (18 total fires) and Puerto Rico (4 fires) due to the small sample size
#For each region, we construct two datasets: one for initial assessments and one for extended assessments
#The assessment types Initial (SS) and Extended (SS) refer to 'single source' assessments, indicating that a pre-fire image
  #was not available for comparison with a post-fire image.
  #We compare the regression models that include and exclude the single source assessment fires to see if 
  #the data for SS fires leads to less correlation, signaling worse data quality
#For each dataset, plot average TMAX, TMIN, PRCP vs acres burned with averages for various timeframes
#Then try multiple regression models


firedataSW_I <- firedataDist[which(firedataDist$region == "Southwest" & firedataDist$Asmnt_Type == "Initial"),]
firedataSW_E <- firedataDist[which(firedataDist$region == "Southwest" & firedataDist$Asmnt_Type == "Extended"),]
firedataSE_I <- firedataDist[which(firedataDist$region == "Southeast" & firedataDist$Asmnt_Type == "Initial"),]
firedataSE_E <- firedataDist[which(firedataDist$region == "Southeast" & firedataDist$Asmnt_Type == "Extended"),]
firedataS_I <- firedataDist[which(firedataDist$region == "South" & firedataDist$Asmnt_Type == "Initial"),]
firedataS_E <- firedataDist[which(firedataDist$region == "South" & firedataDist$Asmnt_Type == "Extended"),]
firedataW_I <- firedataDist[which(firedataDist$region == "West" & firedataDist$Asmnt_Type == "Initial"),]
firedataW_E <- firedataDist[which(firedataDist$region == "West" & firedataDist$Asmnt_Type == "Extended"),]
firedataNRP_I <- firedataDist[which(firedataDist$region == "Northern Rockies and Plains" & firedataDist$Asmnt_Type == "Initial"),]
firedataNRP_E <- firedataDist[which(firedataDist$region == "Northern Rockies and Plains" & firedataDist$Asmnt_Type == "Extended"),]
firedataOV_I <- firedataDist[which(firedataDist$region == "Ohio Valley" & firedataDist$Asmnt_Type == "Initial"),]
firedataOV_E <- firedataDist[which(firedataDist$region == "Ohio Valley" & firedataDist$Asmnt_Type == "Extended"),]
firedataNW_I <- firedataDist[which(firedataDist$region == "Northwest" & firedataDist$Asmnt_Type == "Initial"),]
firedataNW_E <- firedataDist[which(firedataDist$region == "Northwest" & firedataDist$Asmnt_Type == "Extended"),]
firedataNE_I <- firedataDist[which(firedataDist$region == "Northeast" & firedataDist$Asmnt_Type == "Initial"),]
firedataNE_E <- firedataDist[which(firedataDist$region == "Northeast" & firedataDist$Asmnt_Type == "Extended"),]
firedataUM_I <- firedataDist[which(firedataDist$region == "Upper Midwest" & firedataDist$Asmnt_Type == "Initial"),]
firedataUM_E <- firedataDist[which(firedataDist$region == "Upper Midwest" & firedataDist$Asmnt_Type == "Extended"),]
firedataAL_I <- firedataDist[which(firedataDist$region == "Alaska" & firedataDist$Asmnt_Type == "Initial"),]
firedataAL_E <- firedataDist[which(firedataDist$region == "Alaska" & firedataDist$Asmnt_Type == "Extended"),]


