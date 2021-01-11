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
  if(!"TMAX" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM |
     !"TMIN" %in% inv[which(inv$ID == stationsUS$ID[stn]),]$ELEM |
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

#save stations dataframe
write.csv(stationsUS, "Data/mtbs/stationsUS.csv")
stationsUS <- read.csv("Data/mtbs/stationsUS.csv")

firedata <- firedata[,which(!names(firedata) %in% c("X.1", "X", "irwinID", "Incid_Name", "Map_ID", "Map_Prog", "Pre_ID", "Post_ID", "Perim_ID", "dNBR_offst", "dNBR_stdDv", "NoData_T", "IncGreen_T", "Low_T", "Mod_T", "High_T", "ORIG_FID",))]

#save fire data
otherFiredata <- firedata[which(!firedata$Incid_Type %in% c("Prescribed Fire", "Wildfire", "Wildland Fire Use")),]
#remove from firedata all fires that are not wildfires
firedata <- firedata[which(firedata$Incid_Type %in% c("Wildfire", "Prescribed Fire", "Wildland Fire Use")),]
write.csv(firedata, "Data/mtbs/firedata1.csv")
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

#identify closest stations for wildfire data
ptm <- proc.time()
for (row in 1:nrow(firedata)){
  if (row %% 10 == 0) {print(paste0("Row: ",row))}
  for (stn in 1:nrow(stationsUS)){
    #find closest station with TMAX observations during time period
    if (firedata$IG_YEAR[row] <= stationsUS$endTMAX[stn] & firedata$IG_YEAR[row] >= stationsUS$startTMAX[stn] & earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
        earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row], firedata$closestStnLong_TMAX[row], firedata$closestStnLat_TMAX[row])){
      firedata$closestStnDist_TMAX[row] <- earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      firedata$closestStnID_TMAX[row] <- toString(stationsUS$ID[stn])
      firedata$closestStnLong_TMAX[row] <-  stationsUS$LON[stn]
      firedata$closestStnLat_TMAX[row] <-  stationsUS$LAT[stn]
      #print(paste0("row: ", row, " ID: ", firedata$closestStnID_TMAX[row], " stn: ", stn))
    }
    #TMIN
    if (firedata$IG_YEAR[row] <= stationsUS$endTMIN[stn] & firedata$IG_YEAR[row] >= stationsUS$startTMIN[stn] & earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
        earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row], firedata$closestStnLong_TMIN[row], firedata$closestStnLat_TMIN[row])){
      firedata$closestStnDist_TMIN[row] <- earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      firedata$closestStnID_TMIN[row] <- toString(stationsUS$ID[stn])
      firedata$closestStnLong_TMIN[row] <-  stationsUS$LON[stn]
      firedata$closestStnLat_TMIN[row] <-  stationsUS$LAT[stn]
    }
    #PRCP
    if (firedata$IG_YEAR[row] <= stationsUS$endPRCP[stn] & firedata$IG_YEAR[row] >= stationsUS$startPRCP[stn] & earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
        earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row], firedata$closestStnLong_PRCP[row], firedata$closestStnLat_PRCP[row])){
      firedata$closestStnDist_PRCP[row] <- earth.dist(firedata$BurnBndLon[row], firedata$BurnBndLat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      firedata$closestStnID_PRCP[row] <- toString(stationsUS$ID[stn])
      firedata$closestStnLong_PRCP[row] <-  stationsUS$LON[stn]
      firedata$closestStnLat_PRCP[row] <-  stationsUS$LAT[stn]
    }
  }
}
proc.time()-ptm


#save closest station data
write.csv(firedata, "Data/firedata2.csv")

firedata <- read.csv("Data/firedata2.csv")

#########
#Section 3: Download noaa weather data for the stations identified in section 2
#########

#to upload the dataframe, if the mtbs_data.csv file has already been created
firedata <- read.csv("Data/mtbs/mtbs_data_stn.csv")
#rxFiredata <- read.csv("Data/mtbs/prescribed_stn.csv")
stationsUS <- read.csv("Data/mtbs/stationsUS.csv")

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

write.csv(firedata, "Data/firedata3.csv")
########################################
#end of code blocks to remove corrupted files

#########
#Section 4: extract weather data and input to firedata dataframe
#########

firedata <- read.csv("Data/firedata3.csv")
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
      #print(paste0("Flagged: ", stn, " ", date, " ", elm))
      return("flagged")
    }
    #if no quality flags, return the observation value
    value_col_name <- paste0("VALUE", day)
    df2 <- select(df, value_col_name)
    if(nrow(df2) == 0){return(NA)}
    out <- df2[1,1]
    #print(strtoi(out))
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
  varname_TMAX <- paste0("TMAX", i)
  firedata[[varname_TMAX]]<-vNoaaExtract(firedata$closestStnID_TMAX, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "TMAX")
  print(paste0("TMIN", i))
  varname_TMIN <- paste0("TMIN", i)
  firedata[[varname_TMIN]]<-vNoaaExtract(firedata$closestStnID_TMIN, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "TMIN")
  print(paste0("PRCP", i))
  varname_PRCP <- paste0("PRCP", i)
  firedata[[varname_PRCP]]<-vNoaaExtract(firedata$closestStnID_PRCP, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "PRCP")
}
ptm-proc.time()

#Above code segment names columns as "TMAX.3" instead of "TMAX-3". Corrected below

names(firedata)[names(firedata) == "TMAX-3"] <- "TMAX.3"
names(firedata)[names(firedata) == "TMAX-2"] <- "TMAX.2"
names(firedata)[names(firedata) == "TMAX-1"] <- "TMAX.1"
names(firedata)[names(firedata) == "TMIN-3"] <- "TMIN.3"
names(firedata)[names(firedata) == "TMIN-2"] <- "TMIN.2"
names(firedata)[names(firedata) == "TMIN-1"] <- "TMIN.1"
names(firedata)[names(firedata) == "PRCP-3"] <- "PRCP.3"
names(firedata)[names(firedata) == "PRCP-2"] <- "PRCP.2"
names(firedata)[names(firedata) == "PRCP-1"] <- "PRCP.1"

write.csv(firedata, 'Data/firedata4.csv')

#rxFiredata <- read.csv('Data/mtbs/prescribed_noaa.csv')
############

#Section 5
#Next, go through all the fires with measurements that were flagged and find the next closest stations
firedata <- read.csv('Data/firedata4.csv')

#Build the flagged observations datafarme
#stnID contains all previous stations that had flagged values
#stations are consistent accross days but not across variable types.
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
      newrow <- data.frame(fireID = as.character(df$Event_ID[row]), elm = "PRCP", stnID = c(as.character(df$closestStnID_TMIN[row])), fireLong = as.numeric(df$BurnBndLon[row]), fireLat = as.numeric(df$BurnBndLat[row]), stnLong = as.numeric(df[[stnLongVarname]][row]), stnLat = as.numeric(df[[stnLatVarname]][row]),fireDate = as.Date(as.character(df$IG_DATE[row]), format='%Y-%m-%e'), i = as.numeric(i), newStn = as.character(df$closestStnID_TMIN[row]), Dist = 9999, stringsAsFactors = FALSE)
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
          varname=paste0(flaggedObs$elm[fire], i)
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

#In some runs, the strtoi function from noaaExtract hasn't been returning a numeric value. Converted here.
firedatacopy <- firedata
firedata$TMAX.3 <- as.numeric(firedata$TMAX.3)
firedata$TMAX.2 <- as.numeric(firedata$TMAX.2)
firedata$TMAX.1 <- as.numeric(firedata$TMAX.1)
firedata$TMAX0 <- as.numeric(firedata$TMAX0)
firedata$TMAX1 <- as.numeric(firedata$TMAX1)
firedata$TMAX2 <- as.numeric(firedata$TMAX2)
firedata$TMAX3 <- as.numeric(firedata$TMAX3)
firedata$TMIN.3 <- as.numeric(firedata$TMIN.3)
firedata$TMIN.2 <- as.numeric(firedata$TMIN.2)
firedata$TMIN.1 <- as.numeric(firedata$TMIN.1)
firedata$TMIN0 <- as.numeric(firedata$TMIN0)
firedata$TMIN1 <- as.numeric(firedata$TMIN1)
firedata$TMIN2 <- as.numeric(firedata$TMIN2)
firedata$TMIN3 <- as.numeric(firedata$TMIN3)
firedata$PRCP.3 <- as.numeric(firedata$PRCP.3)
firedata$PRCP.2 <- as.numeric(firedata$PRCP.2)
firedata$PRCP.1 <- as.numeric(firedata$PRCP.1)
firedata$PRCP0 <- as.numeric(firedata$PRCP0)
firedata$PRCP1 <- as.numeric(firedata$PRCP1)
firedata$PRCP2 <- as.numeric(firedata$PRCP2)
firedata$PRCP3 <- as.numeric(firedata$PRCP3)

#Find the average TMAX, TMIN and PRCP from the time period from 3 days before the fire started to 3 days after the fire started
for(i in 1:nrow(firedata)){
  print(i)
  firedata$TMAXavg[i] <- mean(c(firedata$TMAX.3[i], firedata$TMAX.2[i], firedata$TMAX.1[i], firedata$TMAX0[i], firedata$TMAX1[i], firedata$TMAX2[i], firedata$TMAX3[i]), na.rm = TRUE)
  firedata$TMINavg[i] <- mean(c(firedata$TMIN.3[i], firedata$TMIN.2[i], firedata$TMIN.1[i], firedata$TMIN0[i], firedata$TMIN1[i], firedata$TMIN2[i], firedata$TMIN3[i]), na.rm = TRUE)
  firedata$PRCPavg[i] <- mean(c(firedata$PRCP.3[i], firedata$PRCP.2[i], firedata$PRCP.1[i], firedata$PRCP0[i], firedata$PRCP1[i], firedata$PRCP2[i], firedata$PRCP3[i]), na.rm = TRUE)
}

write.csv(firedata, "Data/firedata5.csv")

#########
#Section 6: Identify state of each fire. Group fires by state regions with common climates as described by NOAA: https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php
#########
firedata <- read.csv("Data/firedata5.csv")

#Download NWS state and territory shapefiles
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
lonlatvals<-select(firedata, BurnBndLon, BurnBndLat)
firedata$state <- lonlat_to_state(lonlatvals)
firedata$region <- "region"

#36 lat/long coordinates are either just out of the boundary of the US, on a body of water, or otherwise not assigned their given state
#These observations were given state values individually

#NEED TO UPDATE TO INCLUDE 2018 DATA

attach(firedata)
firedata[which(BurnBndLat > 60),]$state <- "Alaska"
firedata[which((BurnBndLat == 49.021 & BurnBndLon == -113.601)),]$state <- "Montana"
firedata[which((BurnBndLat == 49.017 & BurnBndLon == -119.596)),]$state <- "Washington"
firedata[which((BurnBndLat == 49.009 & BurnBndLon == -96.568) | (BurnBndLat == 49.003 & BurnBndLon == -96.878) | (BurnBndLat == 48.225 & BurnBndLon == -90.782) | (BurnBndLat == 48.156 & BurnBndLon == -90.762)),]$state <- "Minnesota"
firedata[which(BurnBndLat == 42.501 & BurnBndLon == -71.656),]$state <- "Massachusetts"
firedata[which((BurnBndLat == 32.609 & BurnBndLon == -116.057) | (BurnBndLat == 32.589 & BurnBndLon == -116.443) | (BurnBndLat == 32.585 & BurnBndLon == -116.514) | (BurnBndLat == 32.579 & BurnBndLon == -116.264)),]$state <- "California"
firedata[which((BurnBndLat == 31.346 & BurnBndLon == -111.12) | (BurnBndLat == 31.299 & BurnBndLon == -110.357)),]$state <- "Arizona"
firedata[which((BurnBndLat == 31.331 & BurnBndLon == -108.9) | (BurnBndLat == 31.329 & BurnBndLon == -108.998) | (BurnBndLat == 31.312 & BurnBndLon == -108.834) | (BurnBndLat == 31.318 & BurnBndLon == -108.714)),]$state <- "New Mexico"
firedata[which(BurnBndLat == 27.012 & BurnBndLon == -97.373),]$state <- "Texas"
firedata[which((BurnBndLat == 36.549 & BurnBndLon == -75.974) | (BurnBndLat == 34.907 & BurnBndLon == -76.345)),]$state <- "North Carolina"
if(anyNA(state)){
  firedata[which(is.na(state) & BurnBndLat < 30.6 & BurnBndLon > -86),]$state <- "Florida" 
}
detach(firedata)

#Assign region values based on state
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


write.csv(firedata, "Data/firedata6.csv")

#only analyze fires whose closest weather station is within 25 km
firedataDistWf <- firedata[which(firedata$closestStnDist_TMAX < 25 & firedata$closestStnDist_TMIN < 25 & firedata$closestStnDist_TMAX < 25 & firedata$Incid_Type == "Wildfire"),]
firedataDistRx <- firedata[which(firedata$closestStnDist_TMAX < 25 & firedata$closestStnDist_TMIN < 25 & firedata$closestStnDist_TMAX < 25 & firedata$Incid_Type == "Prescribed Fire"),]
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
#We construct two subsets of data, one containing wildfires and one containing prescribed burns
#For each region, we construct two datasets: one for initial assessments and one for extended assessments
#The assessment types Initial (SS) and Extended (SS) refer to 'single source' assessments, indicating that a pre-fire image
#was not available for comparison with a post-fire image.
#We compare the regression models that include and exclude the single source assessment fires to see if 
#the data for SS fires leads to less correlation, signaling worse data quality
#For each dataset, plot average TMAX, TMIN, PRCP vs acres burned with averages for various timeframes
#Then try multiple regression models

#Wildfire subsets
firedataSW_I_wf <- firedataDistWf[which(firedataDistWf$region == "Southwest" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataSW_E_wf <- firedataDistWf[which(firedataDistWf$region == "Southwest" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataSE_I_wf <- firedataDistWf[which(firedataDistWf$region == "Southeast" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataSE_E_wf <- firedataDistWf[which(firedataDistWf$region == "Southeast" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataS_I_wf <- firedataDistWf[which(firedataDistWf$region == "South" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataS_E_wf <- firedataDistWf[which(firedataDistWf$region == "South" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataW_I_wf <- firedataDistWf[which(firedataDistWf$region == "West" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataW_E_wf <- firedataDistWf[which(firedataDistWf$region == "West" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataNRP_I_wf <- firedataDistWf[which(firedataDistWf$region == "Northern Rockies and Plains" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataNRP_E_wf <- firedataDistWf[which(firedataDistWf$region == "Northern Rockies and Plains" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataOV_I_wf <- firedataDistWf[which(firedataDistWf$region == "Ohio Valley" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataOV_E_wf <- firedataDistWf[which(firedataDistWf$region == "Ohio Valley" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataNW_I_wf <- firedataDistWf[which(firedataDistWf$region == "Northwest" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataNW_E_wf <- firedataDistWf[which(firedataDistWf$region == "Northwest" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataNE_I_wf <- firedataDistWf[which(firedataDistWf$region == "Northeast" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataNE_E_wf <- firedataDistWf[which(firedataDistWf$region == "Northeast" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataUM_I_wf <- firedataDistWf[which(firedataDistWf$region == "Upper Midwest" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataUM_E_wf <- firedataDistWf[which(firedataDistWf$region == "Upper Midwest" & firedataDistWf$Asmnt_Type == "Extended"),]
firedataAL_I_wf <- firedataDistWf[which(firedataDistWf$region == "Alaska" & firedataDistWf$Asmnt_Type == "Initial"),]
firedataAL_E_wf <- firedataDistWf[which(firedataDistWf$region == "Alaska" & firedataDistWf$Asmnt_Type == "Extended"),]

#Prescribed fire subsets
firedataSW_I_rx <- firedataDistRx[which(firedataDistRx$region == "Southwest" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataSW_E_rx <- firedataDistRx[which(firedataDistRx$region == "Southwest" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataSE_I_rx <- firedataDistRx[which(firedataDistRx$region == "Southeast" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataSE_E_rx <- firedataDistRx[which(firedataDistRx$region == "Southeast" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataS_I_rx <- firedataDistRx[which(firedataDistRx$region == "South" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataS_E_rx <- firedataDistRx[which(firedataDistRx$region == "South" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataW_I_rx <- firedataDistRx[which(firedataDistRx$region == "West" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataW_E_rx <- firedataDistRx[which(firedataDistRx$region == "West" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataNRP_I_rx <- firedataDistRx[which(firedataDistRx$region == "Northern Rockies and Plains" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataNRP_E_rx <- firedataDistRx[which(firedataDistRx$region == "Northern Rockies and Plains" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataOV_I_rx <- firedataDistRx[which(firedataDistRx$region == "Ohio Valley" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataOV_E_rx <- firedataDistRx[which(firedataDistRx$region == "Ohio Valley" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataNW_I_rx <- firedataDistRx[which(firedataDistRx$region == "Northwest" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataNW_E_rx <- firedataDistRx[which(firedataDistRx$region == "Northwest" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataNE_I_rx <- firedataDistRx[which(firedataDistRx$region == "Northeast" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataNE_E_rx <- firedataDistRx[which(firedataDistRx$region == "Northeast" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataUM_I_rx <- firedataDistRx[which(firedataDistRx$region == "Upper Midwest" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataUM_E_rx <- firedataDistRx[which(firedataDistRx$region == "Upper Midwest" & firedataDistRx$Asmnt_Type == "Extended"),]
firedataAL_I_rx <- firedataDistRx[which(firedataDistRx$region == "Alaska" & firedataDistRx$Asmnt_Type == "Initial"),]
firedataAL_E_rx <- firedataDistRx[which(firedataDistRx$region == "Alaska" & firedataDistRx$Asmnt_Type == "Extended"),]
#General Multiple Linear Regression (not dependent on geographic region or assessment type)

attach(firedataDistWf)
fit <-lm(BurnBndAc ~ TMAXavg + TMINavg + PRCPavg + IG_YEAR, data=firedataDistWf)
detach(firedataDistWf)
summary(fit)

WfSubsets <- list(firedataSW_I_wf, firedataSW_E_wf, firedataSE_I_wf, firedataSE_E_wf, firedataS_I_wf, firedataS_E_wf, firedataW_I_wf, firedataW_E_wf, firedataNRP_I_wf, firedataNRP_E_wf, firedataOV_I_wf, firedataOV_E_wf, firedataNW_I_wf, firedataNW_E_wf, firedataNE_I_wf, firedataNE_E_wf, firedataUM_I_wf, firedataUM_E_wf, firedataAL_E_wf, firedataAL_I_wf)
RxSubsets <- list(firedataSW_I_rx, firedataSW_E_rx, firedataSE_I_rx, firedataSE_E_rx, firedataS_I_rx, firedataS_E_rx, firedataW_I_rx, firedataW_E_rx, firedataNRP_I_rx, firedataNRP_E_rx, firedataOV_I_rx, firedataOV_E_rx, firedataNW_I_rx, firedataNW_E_rx, firedataNE_I_rx, firedataNE_E_rx, firedataUM_I_rx, firedataUM_E_rx, firedataAL_E_rx, firedataAL_I_rx)
wf_names <- c("South West Initial wf", "South West Extended wf", "South East Initial wf" ,"South East Extended wf", "South Initial wf", "South Extended wf", "West Initial wf", "West Extended wf", "Northern Rockies and Plains Initial wf", "Northern Rockies and Plains", "Ohio Valley Initial wf", "Ohio Valley Extended wf", "North West Initial wf", "North West Extended wf", "North East Initial wf", "North East Extended wf", "Upper Midwest Initial wf", "Upper Midwest Extended wf", "Alaska Initial wf", "Alaska Extended wf")
rx_names <- c("South West Initial rx", "South West Extended rx", "South East Initial rx" ,"South East Extended rx", "South Initial rx", "South Extended rx", "West Initial rx", "West Extended rx", "Northern Rockies and Plains Initial rx", "Northern Rockies and Plains", "Ohio Valley Initial rx", "Ohio Valley Extended rx", "North West Initial rx", "North West Extended rx", "North East Initial rx", "North East Extended rx", "Upper Midwest Initial rx", "Upper Midwest Extended rx", "Alaska Initial rx", "Alaska Extended rx")

for (i in 1:length(WfSubsets)){
  if(nrow(WfSubsets[[i]])>10){
    print(wf_names[i])
    attach(WfSubsets[[i]], warn.conflicts = FALSE)
    fit <-lm(BurnBndAc ~ TMAXavg + TMINavg + PRCPavg + IG_YEAR, data=WfSubsets[i])
    detach(WfSubsets[[i]])
    print(summary(fit))
  }
}

for (i in 1:length(RxSubsets)){
  if(nrow(RxSubsets[[i]])>10){
    print(paste0(rx_names[i], " ", i))
    attach(RxSubsets[[i]], warn.conflicts = FALSE)
    fit <-lm(BurnBndAc ~ TMAXavg + TMINavg + PRCPavg + IG_YEAR, data=RxSubsets[i])
    detach(RxSubsets[[i]])
    print(summary(fit))
  }
}

write.csv(firedata, "Data/firedata6.csv")
#########
#Section 7: Exploratory data analysis and data visualization
#########
firedata <- read.csv("Data/firedata6.csv")

#create maps of fires per year
#point size corresponds to acres burned, color corresponds to fire type
for(year in 1984:2018){
  print(year)
  firedatayear <- firedata[which(firedata$IG_YEAR == year & firedata$BurnBndAc >=1000),]
  p <- ggplot(data=states_and_territories)+
    geom_sf()+
    geom_point(data=firedatayear, mapping = aes(x=BurnBndLon, y=BurnBndLat, colour = Incid_Type, size = BurnBndAc/1e18), alpha=1/6)+
    coord_sf(xlim=c(-180,-60), ylim=c(20,75))+
    scale_color_manual(values=c("#189ff2", "#f22718", "#f2ef18"))
  ggsave(paste0("map", year, ".png"), p, width = 15, height = 10)
}

#Histograms of fires by month, per type
monthplot <- ggplot(firedata, aes(IG_MONTH))+
  geom_histogram(binwidth=1)+
  facet_wrap(~Incid_Type, ncol=1)
ggsave("monthplot.png", monthplot)

#boxplot of acres by type, outliers removed
ggplot(firedata, aes(x=Incid_Type, y=BurnBndAc))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,20000))

#Scatter plot of acres vs date, color = TMAXavg
firedataDistWf_1000 <- firedataDistWf[which(firedataDistWf$BurnBndAc >= 1000),]
ggplot(firedataDistWf_outlier, aes(x=as.Date(IG_DATE), y=log(BurnBndAc), color=TMAXavg))+
  geom_point(alpha=1/2)+
  scale_x_date()+
  scale_color_gradientn(colors=c("cyan", "deepskyblue", "deepskyblue4", "blueviolet", "indianred3","orangered3", "red"), values=c(0,0.4,0.45,0.5,0.55,0.6,1))

#above broken down by month
#edit so there's a fixed color scale
firedataDistWf_summer <- firedataDistWf_1000[which(firedataDistWf_1000$IG_MONTH %in% c(4,5,6,7,8,9)),]
ggplot(firedataDistWf_summer, aes(x=as.Date(IG_DATE), y=log(BurnBndAc), color=TMAXavg))+
  geom_point(alpha=1/2)+
  scale_x_date()+
  scale_color_gradientn(colors=c("cyan", "deepskyblue", "deepskyblue4", "blueviolet", "indianred3","orangered3", "red"), values=c(0,0.4,0.45,0.5,0.55,0.6,1))

firedataDistWf_winter <- firedataDistWf_1000[which(!firedataDistWf_1000$IG_MONTH %in% c(4,5,6,7,8,9)),]
ggplot(firedataDistWf_winter, aes(x=as.Date(IG_DATE), y=log(BurnBndAc), color=TMAXavg))+
  geom_point(alpha=1/2)+
  scale_x_date()+
  scale_color_gradientn(colors=c("cyan", "deepskyblue", "deepskyblue4", "blueviolet", "indianred3","orangered3", "red"), values=c(0,0.4,0.45,0.5,0.55,0.6,1))

ggplot(firedataDistWf, aes(TMAXavg))+
  geom_histogram()



