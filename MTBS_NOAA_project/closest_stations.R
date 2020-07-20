library(rio)
library(dplyr)
library(tidyr)
library(rgdal)
library(profvis)
library(rnoaa)
library(lubridate)

#download mtbs file
#code modified from https://github.com/mbjoseph/mtbs-data/blob/master/get-mtbs-data.R

#make mtbs directory
if(!dir.exists("mtbs")){
  dir.create("mtbs")
}
if(!file.exists('mtbs/mtbs_data.csv')){
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip"
  dest <- paste0('mtbs/mtbs_data.zip')
  download.file(loc, dest)
  unzip(dest)
  unlink(dest)
  convert('mtbs/mtbs_fod_pts_DD.dbf', 'mtbs/mtbs_data.csv')
  #remove unnecessary files from zip
  file.remove('mtbs/mtbs_fod_pts_DD.cpg')
  file.remove('mtbs/mtbs_fod_pts_DD.html')
  file.remove('mtbs/mtbs_fod_pts_DD.prj')
  file.remove('mtbs/mtbs_fod_pts_DD.sbn')
  file.remove('mtbs/mtbs_fod_pts_DD.sbx')
  file.remove('mtbs/mtbs_fod_pts_DD.shp')
  file.remove('mtbs/mtbs_fod_pts_DD.shp.xml')
  file.remove('mtbs/mtbs_fod_pts_DD.shx')
  file.remove('mtbs/mtbs_fod_pts_DD.dbf')
}

#create firedata dataframe
firedata <- read.csv('mtbs/mtbs_data.csv')

#convert ignition date variable from factor to date and store values numerically
firedata$IG_DATE <- as.Date(firedata$Ig_Date)
firedata$IG_YEAR <- as.numeric(format(firedata$IG_DATE, '%Y'))
firedata$IG_MONTH <- as.numeric(format(firedata$IG_DATE, '%m'))
firedata$IG_DAY <- as.numeric(format(firedata$IG_DATE, '%d'))

#download stations file
if(!file.exists('mtbs/stations.txt')){
  download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 'mtbs/stations.txt')
}

#download inventory file
if(!file.exists('mtbs/inventory.txt')){
  download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 'mtbs/inventory.txt')
}

#create stations dataframe
typedcols <- c( "A11", "F9", "F10", "F7", "X1","A2",
                "X1","A30", "X1", "A3", "X1", "A3", "X1", "A5" )
stations <- read.fortran("mtbs/stations.txt",
                         typedcols, 
                         comment.char="")
hdrs <- c("ID", "LAT", "LON", "ELEV", "ST", "NAME","GSN", "HCN", "WMOID")
names(stations) <- hdrs

#create inventory dataframe
invcols <- c( "A11", "X1", "F8", "X1", "F9", "X1","A4",
              "X1","I4", "X1", "I4" )
inv <- read.fortran("mtbs/inventory.txt",
                    invcols,
                    comment.char="")
invhdrs <- c("ID", "LAT", "LON", "ELEM" , "FIRST", "LAST")
names(inv) <- invhdrs

#only include inventory rows for desired observations
inv$ELEM <- as.factor(inv$ELEM)
inv <- inv[which(inv$ELEM == "TMAX" | inv$ELEM == "TMIN" | inv$ELEM == "PRCP"),]

##############
#Modify so the user can input the desired observation variables
##############

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
#BUG: this values are NULL
ptm <- proc.time()
for (stn in 1:nrow(stationsUS)){
  if (stn%%100 == 0){
    print(stn)
  }
  stationsUS$startTMAX <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMAX"),]$FIRST[1]
  stationsUS$endTMAX <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMAX"),]$LAST[1]
  stationsUS$startTMIN <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMIN"),]$FIRST[1]
  stationsUS$endTMIN <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "TMIN"),]$LAST[1]
  stationsUS$startPRCP <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "PRCP"),]$FIRST[1]
  stationsUS$endPRCP <- inv[which(inv$ID == stationsUS[stn,]$ID && inv$ELEM == "PRCP"),]$FIRST[1]
}
proc.time()-ptm
#here


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

#remove all fires that are not wildfires
firedata <- firedata[which(firedata$Fire_Type == "Wildfire"),]

#add fields to mtbs firedata
#dummy station created with coordinates outside the US
firedata$closestStnID <- '0'
firedata$closestStnID <- "dummy station"
firedata$closestStnLong <- 90
firedata$closestStnLat <- -30

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
firedata$closestStnDist <- 0
for (row in 1:nrow(firedata)){
  if (row %% 10 == 0) {print(row)}
  for (stn in 1:nrow(stationsUS)){
    if (firedata$IG_YEAR[row] <= stationsUS$LAST[stn] & firedata$IG_YEAR[row] >= stationsUS$FIRST[stn] & earth.dist(firedata$Long[row], firedata$Lat[row], stationsUS$LON[stn], stationsUS$LAT[stn]) < 
        earth.dist(firedata$Long[row], firedata$Lat[row], firedata$closestStnLong[row], firedata$closestStnLat[row])){
      firedata$closestStnDist[row] <- earth.dist(firedata$Long[row], firedata$Lat[row],  stationsUS$LON[stn],stationsUS$LAT[stn])
      firedata$closestStnID[row] <- stationsUS$ID[stn]
      firedata$closestStnLong[row] <-  stationsUS$LON[stn]
      firedata$closestStnLat[row] <-  stationsUS$LAT[stn]
    }
  }
}
proc.time()-ptm
##########
#edit this to properly convert from factor to chr
#then try noaaExtract fn
firedata$closestStnID2 <- toString(firedata$closestStnID)
#########

#save closest station data
write.csv(firedata, "mtbs/mtbs_data_stn.csv")

#to upload the dataframe, if the mtbs_data.csv file has already been created
firedata <- read.csv("mtbs/mtbs_data_stn.csv")

#create a vector of the closest stations
listOfStns <- firedata$closestStnID
listOfStns <- listOfStns[listOfStns != '0']
listOfStns <- unique(listOfStns)

#create noaa data directories
if(!dir.exists("noaa")){
  dir.create("noaa")
}
if(!dir.exists("noaa/noaaout")){
  dir.create("noaa/noaaout")
}
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

######################################
#In downloading the noaa weather files, I have noticed many corrupted files that have
#html code fragments in the csv file, instead of the data values.
#Run these code blocks to identify if there are any corrupted files 
#and to redownload them.

corruptedFiles <- vector()
#identify which files are corrupted
for (i in 1:length(listOfStns)){
  print(i)
  if(file.exists(paste0('noaa/noaaout/', listOfStns[i], ".csv"))){
    df<-read.csv(paste0('noaa/noaaout/', listOfStns[i], ".csv"))
    if (!"TMAX" %in% df$element | !"TMIN" %in% df$element | !toString(listOfStns[i])  %in% df$id){
      corruptedFiles <- c(corruptedFiles, toString(listOfStns[i]))
    }
  }else{
    print(paste0("file missing: ", istOfStns[i]))
  }
}

while(length(corruptedFiles) > 0){
print(paste0("number of corrupted files: ", length(corruptedFiles)))
#download corrupted files
  for(str in corruptedFiles){
    outfile <- paste0('noaa/noaaout/', str, ".csv")
    df<-ghcnd(str, refresh=TRUE)
    write.csv(df, outfile) 
    print(paste0(i, ": ", str))
  }
  #identify which files are corrupted
  for (i in 1:length(listOfStns)){
    if(file.exists(paste0('noaa/noaaout/', listOfStns[i], ".csv"))){
      df<-read.csv(paste0('noaa/noaaout/', listOfStns[i], ".csv"))
      if (!"TMAX" %in% df$element | ! "TMIN" %in% df$element | !listOfStns[i]  %in% df$id){
        corruptedFiles <- c(corruptedFiles, toString(listOfStns[i]))
      }
    }
  }
}
########################################
#end of code blocks to remove corrupted files

#Define function to extract weather data and store in firedata dataframe
#ELMn refers to the ELM element n days before the fire
#Ex: TMAX0 refers to the TMAX the day of the fire

#######better version of noaaExtract:
noaaExtract <- function(stn, date, elm){
  #upload dataframe, select rows of the given year and month
  if(!file.exists(paste0("noaa/noaaout/", stn, ".csv"))){
    print(paste0("Fill missing: ", stn))
    return(-9999)
  }
  if(file.exists(paste0("noaa/noaaout/", stn, ".csv"))){
    df <- read.csv(paste0("noaa/noaaout/", stn, ".csv"))
    df <- df[which(df$year == year(date)),]
    df <- df[which(df$month == month(date)),]
    df <- df[which(df$element == elm),]
    day = day(date)
    col_name <- paste0("VALUE", day)
    df <- select(df, col_name)
    out <- df[1,1]
    return(out)
  }
}

vNoaaExtract <- Vectorize(noaaExtract, vectorize.args = c("stn", "date"))
#convert ID names from factors to strings
firedata$closestStnID2 <- sapply(firedata$closestStnID, as.character)

firedata$TMAX0 <- 0
firedata$TMIN0 <- 0
firedata$PRCP0 <- 0
firedata$TMAX0 <- vNoaaExtract(firedata$closestStnID2, firedata$IG_DATE, "TMAX")
firedata$TMIN0 <- vNoaaExtract(firedata$closestStnID2, firedata$IG_DATE, "TMIN")
firedata$PRCP0 <- vNoaaExtract(firedata$closestStnID2, firedata$IG_DATE, "TMAX")

#these three loops no longer needed, superseeded by above vectorized code
for(i in 1:nrow(firedata)){
  firedata$TMAX0[i]<- noaaExtract(firedata$closestStnID2[i], firedata$IG_DATE[i], "TMAX")
  print(i)
}

for(i in 1:nrow(firedata)){
  firedata$TMIN0[i]<- noaaExtract(firedata$closestStnID2[i], firedata$IG_DATE[i], "TMIN")
  print(i)
  
}

for(i in 1:nrow(firedata)){
  firedata$PRCP0[i]<- noaaExtract(firedata$closestStnID2[i], firedata$IG_DATE[i], "PRCP")
  print(i)
  
}

#Include data values of 1 day before the fire:
#Eventually figure out how to loop this data
firedata$TMAX1 <- 0
firedata$TMIN1 <- 0
firedata$PRCP1 <- 0
firedata$elm <- "TMAX"
#firedata$TMAX1 <- noaaExtract(toString(firedata$closestStnID), firedata$IG_DATE, firedata$elm)
firedata$elm <- "TMIN"
#firedata$TMIN1 <- noaaExtract(toString(firedata$closestStnID), firedata$IG_DATE, firedata$elm)
firedata$elm <- "PRCP"
#firedata$PRCP1 <- noaaExtract(toString(firedata$closestStnID), firedata$IG_DATE, firedata$elm)

for(i in 1:nrow(firedata)){
  firedata$TMAX0[i]<- noaaExtract(firedata$closestStnID2[i], as.Date(firedata$IG_DATE[i])-1, "TMAX")
  print(paste0("TMAX1 ", i))
}

for(i in 1:nrow(firedata)){
  firedata$TMIN0[i]<- noaaExtract(firedata$closestStnID2[i], as.Date(firedata$IG_DATE[i])-1, "TMIN")
  print(paste0("TMIN1 ", i))
  
}

for(i in 1:nrow(firedata)){
  firedata$PRCP0[i]<- noaaExtract(firedata$closestStnID2[i], as.Date(firedata$IG_DATE[i])-1, "PRCP")
  print(paste0("PRCP1 ", i))
  
}

write.csv(firedata, "mtbs/mtbs_data_stn.csv")

firedata <- read.csv("mtbs/mtbs_data_stn.csv")

#only analyze fires whose closest weather station is within 8 km (~5 miles)
#this includes 1,617 fires
firedataDist <- firedata[which(firedata$closestStnDist < 8),]


#Data visualizations:

#TMAX vs ACRES
firedataTMAX <- firedataDist[which(!is.na(firedataDist$TMAX0)),]
attach(firedataTMAX)
summary(TMAX0)

#plot with outliers
plot(TMAX0, Acres)
abline(lm(Acres~TMAX0), col="red") # regression line (y~x)
lines(lowess(TMAX0,Acres, delta = 0.01), col="blue") # lowess line (x,y)
cor(TMAX0, Acres)
#returns 0.0220592, so TMAX and Acres are very very weakly correlated
#there are several outliers visible here corresponding to fires that burned particularly large areas
#note that most of these outliers occured when the temperature max was above the median

#to better visualize the trends, we remove from the dataset all fires with more than 6,000 total acres burned
detach(firedataTMAX)
firedataTmaxAcres<-firedataTMAX[which(firedataTMAX$Acres < 6000),]
attach(firedataTmaxAcres)
summary(TMAX0)
plot(TMAX0, Acres)
abline(lm(Acres~TMAX0), col="red") # regression line (y~x)
lines(lowess(TMAX0,Acres, delta = 0.01), col="blue") # lowess line (x,y)
#TMAX and ACRES are very weakly correlated, even when the outliers are removed:
cor(TMAX0, Acres)
#returns 0.104622

#TMIN VS ACRES
firedataTMIN <- firedataDist[which(!is.na(firedataDist$TMAX0)),]