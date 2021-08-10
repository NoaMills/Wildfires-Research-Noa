library(foreach)
library(doParallel)
library(doMC)
library(lubridate)
library(dplyr)
library(ggplot2)

firedata <- read.csv("Data/firedata6.csv")
firedata$IG_DATE <- as.Date(firedata$IG_DATE)

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


#define function to identify the time between two burns if they are sufficiently close
distTSLB10 <- function(date1, lon1, lat1, date2, lon2, lat2){
  #fire 1 is a given fire, and we're trying to find the most recent burn near fire 1
  if(date1 > date2 & earth.dist(lon1,lat1,lon2,lat2) <= 10){
    return(interval(date2,date1) %/% days(1))
  }else{
    return(NA)
  }
}

distTSLB25 <- function(date1, lon1, lat1, date2, lon2, lat2){
  #fire 1 is a given fire, and we're trying to find the most recent burn near fire 1
  if(date1 > date2 & earth.dist(lon1,lat1,lon2,lat2) <= 25){
    return(interval(date2,date1) %/% days(1))
  }else{
    return(NA)
  }
}

DistTSLB10 <- Vectorize(distTSLB10, vectorize.args=c("date2", "lon2", "lat2"), USE.NAMES=FALSE)
DistTSLB25 <- Vectorize(distTSLB25, vectorize.args=c("date2", "lon2", "lat2"), USE.NAMES=FALSE)

#practice:
#firedata2 <- firedata[1:300,]
numRows <- nrow(firedata)
firedata$TSLB10 <- NA
firedata$TSLB25 <- NA
registerDoParallel()
firedata <- foreach (row=1:numRows, .combine=rbind) %dopar% {
  newrow <- firedata[row,]
  TSLB_10 <- DistTSLB10(newrow$IG_DATE[1], newrow$BurnBndLon[1], newrow$BurnBndLat[1],
                        firedata$IG_DATE[1:numRows], firedata$BurnBndLon[1:numRows], firedata$BurnBndLat[1:numRows])
  TSLB_25 <- DistTSLB25(newrow$IG_DATE[1], newrow$BurnBndLon[1], newrow$BurnBndLat[1],
                        firedata$IG_DATE[1:numRows], firedata$BurnBndLon[1:numRows], firedata$BurnBndLat[1:numRows])
  if(all(is.na(TSLB_10))){
    tslb10 <- NA
  }else{
    tslb10 <- min(TSLB_10, na.rm=TRUE)
  }
  if(all(is.na(TSLB_25))){
    tslb25 <- NA
  }else{
    tslb25 <- min(TSLB_25, na.rm=TRUE)
  }
  newrow$TSLB10[1] <- tslb10
  newrow$TSLB25[1] <- tslb25
  if(!is.na(tslb10) | !is.na(tslb25)){
    print(paste0("Row: ", row, " tslb10: ", tslb10, " tslb25: ", tslb25))
  }else{
    print(row)
  }
  newrow
}

#Create bin variables to categorize time since last burn
firedata$tslbNA <- FALSE #make true if both tslb25 and tslb10 are NA
firedata$tslb25_750 <- FALSE #make true if tslb25 < 750
firedata$tslb10_750 <- FALSE #make true if tslb10 < 750
firedata$tslb25_250 <- FALSE
firedata$tslb10_250 <- FALSE
firedata$tslb10_100 <- FALSE

firedata[which(is.na(firedata$TSLB10) & is.na(firedata$TSLB25)),]$tslbNA <- TRUE
firedata[which(firedata$TSLB25 < 750),]$tslb25_750 <- TRUE
firedata[which(firedata$TSLB10 < 750),]$tslb10_750 <- TRUE
firedata[which(firedata$TSLB25 < 250),]$tslb25_250 <- TRUE
firedata[which(firedata$TSLB10 < 250),]$tslb10_250 <- TRUE
firedata[which(firedata$TSLB10 < 100),]$tslb10_100 <- TRUE

#or try binning into non-binary categories
firedata<-firedata%>%mutate(TSLB10bins = cut(TSLB10, breaks = c(-Inf, 20,100,300,500,1000,2000, Inf)))
firedata%>%group_by(TSLB10bins)%>%count()
firedata<-firedata%>%mutate(TSLB25bins = cut(TSLB25, breaks = c(-Inf, 20,100,300,500,1000,2000, Inf)))
firedata%>%group_by(TSLB25bins)%>%count()

#create dummy variables for bins
firedata$TSLB10_inf_20 <- 0
firedata$TSLB10_20_100 <- 0
firedata$TSLB10_100_300 <- 0
firedata$TSLB10_300_500 <- 0
firedata$TSLB10_500_1000 <- 0
firedata$TSLB10_1000_2000 <- 0
firedata$TSLB10_2000_inf <- 0
firedata$TSLB10_na <- 0

firedata$TSLB25_inf_20 <- 0
firedata$TSLB25_20_100 <- 0
firedata$TSLB25_100_300 <- 0
firedata$TSLB25_300_500 <- 0
firedata$TSLB25_500_1000 <- 0
firedata$TSLB25_1000_2000 <- 0
firedata$TSLB25_2000_inf <- 0
firedata$TSLB25_na <- 0

firedata[which(firedata$TSLB10bins == "(-Inf,20]"),]$TSLB10_inf_20 <- 1
firedata[which(firedata$TSLB10bins == "(20,100]"),]$TSLB10_20_100 <- 1
firedata[which(firedata$TSLB10bins == "(100,300]"),]$TSLB10_100_300 <- 1
firedata[which(firedata$TSLB10bins == "(300,500]"),]$TSLB10_300_500 <- 1
firedata[which(firedata$TSLB10bins == "(500,1e+03]"),]$TSLB10_500_1000 <- 1
firedata[which(firedata$TSLB10bins == "(1e+03,2e+03]"),]$TSLB10_1000_2000 <- 1
firedata[which(firedata$TSLB10bins == "(2e+03, Inf]"),]$TSLB10_2000_inf <- 1
firedata[which(is.na(firedata$TSLB10bins)),]$TSLB10_na <- 1

firedata[which(firedata$TSLB25bins == "(-Inf,20]"),]$TSLB25_inf_20 <- 1
firedata[which(firedata$TSLB25bins == "(20,100]"),]$TSLB25_20_100 <- 1
firedata[which(firedata$TSLB25bins == "(100,300]"),]$TSLB25_100_300 <- 1
firedata[which(firedata$TSLB25bins == "(300,500]"),]$TSLB25_300_500 <- 1
firedata[which(firedata$TSLB25bins == "(500,1e+03]"),]$TSLB25_500_1000 <- 1
firedata[which(firedata$TSLB25bins == "(1e+03,2e+03]"),]$TSLB25_1000_2000 <- 1
firedata[which(firedata$TSLB25bins == "(2e+03, Inf]"),]$TSLB25_2000_inf <- 1
firedata[which(is.na(firedata$TSLB25bins)),]$TSLB25_na <- 1

ggplot(firedata, aes(x = TSLB10bins, y = log(BurnBndAc))) + geom_boxplot() 
ggplot(firedata, aes(x = TSLB25bins, y = log(BurnBndAc))) + geom_boxplot()

write.csv(firedata, "Data/firedata7.csv")
