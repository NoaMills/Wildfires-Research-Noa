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


firedata <- read.csv("Data/firedata2.csv")


#########
#Section 1: Identify state of each fire. Group fires by state regions with common climates as described by NOAA: https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php
#########


#Merge the attribute levels of Assessment type so "Initial" and "Initial (SS)" are both stored as "Initial".

firedata[which(firedata$Asmnt_Type == "Initial (SS)"),]$Asmnt_Type <- "Initial"
firedata[which(firedata$Asmnt_Type == "Extended (SS)"),]$Asmnt_Type <- "Extended"

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
states_and_territories %>%
  filter(!NAME %in% c("American Samoa","Guam", "Northern Marianas", "Virgin Islands"))

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

########
#Section 2
########

#The assessment type (Initial vs Extended) was determined based on the type of ecosystem.
#Extended assessments were used for forests, woodlands, shrublands, etc. where wildfires have longer lasting impacts
#Initial assessments were used for grasslands where ecosystems tend to recover by the next growing season

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
#PR = Puerto Rico
#HI = Hawaii

#Create dummy variables for categorical variables.

#Dummy variables for fire type
firedata$isRx <- 0
firedata$isWf <- 0
firedata$isWFU <- 0
firedata[which(firedata$Incid_Type == "Wildfire"),]$isWf <- 1
firedata[which(firedata$Incid_Type == "Prescribed Fire"),]$isRx <- 1
firedata[which(firedata$Incid_Type == "Wildland Fire Use"),]$isWFU <- 1

#Dummy variables for assessment type
firedata$isInitial <- 0
firedata$isExtended <- 0
firedata[which(firedata$Asmnt_Type == "Initial"),]$isInitial <- 1
firedata[which(firedata$Asmnt_Type == "Extended"),]$isExtended <- 1

#Dummy variables for region
firedata$isSW <- 0
firedata$isSE <- 0
firedata$isS <- 0
firedata$isW <- 0
firedata$isNRP <- 0
firedata$isOV <- 0
firedata$isNW <- 0
firedata$isNE <- 0
firedata$isUM <- 0
firedata$isAL <- 0
firedata$isPR <- 0
firedata$isHI <- 0
firedata[which(firedata$region == "Southwest"),]$isSW <- 1
firedata[which(firedata$region == "Southeast"),]$isSE <- 1
firedata[which(firedata$region == "South"),]$isS <- 1
firedata[which(firedata$region == "West"),]$isW <- 1
firedata[which(firedata$region == "Northern Rockies and Plains"),]$isNRP <- 1
firedata[which(firedata$region == "Ohio Valley"),]$isOV <- 1
firedata[which(firedata$region == "Northwest"),]$isNW <- 1
firedata[which(firedata$region == "Northeast"),]$isNE <- 1
firedata[which(firedata$region == "Upper Midwest"),]$isUM <- 1
firedata[which(firedata$region == "Alaska"),]$isAL <- 1
firedata[which(firedata$region == "Puerto Rico"),]$isPR <- 1
firedata[which(firedata$region == "Hawaii"),]$isHI <- 1

#Dummy variables for month
firedata$isJan <- 0
firedata$isFeb <- 0
firedata$isMar <- 0
firedata$isApr <- 0
firedata$isMay <- 0
firedata$isJun <- 0
firedata$isJul <- 0
firedata$isAug <- 0
firedata$isSep <- 0
firedata$isOct <- 0
firedata$isNov <- 0
firedata$isDec <- 0
firedata[which(firedata$IG_MONTH == 1),]$isJan <- 1
firedata[which(firedata$IG_MONTH == 2),]$isFeb <- 1
firedata[which(firedata$IG_MONTH == 3),]$isMar <- 1
firedata[which(firedata$IG_MONTH == 4),]$isApr <- 1
firedata[which(firedata$IG_MONTH == 5),]$isMay <- 1
firedata[which(firedata$IG_MONTH == 6),]$isJun <- 1
firedata[which(firedata$IG_MONTH == 7),]$isJul <- 1
firedata[which(firedata$IG_MONTH == 8),]$isAug <- 1
firedata[which(firedata$IG_MONTH == 9),]$isSep <- 1
firedata[which(firedata$IG_MONTH == 10),]$isOct <- 1
firedata[which(firedata$IG_MONTH == 11),]$isNov <- 1
firedata[which(firedata$IG_MONTH == 12),]$isDec <- 1
firedata$isFireSeason <- 0
firedata[which(firedata$IG_MONTH %in% c(7,8,9,10)),]$isFireSeason <- 1

#Dummy variables for year
#Separate into groups by 5 years
#84-88, 89-93, 94-98, 99-03, 04-08, 09-13, 14-18
firedata$year_group <- ""
firedata[which(firedata$IG_YEAR >=1984 & firedata$IG_YEAR <= 1988),]$year_group <- "1984-1988"
firedata[which(firedata$IG_YEAR >=1989 & firedata$IG_YEAR <= 1993),]$year_group <- "1989-1993"
firedata[which(firedata$IG_YEAR >=1994 & firedata$IG_YEAR <= 1998),]$year_group <- "1994-1998"
firedata[which(firedata$IG_YEAR >=1998 & firedata$IG_YEAR <= 2003),]$year_group <- "1998-2003"
firedata[which(firedata$IG_YEAR >=2004 & firedata$IG_YEAR <= 2008),]$year_group <- "2004-2008"
firedata[which(firedata$IG_YEAR >=2009 & firedata$IG_YEAR <= 2013),]$year_group <- "2009-2013"
firedata[which(firedata$IG_YEAR >=2014 & firedata$IG_YEAR <= 2018),]$year_group <- "2014-2018"

write.csv(firedata, "Data/firedata3.csv")