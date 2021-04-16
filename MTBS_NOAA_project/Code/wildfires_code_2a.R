#wildfires_code_2.R section 1, to be run on local computer
#Downloads weather data based on closest stations
#then run script file_transfer_1 to transfer code and data to remote repo
#section 2 to be run on hummingbird

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

if(!dir.exists("Data/noaa/noaadata")){
  dir.create("Data/noaa/noaadata")
}
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