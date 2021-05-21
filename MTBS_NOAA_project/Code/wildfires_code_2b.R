library(rnoaa)
library(dplyr)

files <- list.files(path="Data/", pattern="firedata2a_[0123456789]+.csv", recursive=FALSE)
iteration <- 1
for(file in files){
  fileval <- gsub("firedata2a_", "", file)
  fileval <- gsub(".csv", "", fileval)
  fileval <- as.integer(fileval)
  if(fileval > iteration){
    iteration <- fileval
  }
}
firedata <- read.csv(paste0("Data/firedata2a_", iteration, ".csv"), stringsAsFactors = FALSE)

#firedata <- firedata[1:200,]

firedata <- firedata %>% select(!starts_with("X"))
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
    print(paste0("written file ", listOfStns[i]))
    
  }
  #print(i)
}


write.csv(firedata, paste0("Data/firedata2b_", iteration, ".csv"))

#make sure none of the files are empty
#not sure how but some files are becoming empty. Redownloading seems to fix it.
for(i in 1:length(listOfStns)){
  #print(i)
  if(file.info(paste0("Data/noaa/noaadata/", listOfStns[i], ".csv"))$size <= 4){
    outfile <- paste0('Data/noaa/noaadata/updated', listOfStns[i], ".csv")
    df<-ghcnd(listOfStns[i], refresh=TRUE)
    write.csv(df, outfile) 
    print(paste0("written file ", listOfStns[i]))
  }
}
#Sequential time elapsed: 243.17 
#Time elapsed with dopar: 228.93
#With registerDoMC: 228.93
#With registerDoParallel: 78.65 s
