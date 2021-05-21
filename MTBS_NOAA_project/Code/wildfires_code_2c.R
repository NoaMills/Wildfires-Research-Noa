library(foreach)
library(lubridate)
library(doParallel)
library(doMC)
library(dplyr)
library(matrixStats)
#library(tidyverse)

stationsUS <- read.csv("Data/noaa/stationsUS.csv")

files <- list.files(path="Data/", pattern="firedata2b_[0123456789]+.csv", recursive=FALSE)
iteration <- 1
for(file in files){
  fileval <- gsub("firedata2b_", "", file)
  fileval <- gsub(".csv", "", fileval)
  fileval <- as.integer(fileval)
  if(fileval > iteration){
    iteration <- fileval
  }
}
firedata <- read.csv(paste0("Data/firedata2b_", iteration, ".csv"), stringsAsFactors = FALSE)

#firedata <- firedata[10000:10200,]
#firedata$Update[1:20500] <- FALSE
firedata <- firedata %>% select(!starts_with("X"))

noaaExtractRow <- function(stn, date, elm){
  if(!file.exists(paste0("Data/noaa/noaadata/", stn, ".csv"))){
    print(paste0("File missing: ", stn))
    return(data.frame(matrix(nrow=0, ncol=0)))
  }else{
    df <- read.csv(paste0("Data/noaa/noaadata/", stn, ".csv"))
    df <- df[which(df$element == elm),]
    df_out <- data.frame(matrix(nrow=57, ncol=2))
    names(df_out) <- c("Index", elm)
    df_out$Index <- -21:35
    for(i in -21:35){
      date_pull <- as.Date(date) + lubridate::days(i)
      dfi <- df[which(df$year == lubridate::year(date) & df$month == lubridate::month(date)),]
      day <- lubridate::day(date_pull)
      flag_col_name <- paste0("QFLAG", day)
      dfFlag <- dplyr::select(dfi, all_of(flag_col_name))
      if(!is.na(dfFlag[1,1]) & as.character(dfFlag[1,1]) != "" & as.character(dfFlag[1,1]) != " "){
        #print(paste0("Flagged: ", stn, " ", date, " ", elm))
        df_out[which(df_out$Index == i),2] == "flagged"
      }else{#if no quality flags, return the observation value
        value_col_name <- paste0("VALUE", day)
        dfval <- dplyr::select(dfi, all_of(value_col_name))
        out <- dfval[1,1]
        #print(out)
        df_out[which(df_out$Index == i),2] <- as.numeric(out)
      }
    }
  }
  return(df_out)
}

for(i in -21:35){
  if(i < 0){
    varnameTMAX <- paste0("TMAX.", -i)
    varnameTMIN <- paste0("TMIN.", -i)
    varnamePRCP <- paste0("PRCP.", -i)
  }else{
    varnameTMAX <- paste0("TMAX", i)
    varnameTMIN <- paste0("TMIN", i)
    varnamePRCP <- paste0("PRCP", i)
  }
  if(varnameTMAX %in% names(firedata)){
    if(nrow(firedata[which(firedata$UpdateTMAX == TRUE),]) > 0){
      firedata[which(firedata$UpdateTMAX == TRUE),][[varnameTMAX]] <- NA
    }
    if(nrow(firedata[which(firedata$UpdateTMIN == TRUE),]) > 0){
      firedata[which(firedata$UpdateTMIN == TRUE),][[varnameTMIN]] <- NA
    }
    if(nrow(firedata[which(firedata$UpdatePRCP == TRUE),]) > 0){
      firedata[which(firedata$UpdatePRCP == TRUE),][[varnamePRCP]] <- NA
    }
  }else{
    #print(paste0("else ", i))
    firedata[[varnameTMAX]] <- NA
    firedata[[varnameTMIN]] <- NA
    firedata[[varnamePRCP]] <- NA
  }
}

firedataToUpdate <- firedata[which(firedata$UpdateTMAX == TRUE | 
                                     firedata$UpdateTMIN == TRUE | firedata$UpdatePRCP == TRUE),]


#First, extract data from firedataToUpdate to firedataOut
#Then, transfer updated data to firedata

#foreach (row in 1:nrow(firedata)) %doPar% 
#no_cores <- detectCores()
#cl <- makeCluster(no_cores)
registerDoParallel()
ptm <- proc.time()
firedataOut <- foreach (row=1:nrow(firedataToUpdate), .combine=rbind) %dopar% {
  newrow <- firedataToUpdate[row,]
  print(paste0("Row ", row, " of ", nrow(firedataToUpdate)))
  #TMAX
  tmaxDF <- noaaExtractRow(firedataToUpdate$closestStnID_TMAX[row], firedataToUpdate$IG_DATE[row], "TMAX")
  for(i in -21:35){
    if(i < 0){
      varnameTMAX <- paste0("TMAX.", -i)
    }else{
      varnameTMAX <- paste0("TMAX", i)
    }
    newrow[[varnameTMAX]] <- tmaxDF[which(tmaxDF$Index == i),]$TMAX
  }
  #TMIN
  tminDF <- noaaExtractRow(firedataToUpdate$closestStnID_TMIN[row], firedataToUpdate$IG_DATE[row], "TMIN")
  for(i in -21:35){
    if(i < 0){
      varnameTMIN <- paste0("TMIN.", -i)
    }else{
      varnameTMIN <- paste0("TMIN", i)
    }
    newrow[[varnameTMIN]] <- tminDF[which(tminDF$Index == i),]$TMIN
  }
  #PRCP
  prcpDF <- noaaExtractRow(firedataToUpdate$closestStnID_PRCP[row], firedataToUpdate$IG_DATE[row], "PRCP")
  for(i in -21:35){
    if(i < 0){
      varnamePRCP <- paste0("PRCP.", -i)
    }else{
      varnamePRCP <- paste0("PRCP", i)
    }
    newrow[[varnamePRCP]] <- prcpDF[which(prcpDF$Index == i),]$PRCP
  }
  newrow
}
proc.time()-ptm

#write.csv(firedataOut, "Data/firedataOutTest.csv")

#Local, 20 rows: 7.85
#Local, 200 rows: elapsed = 78.34
#Local, 500 rows: elapsed = 169.78, 133.57

#firedata <- firedata %>% select(!starts_with("X"))

#print the number of flags for each element before the flags are updated
#nrow(firedata[which(firedata$flagTMAX >= minFlag | firedata$naTMAX >= minNA),])
#nrow(firedata[which(firedata$flagTMIN >= minFlag | firedata$naTMIN >= minNA),])
#nrow(firedata[which(firedata$flagPRCP >= minFlag | firedata$naPRCP >= minNA),])

for(row in 1:nrow(firedataOut)){
  #print(nrow(firedata[which(firedata$Event_ID == firedataOut$Event_ID[row]),]))
  print(paste0("Incorporating updated data with original data frame. Row ", row, " of ", nrow(firedataOut)))
  firedata[which(firedata$Event_ID == firedataOut$Event_ID[row]),] <- firedataOut[row,]
  #print(row)
}


#firedata now has updated values.

firedata$flagTMAX <- rowSums(firedata %>% select(starts_with("TMAX")) == "flagged", na.rm = TRUE)
firedata$flagTMIN <- rowSums(firedata %>% select(starts_with("TMIN")) == "flagged", na.rm = TRUE)
firedata$flagPRCP <- rowSums(firedata %>% select(starts_with("PRCP")) == "flagged", na.rm = TRUE)
firedata$naTMAX <- rowSums(is.na(firedata %>% select(starts_with("TMAX"))), na.rm = FALSE)
firedata$naTMIN <- rowSums(is.na(firedata %>% select(starts_with("TMIN"))), na.rm = FALSE)
firedata$naPRCP <- rowSums(is.na(firedata %>% select(starts_with("PRCP"))), na.rm = FALSE)

minFlag = 10
minNA = 15
maxDist = 250
firedata$UpdateTMAX <- FALSE
firedata$UpdateTMIN <- FALSE
firedata$UpdatePRCP <- FALSE

if(nrow(firedata[which((firedata$flagTMAX >= minFlag | firedata$naTMAX >= minNA) & firedata$closestStnDist_TMAX <= maxDist),]) > 0){
  firedata[which((firedata$flagTMAX >= minFlag | firedata$naTMAX >= minNA) & firedata$closestStnDist_TMAX <= maxDist),]$UpdateTMAX <- TRUE
  print("TMAX")
}
if(nrow(firedata[which((firedata$flagTMIN >= minFlag | firedata$naTMIN >= minNA) & firedata$closestStnDist_TMIN <= maxDist),]) > 0){
  firedata[which((firedata$flagTMIN >= minFlag | firedata$naTMIN >= minNA) & firedata$closestStnDist_TMIN <= maxDist),]$UpdateTMIN <- TRUE
  print("TMIN")
  }
if(nrow(firedata[which((firedata$flagPRCP >= minFlag | firedata$naPRCP >= minNA) & firedata$closestStnDist_PRCP <= maxDist),]) > 0){
  firedata[which((firedata$flagPRCP >= minFlag | firedata$naPRCP >= minNA) & firedata$closestStnDist_PRCP <= maxDist),]$UpdatePRCP <- TRUE
  print("PRCP")
}

#Update minimum distances to ensure that the same station isn't checked twice
firedata[which(firedata$UpdateTMAX == TRUE),]$minDistTMAX <- 
  firedata[which(firedata$UpdateTMAX == TRUE),]$closestStnDist_TMAX
firedata[which(firedata$UpdateTMIN == TRUE),]$minDistTMIN <- 
  firedata[which(firedata$UpdateTMIN == TRUE),]$closestStnDist_TMIN
firedata[which(firedata$UpdatePRCP == TRUE),]$minDistPRCP <- 
  firedata[which(firedata$UpdatePRCP == TRUE),]$closestStnDist_PRCP

print(paste0("UpdateTMAX ", nrow(firedata[which(firedata$UpdateTMAX == TRUE),])))
print(paste0("UpdateTMIN ", nrow(firedata[which(firedata$UpdateTMIN == TRUE),])))
print(paste0("UpdatePRCP ", nrow(firedata[which(firedata$UpdatePRCP == TRUE),])))
write.csv(firedata, paste0("Data/firedata2c_", iteration, ".csv"))
print(paste0("Done, 2c, iteration: ", iteration))
