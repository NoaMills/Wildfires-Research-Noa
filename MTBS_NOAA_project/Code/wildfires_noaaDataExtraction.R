library(foreach)
library(lubridate)
library(doParallel)
library(doMC)
library(dplyr)
#library(tidyverse)

firedata <- read.csv("Data/firedata1.csv")
firedata <- firedata[1:200,]


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
  firedata[[varnameTMAX]] <- NA
  firedata[[varnameTMIN]] <- NA
  firedata[[varnamePRCP]] <- NA
}
#Extract data to firedata
#foreach (row in 1:nrow(firedata)) %doPar% 
no_cores <- detectCores()
#cl <- makeCluster(no_cores)
registerDoParallel()
ptm <- proc.time()
firedataOut <- foreach (row=1:nrow(firedata), .combine=rbind) %dopar% {
  newrow <- firedata[row,]
  #print(row)
  #TMAX
  tmaxDF <- noaaExtractRow(firedata$closestStnID_TMAX[row], firedata$IG_DATE[row], "TMAX")
  for(i in -21:35){
    if(i < 0){
      varnameTMAX <- paste0("TMAX.", -i)
    }else{
      varnameTMAX <- paste0("TMAX", i)
    }
    newrow[[varnameTMAX]] <- tmaxDF[which(tmaxDF$Index == i),]$TMAX
  }
  #TMIN
  tminDF <- noaaExtractRow(firedata$closestStnID_TMIN[row], firedata$IG_DATE[row], "TMIN")
  for(i in -21:35){
    if(i < 0){
      varnameTMIN <- paste0("TMIN.", -i)
    }else{
      varnameTMIN <- paste0("TMIN", i)
    }
    newrow[[varnameTMIN]] <- tminDF[which(tminDF$Index == i),]$TMIN
  }
  #TMPRCP
  prcpDF <- noaaExtractRow(firedata$closestStnID_PRCP[row], firedata$IG_DATE[row], "PRCP")
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

write.csv(firedataOut, "Data/firedata2.csv")

#Sequential time elapsed: 243.17 
#Time elapsed with dopar: 228.93
#With registerDoMC: 228.93
#With registerDoParallel: 78.65 s