#Code to practice parallel computing

library(foreach)
library(doParallel)
library(doMC)

pbirthdaysim <- function(n){
  ntests <- 100000
  pop <- 1:365
  anydup <- function(i)
    any(duplicated(sample(pop,n,replace=TRUE)))
  sum(sapply(seq(ntests), anydup))/ntests
}

system.time(bdayp <- sapply(1:100, pbirthdaysim))

system.time(
  x <- foreach (n=1:100) %dopar% pbirthdaysim(n)
)


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
vNoaaExtract <- Vectorize(noaaExtract, vectorize.args = c("stn", "date"), SIMPLIFY = TRUE, USE.NAMES = FALSE)

firedata <- read.csv("Data/firedata1.csv")[1:50,]
registerDoMC()
ptm <- proc.time()
newcols_TMAX <- foreach (i=-21:35) %dopar% vNoaaExtract(firedata$closestStnID_TMAX, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "TMAX")
newcols_TMIN <- foreach (i=-21:35) %dopar% vNoaaExtract(firedata$closestStnID_TMIN, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "TMIN")
newcols_PRCP <- foreach (i=-21:35) %dopar% vNoaaExtract(firedata$closestStnID_PRCP, as.Date(firedata$IG_DATE, format = '%Y-%m-%e')+days(i), "PRCP")
proc.time()-ptm

#sequential time: 504.45
#multicore time: 495.30

for(i in -21:35){
  print(i)
  #index of newcols is 22 greater than i since i starts at 21 and index starts at 1
  newcol_TMAX <- newcols_TMAX[[i+22]]
  if(i<0){
    varname_TMAX <- paste0("TMAX.", -i)
  }else{
    varname_TMAX <- paste0("TMAX", i)
  }
  firedata[[varname_TMAX]]<- newcol_TMAX
  
  newcol_TMIN <- newcols_TMIN[[i+22]]
  if(i<0){
    varname_TMIN <- paste0("TMIN.", -i)
  }else{
    varname_TMIN <- paste0("TMIN", i)
  }
  firedata[[varname_TMIN]]<- newcol_TMIN
  
  newcol_PRCP <- newcols_PRCP[[i+22]]
  if(i<0){
    varname_PRCP <- paste0("PRCP.", -i)
  }else{
    varname_PRCP <- paste0("PRCP", i)
  }
  firedata[[varname_PRCP]]<- newcol_PRCP
}
