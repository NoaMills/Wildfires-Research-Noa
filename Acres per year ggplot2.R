firedata <- read.csv('./Data/MTBS_data.csv')
Year <- 1984:2017

total_acres <- vector()
for(year in Year)
  total_acres <- c(total_acres, sum(firedata[which(firedata$YEAR == year),]$ACRES/1000000))

acres_wf <- vector()
for(year in Year)
  acres_wf <- c(acres_wf, sum(firedata[which(firedata$YEAR==year, firedata$FIRE_TYPE == 'Wildfire'),]$ACRES/1000000))

acres_wfu <-vector()
for(year in Year)
  acres_wfu <- c(acres_wfu, sum(firedata[which(firedata$YEAR==year, firedata$FIRE_TYPE == 'Wildland Fire Use'),]$ACRES/1000000))

acres_rx <-vector()
for(year in Year)
  acres_rx <- c(acres_rx, sum(firedata[which(firedata$YEAR==year, firedata$FIRE_TYPE == 'Prescribed Fire'),]$ACRES/1000000))

acres_unk <-vector()
for(year in Year)
  acres_unk <- c(acres_unk, sum(firedata[which(firedata$YEAR==year, firedata$FIRE_TYPE == 'Unknown'),]$ACRES/1000000))

fires_by_year <- data.frame(Year, total_acres, acres_wf, acres_wfu, acres_rx, acres_unk)

qplot(Year, total_acres, data=fires_by_year, ylab = "Total Acres Burned per Year in Millions", geom=c("point", "smooth"))