firedata <- read.csv('./Data/MTBS_data.csv')

acres_per_year <- vector()
for(year in 1984:2017)
  acres_per_year <- c(acres_per_year, sum(firedata[which(firedata$YEAR == year),]$ACRES/1000000))
plot(1984:2017, acres_per_year, xlab="Year", ylab="Acres Burned in Millions", type="b")
