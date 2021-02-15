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
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggplot2)

#Data visualization

firedata <- read.csv("Data/firedata3.csv")
firedata <- firedata %>% filter(BurnBndAc >= 1000)
firedata$isFireSeason <- as.factor(firedata$isFireSeason)
firedataWf <- firedata[which(firedata$Incid_Type == "Wildfire" & firedata$BurnBndAc >=1000),]
firedataRx <- firedata[which(firedata$Incid_Type == "Prescribed Fire" & firedata$BurnBndAc >=1000),]
firedataDistWf <- firedata[which(firedata$BurnBndAc >= 1000 & firedata$closestStnDist_TMAX < 25 & firedata$closestStnDist_TMIN < 25 & firedata$closestStnDist_TMAX < 25 & firedata$Incid_Type == "Wildfire"),]
firedataDistRx <- firedata[which(firedata$BurnBndAc >= 1000 & firedata$closestStnDist_TMAX < 25 & firedata$closestStnDist_TMIN < 25 & firedata$closestStnDist_TMAX < 25 & firedata$Incid_Type == "Prescribed Fire"),]


#########
#Section 7: Exploratory data analysis and data visualization
#########

if(!dir.exists("Output")){
  dir.create("Output")
}

hist(firedataWf$BurnBndAc/1000, xlab="Thousand Acres Burned", main="Histogram of Burned Acreage")
#As shown in the histogram, the number of acres burned is highly skewed.
hist(log(firedataWf$BurnBndAc), xlab="Log Acres Burned", main="Histogram of Log Burned Acreage")
#Even the log burned acrage is skewed

#explore TMIN, TMAX, PRCP and year vs burned area
ggplot(firedataWf, aes(x=as.Date(IG_DATE), y=log(BurnBndAc), color=Asmnt_Type))+
  geom_point(alpha=0.25)+
  scale_x_date()
#^Not a very informative plot

ggplot(firedata %>% filter(Incid_Type == "Wildfire"), aes(x=log(BurnBndAc), fill = year_group))+
  geom_histogram()+
  #facet_wrap(vars(Incid_Type))+
  xlab("Log Burned Acreage")+
  ggtitle("Burned Acreage of Wildfires by Year")+
  labs(fill="Year")

ggplot(firedata %>% filter(Incid_Type == "Prescribed Fire"), aes(x=log(BurnBndAc), fill = year_group))+
  geom_histogram()+
  #facet_wrap(vars(Incid_Type))+
  xlab("Log Burned Acreage")+
  ggtitle("Burned Acreage of Prescribed Fires by Year")+
  labs(fill="Year")

regionList <- setdiff(unique(firedataDistWf$region), c("Hawaii", "Puerto Rico"))
for(reg in regionList){
  firedataRegion <- firedataDistWf[which(firedataDistWf$region == reg),]
  plt <- ggplot(firedataRegion, aes(x=as.Date(IG_DATE), y=log(BurnBndAc)))+
    geom_point(alpha=1/5)+
    geom_smooth()+
    #facet_wrap(~region)+
    scale_x_date()+
    xlab("Date")+
    ylab("Log Acres Burned")+
    ggtitle(paste0("Area Burned by Region\n", reg))
  ggsave(paste0("Output/", reg, ".png"), plt)
}

ggplot(firedataDistWf, aes(x=TMAXavg, y=log(BurnBndAc)))+
  geom_point(alpha=1/6)+
  geom_smooth(method="glm")+
  xlab("Temperature Maximum Average in tenths of a degree C")+
  ylab("Log Burned Acreage")+
  ggtitle("Temperature Maximum vs Area Burned")
#facet_wrap(~subset)

ggplot(firedataDistWf, aes(x=sqrt(PRCPavg), y=log(BurnBndAc)))+
  geom_point(alpha=1/4)+
  geom_smooth(method="glm")+
  xlab("Square Root Average Daily Precipitation (cm)")+
  ylab("Log Burned Acreage")+
  ggtitle("Precipitation vs Area Burned")

ggplot(firedataDistWf, aes(x=TMINavg, y=log(BurnBndAc)))+
  geom_point(alpha=1/6)+
  geom_smooth(method="glm")+
  xlab("Temperature Minimum Average in tenths of a degree C")+
  ylab("Log Burned Acreage")+
  ggtitle("Temperature Minimum vs Area Burned")

ggplot(firedataDistWf, aes(x=TMAXavg, y=TMINavg))+
  geom_point(alpha=1/6)+
  geom_smooth(method="glm")+
  xlab("Temperature Maximum")+
  ylab("Temperature Minimum")+
  ggtitle(paste0("Temperature Maximum vs Minimum", reg))

#split by region
if(!dir.exists("Output/TMAX v TMIN")){
  dir.create("Output/TMAX v TMIN")
}

for(reg in regionList){
  print(reg)
  firedataRegion <- firedataDistWf[which(firedataDistWf$region == reg),]
  plt<-ggplot(firedataRegion, aes(x=TMAXavg, y=TMINavg))+
    geom_point(alpha=1/6)+
    geom_smooth(method="glm")+
    xlab("Temperature Maximum")+
    ylab("Temperature Minimum")+
    ggtitle(paste0("Temperature Maximum vs Minimum ", reg))
  ggsave(paste0("Output/TMIN v TMAX/TMINvTMAX", reg, ".png"), plt)
}

for(reg in regionList){
  firedataRegion <- firedataDistWf[which(firedataDistWf$region == reg),]
  print(reg)
  print(cor(firedataRegion$TMAXavg, firedataRegion$TMINavg))
}
#only a somewhat clear positive correlation in the SE and SW

#Above plot may be influenced by observations where, due to flagged or NA values, TMAX and TMIN were recorded at separate stations
#Currently, only one such observation
nrow(firedata[which(firedata$closestStnID_TMAX != firedata$closestStnID_TMIN),])

#Compare tmin vs tmax day of ignition
ggplot(firedataDistWf[which(!firedataDistWf$region %in% c("Puerto Rico", "Hawaii")),], aes(x=TMAX0, y=TMIN0, color=isFireSeason))+
  geom_point(alpha=1/6)+
  xlab("Temperature Maximum")+
  ylab("Temperature Minimum")+
  ggtitle("Temperature Maximum vs Minimum")+
  facet_wrap(~region)
  #facet_wrap(~subset)
#Above plots show weird shapes comparing tmax and tmin. Explore this more later.

#create maps of fires per year
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
hawaii <- states_and_territories[states_and_territories$NAME == "Hawaii",]
alaska <- states_and_territories[states_and_territories$NAME == "Alaska",]
states_and_territories %>%
  filter(!NAME %in% c("Alaska","American Samoa","Guam", "Hawaii", "Northern Marianas", "Puerto Rico", "Virgin Islands"))

hawaii_firedata <- firedata %>% filter(state == "Hawaii")
alaska_firedata <- firedata %>% filter(state == "Alaska")
puerto_rico_firedata <- firedata %>% filter(state == "Puerto Rico")
continuousUS_firedata <- firedata %>% filter(!state %in% c("Hawaii", "Alaska", "Puerto Rico"))

#Separate US shapefiles into 4 separate files: Continuous US, Hawaii, Alaska, and Puerto Rico
continuousUS <- states_and_territories %>% filter(!NAME %in% c("Hawaii", "Alaska", "Puerto Rico"))
hawaii <- states_and_territories %>% filter(NAME == "Hawaii")
alaska <- states_and_territories %>% filter(NAME == "Alaska")
puerto_rico <- states_and_territories %>% filter(NAME == "Puerto Rico")

options(scipen=15)

#point size corresponds to acres burned, color corresponds to fire type
for(year in 1984:2018){
  print(year)
  
  #Continuous map
  firedatayear_c <- continuousUS_firedata %>% filter(IG_YEAR == year & BurnBndAc >= 1000)
  #numfires <- nrow(firedatayear_c)
  p_cont <- ggplot(data=continuousUS)+
    geom_sf()+
    geom_point(data=firedatayear_c, mapping = aes(x=BurnBndLon, y=BurnBndLat, colour = Incid_Type, size = BurnBndAc), alpha=0.3)+
    coord_sf(xlim=c(-125,-65), ylim=c(23,50))+
    scale_size_continuous(range=c(2,9))+
    theme_void()+
    scale_color_manual(values=c("Wildfire"="#F90404", "Prescribed Fire" = "#2FB0CA", "Wildland Fire Use" = "#0D0529"))+
    theme(axis.title.x = element_blank(),axis.title.y = element_blank())+
    labs(colour="Fire Type", size="Burned Area in acres")+
    guides(colour = guide_legend(override.aes = list(size=10))) +
    ggtitle(paste0("US Wildfires, ", year))+
    theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15), 
      plot.title = element_text(size=25, hjust=0.5)
    )
    
  
  #print(p_cont)
  ggsave(paste0("Output/map", year, ".png"), p_cont, width=15)
  
  #Continuous US map, zoomed in on south east
  #Continuous map
  firedatayear_c <- continuousUS_firedata %>% filter(IG_YEAR == year & BurnBndAc >= 1000)
  #numfires <- nrow(firedatayear_c)
  p_cont <- ggplot(data=continuousUS)+
    geom_sf()+
    geom_point(data=firedatayear_c, mapping = aes(x=BurnBndLon, y=BurnBndLat, colour = Incid_Type, size = BurnBndAc), alpha=0.3)+
    coord_sf(xlim=c(-98,-75), ylim=c(24,40))+
    scale_size_continuous(range=c(2,9))+
    scale_color_manual(values=c("Wildfire"="#F90404", "Prescribed Fire" = "#2FB0CA", "Wildland Fire Use" = "#0D0529"))+
    labs(x="", y="", colour="Fire Type", size="Burned Area in acres")+
    guides(colour = guide_legend(override.aes = list(size=10))) +
    ggtitle(paste0("Southeastern US Wildfires, ", year))+
    theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15), 
      plot.title = element_text(size=25, hjust=0.5)
    )
  #print(p_cont)
  ggsave(paste0("Output/map_se", year, ".png"), p_cont, width=15)
  
  #Hawaii
  if(year %in% (firedata %>% filter(state=="Hawaii"))$IG_YEAR){
    firedatayear_h <- hawaii_firedata %>% filter(IG_YEAR == year & BurnBndAc >= 1000)
    p_hawaii <- ggplot(data=hawaii)+
      geom_sf()+
      geom_point(data=firedatayear_h, mapping = aes(x=BurnBndLon, y=BurnBndLat, colour = Incid_Type, size = BurnBndAc), alpha=1/2)+
      coord_sf(xlim=c(-160.5,-154.5), ylim=c(18.7,22.5))+
      scale_size_continuous(range=c(3,8))+
      scale_color_manual(values=c("Wildfire"="#F90404", "Prescribed Fire" = "#2FB0CA", "Wildland Fire Use" = "#0D0529"))+
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())+
      labs(x="", y="", colour="Fire Type", size="Burned Area in acres")+
      guides(colour = guide_legend(override.aes = list(size=10))) +
      ggtitle(paste0("Hawaii Wildfires, ", year))+
      theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15), 
        plot.title = element_text(size=25, hjust=0.5)
      )
    #print(p_hawaii)
    ggsave(paste0("Output/map_h", year, ".png"), p_hawaii, width = 15)
  }
  
  #Alaska
  if(year %in% (firedata %>% filter(state=="Alaska"))$IG_YEAR){
    firedatayear_a <- alaska_firedata %>% filter(IG_YEAR == year & BurnBndAc >= 1000)
    p_alaska <- ggplot(data=alaska)+
      geom_sf()+
      geom_point(data=firedatayear_a, mapping = aes(x=BurnBndLon, y=BurnBndLat, colour = Incid_Type, size = BurnBndAc), alpha=0.55)+
      coord_sf(xlim=c(-180, -127),ylim=c(51,71))+
      scale_size_continuous(range=c(4,9))+
      scale_color_manual(values=c("Wildfire"="#F90404", "Prescribed Fire" = "#2FB0CA", "Wildland Fire Use" = "#0D0529"))+
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())+
      labs(x="", y="", colour="Fire Type", size="Burned Area in acres")+
      guides(colour = guide_legend(override.aes = list(size=10))) +
      ggtitle(paste0("Alaska Wildfires, ", year))+
      theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15), 
        plot.title = element_text(size=25, hjust=0.5)
      )
    #print(p_alaska)
    ggsave(paste0("Output/map_alaska", year, ".png"), p_alaska, width = 15)
  }
  
  #Puerto Rico
  if(year %in% (firedata %>% filter(state=="Puerto Rico"))$IG_YEAR){
    firedatayear_pr <- puerto_rico_firedata %>% filter(IG_YEAR == year & BurnBndAc >= 1000)
    p_pr <- ggplot(data=puerto_rico)+
      geom_sf()+
      geom_point(data=firedatayear_pr, mapping = aes(x=BurnBndLon, y=BurnBndLat, colour = Incid_Type, size = BurnBndAc), alpha=0.6)+
      coord_sf()+
      scale_color_manual(values=c("Wildfire"="#F90404", "Prescribed Fire" = "#2FB0CA", "Wildland Fire Use" = "#40D0529"))+
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())+
      labs(x="", y="", colour="Fire Type", size="Burned Area in acres")+
      guides(colour = guide_legend(override.aes = list(size=10))) +
      ggtitle(paste0("Puerto Rico Wildfires, ", year))+
      theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15), 
        plot.title = element_text(size=25, hjust=0.5)
      )
    #print(p_pr)
    ggsave(paste0("Output/map_pr", year, ".png"), p_pr, width = 15)
  }
}

#Same, but zoomed in on SE

#Histograms of fires by month, per type
ggplot(firedata, aes(IG_MONTH))+
  geom_histogram(binwidth=1)+
  facet_wrap(~Incid_Type, ncol=1)

#boxplot of acres by type, outliers removed
ggplot(firedata, aes(x=Incid_Type, y=log(BurnBndAc)))+
  geom_boxplot()+
  xlab("Fire type")+
  ylab("ln(Acres Burned)")

ggplot(firedata, aes(x=Incid_Type, y=log(BurnBndAc)))+
  geom_violin()+
  xlab("Fire type")+
  ylab("ln(Acres Burned)")

#Scatter plot of acres vs date, color = TMAXavg
ggplot(firedataDistWf, aes(x=as.Date(IG_DATE), y=log(BurnBndAc), color=TMAXavg))+
  geom_point(alpha=1/2)+
  scale_x_date()+
  scale_color_gradientn(colors=c("cyan", "deepskyblue", "deepskyblue4", "blueviolet", "indianred3","orangered3", "red"), values=c(0,0.3,0.4,0.5,0.6,0.7,1))
#Not a very informative plot

#above broken down by fire season vs not fire season
firedataDistWf_summer <- firedataDistWf_1000[which(firedataDistWf_1000$IG_MONTH %in% c(4,5,6,7,8,9)),]
ggplot(firedataDistWf_summer, aes(x=as.Date(IG_DATE), y=log(BurnBndAc), color=TMAXavg))+
  geom_point(alpha=1/2)+
  scale_x_date()+
  scale_color_gradientn(colors=c("cyan", "deepskyblue", "deepskyblue4", "blueviolet", "indianred3","orangered3", "red"), values=c(0,0.3,0.4,0.5,0.6,0.7,1))

firedataDistWf_winter <- firedataDistWf_1000[which(!firedataDistWf_1000$IG_MONTH %in% c(4,5,6,7,8,9)),]
ggplot(firedataDistWf_winter, aes(x=as.Date(IG_DATE), y=log(BurnBndAc), color=TMAXavg))+
  geom_point(alpha=1/2)+
  scale_x_date()+
  scale_color_gradientn(colors=c("cyan", "deepskyblue", "deepskyblue4", "blueviolet", "indianred3","orangered3", "red"), values=c(0,0.3,0.4,0.5,0.6,0.7,1))
#still not very informative

ggplot(firedataDistWf, aes(TMAXavg))+
  geom_histogram()