firedata <- read.csv('./Data/MTBS_data.csv')
qplot(YEAR, data=firedata[which(firedata$FIRE_TYPE=='Unknown' | 
      FIRE_TYPE == 'Prescribed Fire' | 
      FIRE_TYPE == 'Wildfire' | 
      FIRE_TYPE == 'Wildland Fire Use'),], 
      fill=FIRE_TYPE)