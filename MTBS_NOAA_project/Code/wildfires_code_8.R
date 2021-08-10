library(dplyr)
library(ggplot2)

firedata <- read.csv("Data/firedata7.csv")

firedataBackup <- firedata
#only include fires with weather stations within 25 km
firedata <- firedata %>% filter(closestStnDist_TMAX <= 30 & 
            closestStnDist_TMIN <= 30 & closestStnDist_PRCP <= 30)

#only include wildfires
firedataWf <- firedata[which(firedata$Incid_Type == "Wildfire"),]

#Versions 1 through 4 use station based data, versions 5 through 8 use PDSI data

#Version 1: All bins, adjusted R-squared: 0.1211
fit <-lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + IG_YEAR + isInitial +
           TSLB10_inf_20 + TSLB10_20_100 + TSLB10_100_300 + TSLB10_300_500 +
           TSLB10_500_1000 + TSLB10_1000_2000 + TSLB10_2000_inf +
           TSLB25_inf_20 + TSLB25_20_100 + TSLB25_100_300 + TSLB25_300_500 +
           TSLB25_500_1000 + TSLB25_1000_2000 + TSLB25_2000_inf, 
            data=firedataWf)
summary(fit)

#Version 2: TSLB10 continuous, TSLB25 bin, adjusted R-squared: 0.117
fit2 <- lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + IG_YEAR + isInitial +
             TSLB10 + TSLB25_inf_20 + TSLB25_20_100 + TSLB25_100_300 + 
             TSLB25_300_500 + TSLB25_500_1000 + TSLB25_1000_2000,
              data=firedataWf)
summary(fit2)

#Version 3: TSLB10 bin, TSLB25 continuous, adjusted R-squared: 0.1137
fit3 <- lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + IG_YEAR + isInitial +
             TSLB25 + TSLB10_inf_20 + TSLB10_20_100 + TSLB10_100_300 + 
             TSLB10_300_500 + TSLB10_500_1000 + TSLB10_1000_2000,
           data=firedataWf)
summary(fit3)

#Version 4: All continuous, adjusted R-squared: 0.1159
fit4 <- lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + IG_YEAR + isInitial +
             TSLB25 + TSLB10, data=firedataWf)
summary(fit4)

#Now with PDSI instead of TMAX/TMIN/PRCP

#Version 5: All bins, adjusted R-squared: 0.0685
fit5 <-lm(log(BurnBndAc) ~ PDSI + IG_YEAR + isInitial +
           TSLB10_inf_20 + TSLB10_20_100 + TSLB10_100_300 + TSLB10_300_500 +
           TSLB10_500_1000 + TSLB10_1000_2000 + TSLB10_2000_inf +
           TSLB25_inf_20 + TSLB25_20_100 + TSLB25_100_300 + TSLB25_300_500 +
           TSLB25_500_1000 + TSLB25_1000_2000 + TSLB25_2000_inf, 
         data=firedataWf)
summary(fit5)

#Version 6: TSLB10 continuous, TSLB25 bin, adjusted R-squared: 0.0534
fit6 <- lm(log(BurnBndAc) ~ PDSI + IG_YEAR + isInitial +
             TSLB10 + TSLB25_inf_20 + TSLB25_20_100 + TSLB25_100_300 + 
             TSLB25_300_500 + TSLB25_500_1000 + TSLB25_1000_2000,
           data=firedataWf)
summary(fit6)

#Version 7: TSLB10 bin, TSLB25 continuous, adjusted R-squared: 0.0593
fit7 <- lm(log(BurnBndAc) ~ PDSI + IG_YEAR + isInitial +
             TSLB25 + TSLB10_inf_20 + TSLB10_20_100 + TSLB10_100_300 + 
             TSLB10_300_500 + TSLB10_500_1000 + TSLB10_1000_2000,
           data=firedataWf)
summary(fit7)

#Version 8: All continuous, adjusted R-squared: 0.0504
fit8 <- lm(log(BurnBndAc) ~ PDSI + IG_YEAR + isInitial +
             TSLB25 + TSLB10, data=firedataWf)
summary(fit8)

#Here we see that using the individual weather variables provides more better
#predicting power than using the Palmer Drought Severity Index

cor(firedataWf$PDSI, firedataWf$TMAXavg, use="complete.obs") #-0.05097
cor(firedataWf$PDSI, firedataWf$TMINavg, use="complete.obs") #-0.06555
cor(firedataWf$PDSI, firedataWf$PRCPavg, use="complete.obs") #0.065423

#Since the weather vars and PDSI aren't correlated, try using both
#Adjusted R-squared: 0.1205
fit9 <-lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + PDSI + IG_YEAR + isInitial +
           TSLB10_inf_20 + TSLB10_20_100 + TSLB10_100_300 + TSLB10_300_500 +
           TSLB10_500_1000 + TSLB10_1000_2000 + TSLB10_2000_inf +
           TSLB25_inf_20 + TSLB25_20_100 + TSLB25_100_300 + TSLB25_300_500 +
           TSLB25_500_1000 + TSLB25_1000_2000 + TSLB25_2000_inf, 
         data=firedataWf)
summary(fit9)

ggplot(firedataWf, aes(x=PDSI, y=log(BurnBndAc))) + geom_point(alpha=0.2)
