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

firedata <- read.csv("Data/firedata3.csv")

#Linear Regression

fit <-lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + IG_YEAR, data=firedataDistWf)
summary(fit)
#Multiple R-squared, prev: 0.01841, Adjusted R-squared: 0.01799
#Multiple R-squared, curr: 0.03101, Adjusted R-squared: 0.03056

fit <- lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + IG_YEAR + isInitial + isSW + isSE + isS + isW + isNRP + isOV + isNW + isNE + isUM + isAL + isPR + isJan + isFeb +isMar + isApr + isMay + isJun + isJul + isAug + isSep + isOct +isNov, data=firedataDistWf)
summary(fit)
#Multiple R-squared, prev: 0.06797, Adjusted R-squared = 0.06528
#Multiple R-squared, curr: 0.07116, Adjusted R-squared = 0.06825
#Most significant variables, prev:
#TMAXavg + PRCPavg + IG_YEAR + isInitial + isOV + isNW + isAL + isApr + isSep + isOct + isAug
#Most significant variables, curr:
#TMAXavg + PRCPavg + IG_YEAR + isInitial + isAL + isOV + isUM + isFeb + isApr +isJun + isJul + isAug + isOct
fit <- lm(log(BurnBndAc) ~ TMAXavg + PRCPavg + IG_YEAR + isInitial + isAL + isOV + isUM + isFeb + isApr +isJun + isJul + isAug + isOct, data=firedataDistWf)
summary(fit)
#Multiple R-squared, prev: 0.06456, Adjusted R-squared: 0.06346
#Multiple R-squared, curr: 0.06373, Adjusted R-squared: 0.06232