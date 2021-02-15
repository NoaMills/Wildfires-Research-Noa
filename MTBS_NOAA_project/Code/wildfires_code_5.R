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
#Multiple R-squared: 0.01841, Adjusted R-squared: 0.01799

fit <- lm(log(BurnBndAc) ~ TMAXavg + TMINavg + sqrt(PRCPavg) + IG_YEAR + isInitial + isSW + isSE + isS + isW + isNRP + isOV + isNW + isNE + isUM + isAL + isPR + isJan + isFeb +isMar + isApr + isMay + isJun + isJul + isAug + isSep + isOct +isNov, data=firedataDistWf)
summary(fit)
#Multiple R-squared = 0.06797, Adjusted R-squared = 0.06528
#Most significant variables:
#TMAXavg + PRCPavg + IG_YEAR + isInitial + isOV + isNW + isAL + isApr + isSep + isOct + isAug

fit <- lm(log(BurnBndAc) ~ TMAXavg + sqrt(PRCPavg) + IG_YEAR + isInitial + isOV + isNW + isAL + isApr + isSep + isOct + isAug, data=firedataDistWf)
summary(fit)
#Multiple R-squared: 0.06456, Adjusted R-squared: 0.06346