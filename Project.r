library(data.table)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(stats)

setwd("C:\\Users\\uoakk\\OneDrive - IRI\\Desktop\\noaa-weather-data-jfk-airport.tar\\noaa-weather-data-jfk-airport")
data<-fread("jfk_weather.csv")
glimpse(data)
class(data)

newdata<- data[,c('DATE', 'HOURLYVISIBILITY', 'HOURLYDRYBULBTEMPF',  'HOURLYWETBULBTEMPF', 'HOURLYDewPointTempF',
                  'HOURLYRelativeHumidity', 'HOURLYWindSpeed', 'HOURLYWindDirection', 'HOURLYStationPressure',
                  'HOURLYPressureTendency', 'HOURLYSeaLevelPressure', 'HOURLYPrecip', 'HOURLYAltimeterSetting')]
glimpse(newdata)
fwrite(newdata, "newdata.csv")



newdata[newdata =="*"]=NA
glimpse(newdata)
fwrite(newdata, "newdata.csv")

newdata$HOURLYPrecip=replace(newdata$HOURLYPrecip, newdata$HOURLYPrecip== "T", "0.00")
glimpse(newdata)
fwrite(newdata, "newdata.csv")

newdata$HOURLYPrecip=replace(newdata$HOURLYPrecip, str_count(newdata$HOURLYPrecip,".") >1, "0.00")
fwrite(newdata, "newdata.csv")


newdata$HOURLYVISIBILITY=replace(newdata$HOURLYVISIBILITY, newdata$HOURLYVISIBILITY=="", NA)

newdata$HOURLYVISIBILITY=replace(newdata$HOURLYVISIBILITY, 
                                 (as.numeric(newdata$HOURLYVISIBILITY)>10 | as.numeric(newdata$HOURLYVISIBILITY)<0), NA)
glimpse(newdata)
fwrite(newdata, "newdata.csv")

columnsToNumeric <- c('HOURLYVISIBILITY', 'HOURLYDRYBULBTEMPF',  'HOURLYWETBULBTEMPF', 'HOURLYDewPointTempF',
                      'HOURLYRelativeHumidity', 'HOURLYWindSpeed', 'HOURLYWindDirection', 'HOURLYStationPressure',
                      'HOURLYSeaLevelPressure', 'HOURLYPrecip', 'HOURLYAltimeterSetting')
newdata[,(columnsToNumeric):= lapply(.SD, as.numeric), .SDcols = columnsToNumeric]
warnings()
glimpse(newdata)


newdata <- newdata %>% mutate_at(vars(HOURLYVISIBILITY, HOURLYDRYBULBTEMPF, HOURLYWETBULBTEMPF, HOURLYDewPointTempF,
                                      HOURLYRelativeHumidity, HOURLYWindSpeed, HOURLYWindDirection, HOURLYStationPressure,
                                      HOURLYSeaLevelPressure, HOURLYPrecip, HOURLYAltimeterSetting, HOURLYPressureTendency), ~replace_na(.,mean(., na.rm = TRUE)))

setnames(newdata, "HOURLYVISIBILITY", "HOURLYVisibility")
setnames(newdata, "HOURLYDRYBULBTEMPF", "HOURLYDryBulbTempF")
setnames(newdata, "HOURLYWETBULBTEMPF", "HOURLYWetBulbTempF")


newdata <- newdata[,-1]

lm1<-lm(HOURLYVisibility ~ 1  , data= newdata)
summary(lm1)

AIC(lm1)
step(lm1, scope=list(upper=lm(HOURLYVisibility ~ ., data=newdata)), direction="forward")

