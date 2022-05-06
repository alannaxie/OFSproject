## Temperature correlation plots (air temp vs pond temp)
## By Alanna F. Xie (May 5 2022)

## Loading packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)

pond_data<-read.csv("biweekly_mar11.csv")           # Reading pond data
str(pond_data)                                      # Checking data structure
pond_data$pondid<- as.factor(pond_data$Pond.ID)     # Converting "Pond.ID" into factor
str(pond_data)                                      # Checking new data structure

weather_data<-read.csv("weather.csv")               # Reading weather data
str(weather_data)                                   # Checking data structure

## Calculating mean temperature for each pond
pond_data$mean_temp<- rowMeans(pond_data[,3:5])

## Isolate pond data for each pond
NW_data<-filter(pond_data, Pond.ID=="NW")
NE_data<-filter(pond_data, Pond.ID=="NE")
SE_data<-filter(pond_data, Pond.ID=="SE")
SW_data<-filter(pond_data, Pond.ID=="SW")

## Bind weather temperature data with mean pond temperature for all four ponds
NW_all<-data.frame(weather_data$Air.temperature,NW_data$mean_temp)
NE_all<-data.frame(weather_data$Air.temperature,NE_data$mean_temp)
SE_all<-data.frame(weather_data$Air.temperature,SE_data$mean_temp)
SW_all<-data.frame(weather_data$Air.temperature,SW_data$mean_temp)


## Figure A (Appendix 10) - NE pond
ggscatter(NE_all, x = "weather_data.Air.temperature", y = "NE_data.mean_temp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Air Temperature (°C)", ylab = "Pond Temperature (°C)")

## Figure B (Appendix 10) - NW pond
ggscatter(NW_all, x = "weather_data.Air.temperature", y = "NW_data.mean_temp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Air Temperature (°C)", ylab = "Pond Temperature (°C)")

## Figure C (Appendix 10) - SE pond
ggscatter(SE_all, x = "weather_data.Air.temperature", y = "SE_data.mean_temp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Air Temperature (°C)", ylab = "Pond Temperature (°C)")

## Figure D (Appendix 10) - SW pond
ggscatter(SW_all, x = "weather_data.Air.temperature", y = "SW_data.mean_temp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Air Temperature (°C)", ylab = "Pond Temperature (°C)")


