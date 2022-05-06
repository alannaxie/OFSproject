## Abiotic and Shrimp Correlation Plots (pH/depth/temp vs number of fairy shrimp)
## By Alanna F. Xie (May 5 2022)

## Loading packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)

raw_data<-read.csv("biweekly_mar11.csv")                                   # Reading data
str(raw_data)                                                              # Checking data structure
raw_data$Collection.Date <- as.Date(raw_data$Collection.Date,              # Converting "Collection.Date" into Date format
                                    format = "%Y-%m-%d" )   
raw_data$pondid<- as.factor(raw_data$Pond.ID)                              # Converting "Pond.ID" into factor
str(raw_data)                                                              # Checking new data structure

raw_data$mean_temp<- rowMeans(raw_data[,3:5])                              # Calculating mean temperature
raw_data$mean_depth<-rowMeans(raw_data[,6:9])                              # Calculating mean depth

## Creating data frame with all of NW data, and NE and SE data only after first detection of shrimp
## Step 1) Isolate all required data NW, NW and SE ponds
NW_data<-filter(raw_data, Pond.ID=="NW")                                   # All data from NW pond
NE_data<-filter(raw_data, Pond.ID=="NE")                                   # All data from NE pond
NE_data_adj<-filter(NE_data, Collection.Date >= as.Date("2022-02-19"))    # All data from NE pond from Feb 19th and onwards
SE_data<-filter(raw_data, Pond.ID=="SE")                                   # All data from SE pond
SE_data_adj<-filter(SE_data, Collection.Date >= as.Date("2022-02-10"))    # All data from SE pond from Feb 10th and onwards
#Step 2) Bind data from all ponds
NW_NE_binded<-rbind(NW_data,NE_data_adj)                                   # Binded NW and NE data                                
all_binded<-rbind(NW_NE_binded,SE_data_adj)                                # Binded NW, NE and SE data
all_binded

## Figure E (Appendix 11)
ggscatter(all_binded, x = "pH", y = "Shrimp.Observed", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Pond pH", ylab = "Number of Shrimp Observed")

## Figure F (Appendix 11)
ggscatter(all_binded, x = "mean_temp", y = "Shrimp.Observed",add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Average Water Temperature (°C)", ylab = "Number of Shrimp Observed")

## Figure G (Appendix 11)
ggscatter(all_binded, x = "mean_depth", y = "Shrimp.Observed", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Average Pond Depth (cm)", ylab = "Number of Shrimp Observed")

