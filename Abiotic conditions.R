## Change in pond pH, temperature, depth and DO over time at focal ponds
## By Alanna F. Xie (May 5 2022)

## Loading packages
library(dplyr)
library(ggplot2)
library(tidyr)

raw_data<-read.csv("biweekly_mar11.csv")                               # Reading raw data
str(raw_data)                                                          # Checking data structure
raw_data$Collection.Date2 <- as.Date(raw_data$Collection.Date,         # Converting "Collection.Date" into Date format 
                                     format = "%Y-%m-%d" )    
raw_data$pondid<- as.factor(raw_data$Pond.ID)                          # Converting "Pond.ID" into factor
str(raw_data)                                                          # Checking new data structure

###################################################################################################
## Analyzing change in depth over time
###################################################################################################

depth_noNA<-filter(raw_data, Depth.4!="n/a")         # Filter out dates with no data collected
                                                     # Note: Due to change in methodology, only three depth measurements 
                                                     # were made for each pond prior to December 16th. 
                                                     # Dates lacking a fourth depth measurement were excluded for this analysis.
depth_noNA$mean_depth<-rowMeans(depth_noNA[,6:9])    # Calculate mean depth for each pond 

## Figure 12
ggplot(data=depth_noNA, aes(x=Collection.Date2, y=mean_depth, group=pondid,colour=pondid))  + 
  geom_line() + 
  geom_point()+labs(x="Date", y="Average Pond Depth (cm)", colour="Pond ID") +
  theme_bw() +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "7 days"),
               minor_breaks = function(x) seq.Date(from = min(x), to = max(x),  by = "1 days"),
               expand = c(0,0)) +
  theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1),
        axis.text.y = element_text(color="black"))


###################################################################################################
## Analyzing change in temperature over time
###################################################################################################

raw_data$mean_temp<- rowMeans(raw_data[,3:5])    # Calculate mean temperature for each pond 

## Figure 13
ggplot(data=raw_data, aes(x=Collection.Date2, y=mean_temp, group=pondid, colour=pondid)) + 
  geom_line() + 
  geom_point() + 
  labs(x="Date", y="Average Water Temperature (°C)", colour="Pond ID") +
  theme_bw() +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "7 days"),
               minor_breaks = function(x) seq.Date(from = min(x), to = max(x),  by = "1 days"),
               expand = c(0,0)) +
  theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1),
        axis.text.y = element_text(color="black"),
        plot.margin = margin(0.3,0.3,0.3,0.4, "cm"))


###################################################################################################
## Analyzing change in pond pH over time
###################################################################################################

## Figure 14
ggplot(data=raw_data, aes(x=Collection.Date2, y=pH, group=pondid, colour=pondid)) + 
  geom_line() + 
  geom_point() + 
  labs(x="Date", y="Pond pH", colour="Pond ID") +
  theme_bw()+scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "7 days"),
                          minor_breaks = function(x) seq.Date(from = min(x), to = max(x),  by = "1 days"),
                          expand = c(0,0)) +
  theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1),
        axis.text.y = element_text(color="black"))


###################################################################################################
## Analyzing change in dissolved oxygen over time
###################################################################################################
DO_noNA<-filter(raw_data, DO!="NA")             # Filter out dates with no data collected

## Figure 15
ggplot(data=DO_noNA, aes(x=Collection.Date, y=DO, group=pondid, fill=pondid)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() + 
  labs(x="Date", y="Dissolved Oxygen (mg/L)", fill="Pond ID") +
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))

