## Oregon fairy shrimp population analysis
## By Alanna F. Xie (May 5 2022)

## Loading packages
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

############################################################################################
## Focal pond fairy shrimp population size 
############################################################################################

raw_data<-read.csv("biweekly_mar11.csv")                           # Reading data for focal ponds
str(raw_data)                                                      # Checking data structure
raw_data$Collection.Date2 <- as.Date(raw_data$Collection.Date,     # Converting "Collection.Date" into Date format 
                                     format = "%Y-%m-%d" )
raw_data$pondid<- as.factor(raw_data$Pond.ID)                      # Converting "Pond.ID" into factor
str(raw_data)                                                      # Checking new data structure

## Figure 9
ggplot(data=raw_data, aes(x=Collection.Date2, y=Shrimp.Observed, group=pondid,colour=pondid)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-19")),linetype=2, size=1.5) +
  labs(x="Date", y="Number of Fairy Shrimp Observed", colour="Pond ID") +
  theme_bw() +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "7 days"),
               minor_breaks = function(x) seq.Date(from = min(x), to = max(x),  by = "1 days"),
               expand = c(0,0)) +
  theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1),
        axis.text.y = element_text(color="black"),
        plot.margin = margin(0.3,0.3,0.3,0.4, "cm"))

############################################################################################
## Size and sex of captured fairy shrimp 
############################################################################################

size_sex<-read.csv("captured_sex_size.csv")                         # Reading data

## Mean size of captured individuals organized by sex
mean_size<- ddply(size_sex, "Sex", summarise, size=mean(Size))
mean_size

## Figure 10
ggplot(size_sex, aes(x=Size, fill=Sex,color=Sex)) + 
  geom_histogram(binwidth=0.1)+theme_bw() +
  labs(x="Oregon Fairy Shrimp Size (cm)", y="Number of Oregon Fairy Shrimp") +
  scale_x_continuous(breaks=seq(1.5,3.5,0.2)) +
  scale_y_continuous(breaks=seq(0,2,1))

############################################################################################
## Overflow fairy shrimp density 
############################################################################################

overflow_raw<-read.csv("overflow_densities.csv")         # Reading data for overflow area

## Calculating mean densities for overflow area
overflow_mean<-rowMeans(overflow_raw[ , c(2:4)])/10.8    # Calculate the average number of shrimp sampled per litre for each date 
                                                         # Note: volume of collection container is 10.8 liters
overflow_dataframe <- data.frame(overflow_mean)          # Put average density of shrimp into a dataframe
overflow_dataframe$date <- as.Date(c("2022-01-27",       # Add dates to dataframe in Date format 
                                     "2022-02-10", 
                                     "2022-02-24", 
                                     "2022-03-11"), 
                                   format = "%Y-%m-%d" )

## Figure 11
ggplot(data=overflow_dataframe, aes(x=date, y=overflow_mean)) + 
  geom_line() + 
  geom_point() +
  labs(x="Date", y="Fairy Shrimp Population Density (Individuals per liter)", colour="Pond ID") +
  theme_bw() +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days"),
               minor_breaks = function(x) seq.Date(from = min(x), to = max(x),  by = "1 days"),
               expand = c(0,0)) +
  theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1), 
        axis.text.y = element_text(color="black"),
        plot.margin = margin(0.3,0.4,0.3,0.3, "cm"),
        text = element_text(size=14))



