## Tree Mensuration
## By Alanna F. Xie (May 5 2022)

## Loading packages
library(ggplot2)
library(dplyr)
library(tidyr)

trees<-read.csv("trees.csv")                        # Reading data
str(trees)                                          # Checking data structure
trees$Species.name<-as.factor(trees$Species.name)   # Converting "Species.name" into factor
trees$Pond.ID<-as.factor(trees$Pond.ID)             # Converting "Pond.ID" into factor
str(trees)                                          # Checking new data structure

## Isolate data for each pond
NE_trees<-filter(trees,Pond.ID=="NE")
NW_trees<-filter(trees,Pond.ID=="NW")
SE_trees<-filter(trees,Pond.ID=="SE")
SW_trees<-filter(trees,Pond.ID=="SW")

## Calculate number of trees, their average diameter and average proximity to the ponds for each pond (species-cumulative)
NE_trees %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))
NW_trees %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))
SE_trees %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))
SW_trees %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))

## Calculate number of trees, average diameter of trees and average proximity of trees to the ponds by species for each pond
NE_trees %>% group_by(Species.name) %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))
NW_trees %>% group_by(Species.name) %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))
SE_trees %>% group_by(Species.name) %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))
SW_trees %>% group_by(Species.name) %>% summarize(n(),mean(diameter.cm),mean(distance.to.the.pond.cm))


