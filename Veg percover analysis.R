## Vegetation percent cover analysis
## By Alanna F. Xie (May 5 2022)

## Loading packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)

veg_data<-read.csv("veg_cover.csv")                 # Reading data
str(veg_data)                                       # Checking data structure
veg_data$Pond.ID<- as.factor(veg_data$Pond.ID)      # Converting Pond.ID to factor
str(veg_data)                                       # Checking new data structure
colnames(veg_data) <- c("Pond.ID",                  # Renaming column headers
                        "Section", 
                        "Distance",
                        "Big leaf maple", 
                        "Bitter cherry", 
                        "Black hawthorn", 
                        "Black twinberry", 
                        "Deer fern", 
                        "Douglas fir", 
                        "Dull oregon grape", 
                        "English holly", 
                        "English ivy", 
                        "English oak", 
                        "Flowering red currant", 
                        "Himalayan blackberry", 
                        "Indian plum", 
                        "Laurel", 
                        "Paper birch", 
                        "Red alder", 
                        "Salmonberry", 
                        "Snowberry", 
                        "Sword fern", 
                        "Thimbleberry", 
                        "Trailing blackberry", 
                        "Western red cedar")

## Isolating vegetative data for each pond
NE_veg_data<-filter(veg_data, Pond.ID=="NE")
NW_veg_data<-filter(veg_data, Pond.ID=="NW")
SE_veg_data<-filter(veg_data, Pond.ID=="SE")
SW_veg_data<-filter(veg_data, Pond.ID=="SW")


##########################################################################################
##  Average percent cover of species for each pond and site-wide
##########################################################################################

## Calculating mean vegetative cover for each species 
site_percover<-colMeans(veg_data[ , c(4:25)])        # Averaged across site
NE_percover<-colMeans(NE_veg_data[ , c(4:25)])       # Averaged for NE pond
NW_percover<-colMeans(NW_veg_data[ , c(4:25)])       # Averaged for NW pond
SE_percover<-colMeans(SE_veg_data[ , c(4:25)])       # Averaged for SE pond
SW_percover<-colMeans(SW_veg_data[ , c(4:25)])       # Averaged for SW pond

## Making dataframe containing data from all four ponds and entire site 
NE_NW_percover<-rbind(NE_percover,NW_percover)                       # Bind NE and NW data
NE_NW_SE_percover<-rbind(NE_NW_percover,SE_percover)                 # Bind NE, NW and SE data
NE_NW_SE_SW_percover<-rbind(NE_NW_SE_percover,SW_percover)           # Bind NE, NW, SE and SW data
all_percover<-data.frame(rbind(NE_NW_SE_SW_percover,site_percover))  # Bind all data and convert into dataframe
colnames(all_percover) <- c("Big leaf maple",                        # Renaming column headers
                            "Bitter cherry", 
                            "Black hawthorn", 
                            "Black twinberry", 
                            "Deer fern", 
                            "Douglas fir", 
                            "Dull oregon grape", 
                            "English holly", 
                            "English ivy", 
                            "English oak", 
                            "Flowering red currant", 
                            "Himalayan blackberry", 
                            "Indian plum", 
                            "Laurel", 
                            "Paper birch", 
                            "Red alder", 
                            "Salmonberry", 
                            "Snowberry", 
                            "Sword fern", 
                            "Thimbleberry", 
                            "Trailing blackberry", 
                            "Western red cedar")   
all_percover$pond<- factor(c("NE", "NW", "SE", "SW", "Site"))       # Creating new column with pond names as factors

## Reshaping data
all_percover_long<- gather(all_percover, species, mean, "Big leaf maple":"Western red cedar", factor_key=TRUE)

## Figure 20
ggplot(data=all_percover_long, aes(x=species, y=mean)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1), 
        axis.text.y = element_text(color="black")) +
  facet_wrap(~factor(pond, levels=c("NE","NW","SE","SW","Site")), ncol= 1, strip.position = "top") + 
  labs(x="Species", y="Average Percent Cover (%)", colour="Pond ID")

##########################################################################################
##  Frequency of quadrats with individuals present for each pond and site-wide
##########################################################################################

## For each species, count the total number of quadrats where it is observed.
NE_freq<-gather(NE_veg_data, species, percentcov, 4:25) %>% filter(percentcov!=0) %>% count(species)
NW_freq<-gather(NW_veg_data, species, percentcov, 4:25) %>% filter(percentcov!=0) %>% count(species)
SE_freq<-gather(SE_veg_data, species, percentcov, 4:25) %>% filter(percentcov!=0) %>% count(species)
SW_freq<-gather(SW_veg_data, species, percentcov, 4:25) %>% filter(percentcov!=0) %>% count(species)
site_freq<-gather(veg_data, species, percentcov, 4:25) %>% filter(percentcov!=0) %>% count(species)

## For each species, calculate the frequencies/percentage of quadrats where it is observed.
NE_freq$percent_frequency<-NE_freq$n*100/20         # 20 quadrats sampled in NE pond
NW_freq$percent_frequency<-NW_freq$n*100/20         # 20 quadrats sampled in NE pond                     
SE_freq$percent_frequency<-SE_freq$n*100/15         # 15 quadrats sampled in SE pond
SW_freq$percent_frequency<-SW_freq$n*100/20         # 20 quadrats sampled in SW pond
site_freq$percent_frequency<-site_freq$n*100/75     # 75 quadrats sampled altogether

## Assign pond IDs to all calculated frequencies
NE_freq$Pond<-c("NE")
NW_freq$Pond<-c("NW")
SE_freq$Pond<-c("SE")
SW_freq$Pond<-c("SW")
site_freq$Pond<-c("Site")

## Create dataframe showing the calculated frequencies for the four ponds plus the entire site together
NE_NW_freq<-rbind(NE_freq,NW_freq)                                # Bind NE and NW data
NE_NW_SE_freq<-rbind(NE_NW_freq,SE_freq)                          # Bind NE, NW and SE data
NE_NW_SE_SW_freq<-rbind(NE_NW_SE_freq,SW_freq)                    # Bind NE, NW, SE and SW data
all_freq<-rbind(NE_NW_SE_SW_freq,site_freq)                       # Bind NE, NW, SE, SW and site-wide data
all_freq_dataframe <- data.frame(all_freq)                        # Convert binded data into a dataframe 
all_freq_dataframe$Pond<- as.factor(all_freq_dataframe$Pond)      # Making Pond into a factor
  
## Figure 21
ggplot(data=all_freq_dataframe, aes(x=species, y=percent_frequency)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1), axis.text.y = element_text(color="black")) +
  facet_wrap(~factor(Pond, levels=c("NE","NW","SE","SW","Site")), ncol= 1, strip.position = "top") + 
  labs(x="Species", y="Frequency of Quadrats with Individuals Present (%)", colour="Pond")


