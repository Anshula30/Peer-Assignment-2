# Storm Data Analysis

## Synopsis

The dataset for storm events in the United States is compiled from gathering data entered by many different people, so it suffers a little bit of inconsistent results. It is, however, possible to clean the dataset to some extent. In order for it to be manageable, and because we are only interested in the most harmful events, I have excluded from the analysis any event whose economical or health consequence falls below the median for the category (see below).

The analysis was aimed at answering two questions, namely: - Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health? - Across the United States, which types of events have the greatest economic consequences?

## Data Processing

The following code allows to read and preprocess the data to have a more manageable sized dataset. Note: the download of the file has been commented, and the actual code run starts from reading the file.

## Set working directory and system time to English and download the file
setwd("C:/Users/Steph/Documents/Coursera_Data_science/Course5_Reproducible_Research/Assignment2")
Sys.setlocale("LC_TIME", "English")
## [1] "English_United States.1252"
sessionInfo()
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=French_Switzerland.1252  LC_CTYPE=French_Switzerland.1252   
## [3] LC_MONETARY=French_Switzerland.1252 LC_NUMERIC=C                       
## [5] LC_TIME=English_United States.1252 
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] xtable_1.7-4  ggplot2_1.0.0 dplyr_0.3.0.2
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1   colorspace_1.2-4 DBI_0.3.1        digest_0.6.6    
##  [5] evaluate_0.5.5   formatR_1.0      grid_3.1.2       gtable_0.1.2    
##  [9] htmltools_0.2.6  knitr_1.8        magrittr_1.5     MASS_7.3-35     
## [13] munsell_0.4.2    parallel_3.1.2   plyr_1.8.1       proto_0.3-10    
## [17] Rcpp_0.11.3      reshape2_1.4.1   rmarkdown_0.3.11 scales_0.2.4    
## [21] stringr_0.6.2    tools_3.1.2      yaml_2.1.13
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","repdata-data-StormData.csv.bz2")

## Read data
storm_data<-read.csv(bzfile("repdata-data-StormData.csv.bz2"))

## To reduce dataset size, only keep columns of interest
library(dplyr)
storm_data<-select(storm_data,STATE,BGN_DATE,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
Data cleaning is necessary for the damage cost estimates and for the events. For the damage cost estimates, according to the explanations:“Estimates should be rounded to three significant digits, followed by an alphabetical character signifying the magnitude of the number, i.e., 1.55B for $1,550,000,000. Alphabetical characters used to signify magnitude include”K" for thousands, “M” for millions, and “B” for billions“.

So, the value PROPDMG and CROPDMG should be multiplied bz the coefficient given in PROPDMGEXP and CROPDMGEXP, respectively. There are however some unexpected values in these columns:

# Damage cost estimate
summary(storm_data$PROPDMGEXP)
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
levels(storm_data$PROPDMGEXP)<-c(rep(1,6),2,3,4,5,6,7,8,9,2,2,3,6,6)
summary(storm_data$PROPDMGEXP)
##      1      2      3      4      5      6      7      8      9 
## 466189     20 424669      4     28  11341      5      1     40
# Crop damage cost estimate
summary(storm_data$CROPDMGEXP)
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
levels(storm_data$CROPDMGEXP)<-c(1,1,1,2,9,3,3,6,6)
summary(storm_data$CROPDMGEXP)
##      1      2      9      3      6 
## 618439      1      9 281853   1995
storm_data<-mutate(storm_data,ECON_DMG=PROPDMG*10^(as.numeric(as.character(PROPDMGEXP)))+CROPDMG*10^(as.numeric(as.character(CROPDMGEXP))))
storm_data<-select(storm_data,-PROPDMG,-PROPDMGEXP,-CROPDMG,-CROPDMGEXP)
As the dataset is large, and we only want to keep the top events, we can keep only the entries that have a significant associated cost (more than the mean of all events):

index_rows2Keep<-union(union(which(storm_data$ECON_DMG>mean(storm_data$ECON_DMG)),which(storm_data$FATALITIES>mean(storm_data$FATALITIES))),(which(storm_data$INJURIES>mean(storm_data$INJURIES))))

storm_data<-storm_data[index_rows2Keep,]
Events should be within the list in Table 1 of the manual: real_events<-Astronomical Low Tide; Avalanche; Blizzard; Coastal Flood; Cold/Wind Chill; Debris Flow; Dense Fog; Dense Smoke; Drought; Dust Devil; Dust Storm; Excessive Heat; Extreme Cold/Wind Chill; Flash Flood; Flood; Frost/Freeze; Funnel Cloud; Freezing Fog; Hail; Heat; Heavy Rain; Heavy Snow; High Surf; High Wind; Hurricane (Typhoon); Ice Storm; Lake-Effect Snow; Lakeshore Flood; Lightning; Marine Hail; Marine High Wind; Marine Strong Wind; Marine Thunderstorm Wind; Rip Current; Seiche; Sleet; Storm Surge/Tide; Strong Wind; Thunderstorm Wind; Tornado; Tropical Depression; Tropical Storm; Tsunami; Volcanic Ash; Waterspout; Wildfire; Winter Storm; Winter Weather.

When we look at the events listed in the table, we realize some cleaning is necessary.

#distinct(select(storm_data,EVTYPE))

real_events<-toupper(c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Frost/Freeze","Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane (Typhoon)","Ice Storm","Lake-Effect Snow","Lakeshore Flood","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Surge/Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather"))

#unique(as.character(storm_data$EVTYPE))[which(!(unique(as.character(storm_data$EVTYPE)) %in% real_events))]

levels(storm_data$EVTYPE)[grep("TSTM WIND",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Thunderstorm Wind")
levels(storm_data$EVTYPE)[grep("SEVERE THUNDERSTORM WINDS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Thunderstorm Wind")
levels(storm_data$EVTYPE)[grep("SEVERE THUNDERSTORM WINDS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Thunderstorm Wind")
levels(storm_data$EVTYPE)[grep("THUNDERSTORM WINDS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Thunderstorm Wind")
levels(storm_data$EVTYPE)[grep("HURRICANE",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Hurricane (Typhoon)")
levels(storm_data$EVTYPE)[grep("FLASH FLOOD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("FLASH FLOOD")
levels(storm_data$EVTYPE)[grep("HEAVY RAIN",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("HEAVY RAIN")
levels(storm_data$EVTYPE)[grep("FLOODING",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("RIVER FLOOD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("HIGH WIND",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("HIGH WIND")
levels(storm_data$EVTYPE)[grep("WILD FIRES",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Wildfire")
levels(storm_data$EVTYPE)[grep("WINTER STORM",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("WINTER STORM")
levels(storm_data$EVTYPE)[grep("SEVERE THUNDERSTORMS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Thunderstorm Wind")
levels(storm_data$EVTYPE)[grep("FLOOD/RAIN/WINDS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("FLOOD/FLASH FLOOD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("HEAVY SNOW",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy snow")
levels(storm_data$EVTYPE)[grep("EXTREME COLD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Extreme Cold/Wind Chill")
levels(storm_data$EVTYPE)[grep("URBAN FLOOD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("FLOOD")
levels(storm_data$EVTYPE)[grep("STORM SURGE",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Storm surge/tide")
levels(storm_data$EVTYPE)[grep("TROPICAL STORM",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("TROPICAL STORM")
levels(storm_data$EVTYPE)[grep("^WIND$",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("High wind")
levels(storm_data$EVTYPE)[grep("FROST",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Frost/Freeze")
levels(storm_data$EVTYPE)[grep("WINTER WEATHER/MIX",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("WINTER WEATHER")
levels(storm_data$EVTYPE)[grep("HEAVY SURF/HIGH SURF",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("High surf")
levels(storm_data$EVTYPE)[grep("^SNOW$",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy snow")
levels(storm_data$EVTYPE)[grep("^Rain$",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy rain")
levels(storm_data$EVTYPE)[grep("COOL AND WET",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Cold/Wind Chill")
levels(storm_data$EVTYPE)[grep("FLOODS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("COLD AND WET CONDITIONS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Cold/Wind Chill")
levels(storm_data$EVTYPE)[grep("EXCESSIVE WETNESS",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy Rain")
levels(storm_data$EVTYPE)[grep("hEAT WAVE",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heat")
levels(storm_data$EVTYPE)[grep("MAJOR FLOOD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("FREEZE",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Frost/Freeze")
levels(storm_data$EVTYPE)[grep("HAIL",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Hail")
levels(storm_data$EVTYPE)[grep("WATERSPOUT",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Waterspout")
levels(storm_data$EVTYPE)[grep("LIGHTNING",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Lightning")
levels(storm_data$EVTYPE)[grep("RECORD COLD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Extreme Cold/Wind Chill")
levels(storm_data$EVTYPE)[grep("FOREST FIRES",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("WILDFIRE")
levels(storm_data$EVTYPE)[grep("RIVER AND STREAM FLOOD",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("WILDFIRES",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("WILDFIRE")
levels(storm_data$EVTYPE)[grep("SNOW/FREEZING RAIN",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("HEAvy Snow")
levels(storm_data$EVTYPE)[grep("FREEZING RAIN",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Ice storm")
levels(storm_data$EVTYPE)[grep("TORNADO",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Tornado")
levels(storm_data$EVTYPE)[grep("THUNDERSTORM",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("THUNDERSTORM WIND")
levels(storm_data$EVTYPE)[grep("Strong winds",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("THUNDERSTORM WIND")
levels(storm_data$EVTYPE)[grep("extreme heat",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Excessive heat")
levels(storm_data$EVTYPE)[grep("ice",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("ice storm")
levels(storm_data$EVTYPE)[grep("icy roads",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Frost/Freeze")
levels(storm_data$EVTYPE)[grep("Flood/flash",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flash flood")
levels(storm_data$EVTYPE)[grep("DRY MICROBURST",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("SEICHE")
levels(storm_data$EVTYPE)[grep("WILD/FOREST FIRE",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("WILDFIRE")
levels(storm_data$EVTYPE)[grep("Erosion/Cstl Flood",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Coastal Flood")
levels(storm_data$EVTYPE)[grep("FOG",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Dense fog")
levels(storm_data$EVTYPE)[grep("Unseasonable Cold",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Extreme cold/wind chill")
levels(storm_data$EVTYPE)[grep("Coastal Flood",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Coastal Flood")
levels(storm_data$EVTYPE)[grep("urban",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("flood")
levels(storm_data$EVTYPE)[grep("Extreme Windchill",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Extreme Cold/Wind Chill")
levels(storm_data$EVTYPE)[grep("unseasonably cold",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Extreme cold/wind chill")
levels(storm_data$EVTYPE)[grep("dam break",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Lakeshore Flood")
levels(storm_data$EVTYPE)[grep("astronomical",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Astronomical Low tide")
levels(storm_data$EVTYPE)[grep("typhoon",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Hurricane (Typhoon)")
levels(storm_data$EVTYPE)[grep("unseasonal rain",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy Rain")
levels(storm_data$EVTYPE)[grep("Landslump",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Avalanche")
levels(storm_data$EVTYPE)[grep("Coastal erosion",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Coastal Flood")
levels(storm_data$EVTYPE)[grep("Wind and wave",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("High wind")
levels(storm_data$EVTYPE)[grep("mudslide",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("avalanche")
levels(storm_data$EVTYPE)[grep("landslide",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("avalanche")
levels(storm_data$EVTYPE)[grep("cold",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("cold/Wind chill")
levels(storm_data$EVTYPE)[grep("snow",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy snow")
levels(storm_data$EVTYPE)[grep("Coastal storm",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Marine High Wind")
levels(storm_data$EVTYPE)[grep("Brush fire",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Wildfire")
levels(storm_data$EVTYPE)[grep("winter weather",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("winter weather")
levels(storm_data$EVTYPE)[grep("warm",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Excessive heat")
levels(storm_data$EVTYPE)[grep("wind",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("High wind")
levels(storm_data$EVTYPE)[grep("seas",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Marine hail")
levels(storm_data$EVTYPE)[grep("surf",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("High surf")
levels(storm_data$EVTYPE)[grep("marine",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Marine hail")
levels(storm_data$EVTYPE)[grep("avalan",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Avalanche")
levels(storm_data$EVTYPE)[grep("rain",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy rain")
levels(storm_data$EVTYPE)[grep("hypoth",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Extreme Cold/Wind Chill")
levels(storm_data$EVTYPE)[grep("current",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("marine hail")
levels(storm_data$EVTYPE)[grep("heat",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Excessive heat")
levels(storm_data$EVTYPE)[grep("water",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("wintry",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Winter weather")
levels(storm_data$EVTYPE)[grep("wave",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Marine hail")
levels(storm_data$EVTYPE)[grep("freezing",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Frost/Freeze")
levels(storm_data$EVTYPE)[grep("low temperature",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Cold/wind chill")
levels(storm_data$EVTYPE)[grep("glaze",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Frost/Freeze")
levels(storm_data$EVTYPE)[grep("precip",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Heavy rain")
levels(storm_data$EVTYPE)[grep("coastalstorm",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Marine thunderstorm wind")
levels(storm_data$EVTYPE)[grep("high swells",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("flood")
levels(storm_data$EVTYPE)[grep("hyperth",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Excessive heat")
levels(storm_data$EVTYPE)[grep("drowning",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("Flood")
levels(storm_data$EVTYPE)[grep("high",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("High wind")
levels(storm_data$EVTYPE)[grep("dust devil",levels(storm_data$EVTYPE),ignore.case=T,perl=T)]<-toupper("dust devil")

#unique(as.character(storm_data$EVTYPE))[which(!(unique(as.character(storm_data$EVTYPE)) %in% real_events))]
