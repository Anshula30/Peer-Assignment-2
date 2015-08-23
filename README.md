Health and Economic Impact of Weather Events in the US
======================================================


Storms and other severe weather events can cause both public health and economic
problems for communities and municipalities. Many severe events can result in
fatalities, injuries, and property damage, and preventing such outcomes to the extent
possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric
Administration's (NOAA) storm database. This database tracks characteristics of major
storms and weather events in the United States, including when and where they occur, as
well as estimates of any fatalities, injuries, and property damage.


Synopsis
========

The analysis on the storm event database revealed that tornadoes are the most
dangerous weather event to the population health. The second most dangerous
event type is the excessive heat. The economic impact of weather events was
also analyzed. Flash floods and thunderstorm winds caused billions of dollars
in property damages between 1950 and 2011. The largest crop damage caused by
drought, followed by flood and hails.


Data Processing
===============

The analysis was performed on
[Storm Events Database](http://www.ncdc.noaa.gov/stormevents/ftp.jsp), provided by
[National Climatic Data Center](http://www.ncdc.noaa.gov/). The data is from a comma-separated-value file available
[here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).
There is also some documentation of the data available
[here](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).

The first step is to read the data into a data frame.

```r
storm <- read.csv(bzfile("data/repdata-data-StormData.csv.bz2"))
```

Before the analysis, the data need some preprocessing. Event types don't have a
specific format. For instance, there are events with types `Frost/Freeze`,
`FROST/FREEZE` and `FROST\\FREEZE` which obviously refer to the same type of
event.


```r
# number of unique event types
length(unique(storm$EVTYPE))
```

```
## [1] 985
```


```r
# translate all letters to lowercase
event_types <- tolower(storm$EVTYPE)
# replace all punct. characters with a space
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
length(unique(event_types))
```

```
## [1] 874
```

```r
# update the data frame
storm$EVTYPE <- event_types
```


No further data preprocessing was performed although the event type field can be
processed further to merge event types such as `tstm wind` and `thunderstorm wind`. 
After the cleaning, as expected, the number of unique event types reduce
significantly. For further analysis, the cleaned event types are used.



Dangerous Events with respect to Population Health
================================================

To find the event types that are most harmful to population health, the number
of casualties are aggregated by the event type.


```r
library(plyr)
casualties <- ddply(storm, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused most death and injury
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 10)
```

Top 10 events that caused largest number of deaths are


```r
fatal_events[, c("EVTYPE", "fatalities")]
```

```
##             EVTYPE fatalities
## 737        tornado       5633
## 109 excessive heat       1903
## 132    flash flood        978
## 234           heat        937
## 400      lightning        816
## 760      tstm wind        504
## 148          flood        470
## 511    rip current        368
## 309      high wind        248
## 11       avalanche        224
```

Top 10 events that caused most number of injuries are


```r
injury_events[, c("EVTYPE", "injuries")]
```

Economic Effects of Weather Events
==================================

To analyze the impact of weather events on the economy, available property
damage and crop damage reportings/estimates were used.

In the raw data, the property damage is represented with two fields, a number
`PROPDMG` in dollars and the exponent `PROPDMGEXP`. Similarly, the crop damage
is represented using two fields, `CROPDMG` and `CROPDMGEXP`. The first step in the
analysis is to calculate the property and crop damage for each event.


```r
exp_transform <- function(e) {
    # h -> hundred, k -> thousand, m -> million, b -> billion
    if (e %in% c('h', 'H'))
        return(2)
    else if (e %in% c('k', 'K'))
        return(3)
    else if (e %in% c('m', 'M'))
        return(6)
    else if (e %in% c('b', 'B'))
        return(9)
    else if (!is.na(as.numeric(e))) # if a digit
        return(as.numeric(e))
    else if (e %in% c('', '-', '?', '+'))
        return(0)
    else {
        stop("Invalid exponent value.")
    }
}
```

```r
prop_dmg_exp <- sapply(storm$PROPDMGEXP, FUN=exp_transform)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(storm$CROPDMGEXP, FUN=exp_transform)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_exp)
```

```r
# Compute the economic loss by event type
library(plyr)
econ_loss <- ddply(storm, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)
```


Top 10 events that caused most property damage (in dollars) are as follows


```r
prop_dmg_events[, c("EVTYPE", "prop_dmg")]
```

```
##                 EVTYPE  prop_dmg
## 132        flash flood 6.820e+13
## 694 thunderstorm winds 2.087e+13
## 737            tornado 1.079e+12
## 203               hail 3.158e+11
## 400          lightning 1.729e+11
## 148              flood 1.447e+11
## 361  hurricane typhoon 6.931e+10
## 155           flooding 5.921e+10
## 581        storm surge 4.332e+10
## 264         heavy snow 1.793e+10
```
