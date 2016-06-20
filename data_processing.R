#load dependencies
library(R.utils)
library(ggplot2)
library(gridExtra)
library(dplyr)

# get software environment
sessionInfo()

# notes:
# 2.1 set wd
# 2. look at the data

# download dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("./data")){dir.create("./data")}
download.file(fileUrl,destfile="./data/StormData.csv.bz2")

#unzip file, this operation speeds up reading the data
bunzip2("./data/StormData.csv.bz2", "./data/StormData.csv", remove = FALSE, skip = TRUE)

#before loading a data set check a size of it
size_of_file <- (file.info("./data/StormData.csv")$size)/2^20
size_of_file

#loading dataset
source_df <- read.csv("./data/StormData.csv", header = TRUE, stringsAsFactors = FALSE)

#look at the data
head(source_df)

str(source_df)

# data processing
damage_df <- select(source_df, EVTYPE,FATALITIES, INJURIES, PROPDMG,PROPDMGEXP, CROPDMG, CROPDMGEXP)

#prepare property damamge data
damage_df$PROPDMGEXP <- as.factor(damage_df$PROPDMGEXP)
unique(damage_df$PROPDMGEXP)


# Sorting the property exponent data
damage_df$PROPEXP[damage_df$PROPDMGEXP == "0"] <- 1
damage_df$PROPEXP[damage_df$PROPDMGEXP == "1"] <- 10
damage_df$PROPEXP[damage_df$PROPDMGEXP == "h"] <- 1e+02
damage_df$PROPEXP[damage_df$PROPDMGEXP == "2"] <- 1e+02
damage_df$PROPEXP[damage_df$PROPDMGEXP == "H"] <- 1e+02
damage_df$PROPEXP[damage_df$PROPDMGEXP == "K"] <- 1e+03
damage_df$PROPEXP[damage_df$PROPDMGEXP == "3"] <- 1e+04
damage_df$PROPEXP[damage_df$PROPDMGEXP == "4"] <- 1e+04
damage_df$PROPEXP[damage_df$PROPDMGEXP == "5"] <- 1e+05
damage_df$PROPEXP[damage_df$PROPDMGEXP == "m"] <- 1e+06
damage_df$PROPEXP[damage_df$PROPDMGEXP == "M"] <- 1e+06
damage_df$PROPEXP[damage_df$PROPDMGEXP == "6"] <- 1e+06
damage_df$PROPEXP[damage_df$PROPDMGEXP == "7"] <- 1e+07
damage_df$PROPEXP[damage_df$PROPDMGEXP == "8"] <- 1e+08
damage_df$PROPEXP[damage_df$PROPDMGEXP == "B"] <- 1e+09

# give 0 to invalid exponent data, so they not count in
damage_df$PROPEXP[damage_df$PROPDMGEXP == "+"] <- 0
damage_df$PROPEXP[damage_df$PROPDMGEXP == "-"] <- 0
damage_df$PROPEXP[damage_df$PROPDMGEXP == "?"] <- 0
damage_df$PROPEXP[damage_df$PROPDMGEXP == ""] <-  0
# compute the property damage value
damage_df$PROPDMGVAL <- damage_df$PROPDMG * damage_df$PROPEXP


# prepare crop damage data

damage_df$CROPDMGEXP <- as.factor(damage_df$CROPDMGEXP)
unique(damage_df$CROPDMGEXP)

damage_df$CROPEXP[damage_df$CROPDMGEXP == "0"] <- 1
damage_df$CROPEXP[damage_df$CROPDMGEXP == "2"] <- 1e+02
damage_df$CROPEXP[damage_df$CROPDMGEXP == "k"] <- 1e+03
damage_df$CROPEXP[damage_df$CROPDMGEXP == "K"] <- 1e+03
damage_df$CROPEXP[damage_df$CROPDMGEXP == "m"] <- 1e+06
damage_df$CROPEXP[damage_df$CROPDMGEXP == "M"] <- 1e+06
damage_df$CROPEXP[damage_df$CROPDMGEXP == "B"] <- 1e+09

# give 0 to invalid exponent data, so they not count in
damage_df$CROPEXP[damage_df$CROPDMGEXP == "?"] <- 0
damage_df$CROPEXP[damage_df$CROPDMGEXP == ""] <-  0

# compute the property damage value
damage_df$CROPDMGVAL <- damage_df$CROPDMG * damage_df$CROPEXP


#part 1 here we aggregate data and count injures/fatalities by event type and build figures
agg_fatalities <- aggregate(damage_df$FATALITIES, by=list(damage_df$EVTYPE),FUN=sum, na.rm=FALSE)

agg_fatalities <- agg_fatalities[order(-agg_fatalities$x),][1:10,]

agg_fatalities$Group.1<- factor(agg_fatalities$Group.1, 
                             levels = agg_fatalities$Group.1[-agg_fatalities$x])


agg_injures <- aggregate(damage_df$INJURIES, by=list(damage_df$EVTYPE),FUN=sum, na.rm=FALSE)

agg_injures <- agg_injures[order(-agg_injures$x),][1:10,]

agg_injures$Group.1<- factor(agg_injures$Group.1, 
                                 levels = agg_injures$Group.1[-agg_injures$x])

ggp1 <- ggplot(agg_fatalities, aes(x=Group.1, y = x)) + geom_bar(stat = "identity", fill = "purple") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Event Type") + 
  ylab("Count of events") + ggtitle("Number of fatalities in each event type")

ggp2 <- ggplot(agg_injures, aes(x=Group.1, y = x)) + geom_bar(stat = "identity", fill = "purple") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Event Type") + 
  ylab("Count of events") + ggtitle("Number of injures in each event type")

grid.arrange(ggp1, ggp2, ncol=1, nrow =2)


# part 2 here we aggregate data and count total money equivalent of damage done by event type and build figures

agg_prop_damage <- aggregate(damage_df$PROPDMGVAL, by=list(damage_df$EVTYPE),FUN=sum, na.rm=FALSE)

agg_prop_damage <- agg_prop_damage[order(-agg_prop_damage$x),][1:10,]

agg_prop_damage$Group.1<- factor(agg_prop_damage$Group.1, 
                                 levels = agg_prop_damage$Group.1[-agg_prop_damage$x])

agg_crop_damage <- aggregate(damage_df$CROPDMGVAL, by=list(damage_df$EVTYPE),FUN=sum, na.rm=FALSE)

agg_crop_damage <- agg_crop_damage[order(-agg_crop_damage$x),][1:10,]

agg_crop_damage$Group.1<- factor(agg_crop_damage$Group.1, 
                                 levels = agg_crop_damage$Group.1[-agg_crop_damage$x])

ggp3 <- ggplot(agg_prop_damage, aes(x=Group.1, y = x/10^9)) + geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("Event Type") + 
  ylab("Amount of damage, billions, usd") + ggtitle("Total property damage in each event type")

ggp4 <- ggplot(agg_crop_damage, aes(x=Group.1, y = x/10^9)) + geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("Event Type") + 
  ylab("Amount of damage, billions, usd") + ggtitle("Total crop damage in each event type")

grid.arrange(ggp3, ggp4, ncol=1, nrow =2)