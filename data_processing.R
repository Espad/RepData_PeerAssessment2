#dependencies
library(R.utils)
library(ggplot2)
library(gridExtra)

# download dataset
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl,destfile="./data/StormData.csv.bz2")


#loading dataset
source_df <- read.csv("./data/StormData.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


agg_fatalities <- aggregate(source_df$FATALITIES, by=list(source_df$EVTYPE),FUN=sum, na.rm=FALSE)

agg_fatalities <- agg_fatalities[order(-agg_fatalities$x),]

agg_fatalities <- agg_fatalities[1:20,]

ggp1 <- ggplot(agg_fatalities, aes(x=Group.1, y = x)) + geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

agg_injures <- aggregate(source_df$INJURIES, by=list(source_df$EVTYPE),FUN=sum, na.rm=FALSE)

agg_injures <- agg_injures[order(-agg_injures$x),]

agg_injures <- agg_injures[1:20,]

ggp2 <- ggplot(agg_injures, aes(x=Group.1, y = x)) + geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(ggp1, ggp2, ncol=1, nrow =2)