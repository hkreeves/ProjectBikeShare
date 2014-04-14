##
## Project BikeShare
##

library(plyr)

## EXPLORATION ON DATA
## a subset of available data corresponding to Q1 2012
url<- "http://capitalbikeshare.com/assets/files/trip-history-data/2012-1st-quarter.csv"
download.file(url, "./data/2012q1.csv")

bs <- read.csv("./data/2012q1.csv", stringsAsFactors=F)
str(bs)
selected.vars <- names(bs)
selected.vars <- selected.vars[-c(4,7)] # remove the station address text variables
bs <- bs[selected.vars]

# Processing variables: 1. make Type factors. 
# 2. convert Start.date and End.date to POSIXt timestamps
# (joke, already existed) 2. convert Duration to time length
bs$Type <- as.factor(bs$Type)
#duration <- ldply(bs$Duration, function(x) as.numeric(strsplit(x, split="[h|m|sec\\.] *")[[1]][1:3]))
#bs$Duration <- duration[,1] * 360 + duration[,2]* 60 +duration[,3]
bs$Start.date <- strptime(bs$Start.date, "%m/%d/%Y %H:%M")
bs$End.date <- strptime(bs$End.date, "%m/%d/%Y %H:%M")

# some plots
hist(as.Date(bs$Start.date), "days", freq=T)
freqStart <- sort(table(bs$Start.Term), dec=T)
freqEnd <- sort(table(bs$End.Term), dec=T)

# popular stations
plot(stationBook$long, stationBook$lat, type="p", pch=19, col="steelblue2")
points(stationBook$long[stationBook$term %in% names(freqStart)[1:25]],
stationBook$lat[stationBook$term %in% names(freqStart)[1:25]],
col="red", pch=19)
points(stationBook$long[stationBook$term %in% names(freqEnd)[1:25]],
stationBook$lat[stationBook$term %in% names(freqEnd)[1:25]],
col="green", pch=1, cex=1.05)

save(stationBook, bs, freqStart, freqEnd, file="./temp.Rda")
