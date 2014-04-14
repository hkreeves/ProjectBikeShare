##
## Project BikeShare
##
## Full data processing
##
## April 6th, 2014
##

library(plyr)

# load data
load("./stationInfo.Rda")
setwd("./data/")
all.quarters <- c("q1", "q2", "q3", "q4")
all.2011.files <- paste0("2011", all.quarters, ".csv")
all.2012.files <- paste0("2012", all.quarters, ".csv")
all.2013.files <- paste0("2013", all.quarters, ".csv")

bs2011 <- lapply(all.2011.files, read.csv, stringsAsFactors=F) 
bs2011 <- do.call(rbind, bs2011)

bs2012 <- lapply(all.2012.files, read.csv, stringsAsFactors=F) 

bs2013 <- lapply(all.2013.files, read.csv, stringsAsFactors=F) 

######## inspection and processing (2011 data) ##############
str(bs2011)

# remove records of missing bikes (no End.station)
bs2011[bs2011 == ""] <- NA
bs2011 <- bs2011[!is.na(bs2011$End.station),]

# stationID is part of the Start.station and End.station variable
# eg. "14th & Harvard St NW (31105)"
# need extraction
start.id <- regmatches(bs2011$Start.station, regexec("\\((.*)\\)", bs2011$Start.station))
bs2011$Start.Terminal <- as.integer(ldply(start.id, .parallel=T)[,2])

end.id <- regmatches(bs2011$End.station, regexec("\\((.*)\\)", bs2011$End.station))
bs2011$End.Terminal <- as.integer(ldply(end.id, .parallel=T)[,2])

# convert Duration to Duration.Sec variable
duration <- ldply(regmatches(bs2011$Duration, 
regexec("^([0-9]*)h ([0-9]*)min. ([0-9]*)sec.", bs2011$Duration)), .parallel=T)[,2:4]
duration <- sapply(duration, as.integer)
bs2011$Duration.Sec <- duration[,1] * 360 + duration[,2]* 60 +duration[,3]

# rename variables
colnames(bs2011)[2] <- "Start.Date"
colnames(bs2011)[3] <- "End.Date"
colnames(bs2011)[6] <- "Bike"
colnames(bs2011)[7] <- "Type"

#################### 2012 data ######################

[ Due to the crash of R, all gone. FUXK R! ]

#################### 2013 data ######################
str(2013)
col.names <- c("Duration", "Start.Date", "Start.Station", "Start.Terminal", 
  "End.Date", "End.Station", "End.Terminal", "Bike", "Type")
for(i in 1:4){
  colnames(bs2013[[i]]) <- col.names}
bs2013 <- do.call(rbind, bs2013)

# remove records with missing stations
bs2013[bs2013 == ""] <- NA
sum(is.na(bs2013$End.Station)) # 3
bs2013 <- bs2013[!is.na(bs2013$End.Station), ]

duration <- ldply(regmatches(bs2013$Duration, 
regexec("^([0-9]*)h ([0-9]*)m ([0-9]*)s", bs2013$Duration)), .parallel=T)[,2:4]
duration <- sapply(duration, as.integer)
bs2013$Duration.Sec <- duration[,1] * 360 + duration[,2]* 60 +duration[,3]

#### missing station issue #######
all.term <- unique(c(bs2011$Start.Term, bs2011$End.Term))
missing.term2011 <- all.term[!(all.term %in% stationBook$terminalName)]
for(miss in missing.term2011){
  print(bs2011$Start.station[bs2011$Start.Term == miss][1])
}
# [1] "Alta Bicycle Share Warehouse (31902)"
# [1] "Alta Warehouse Station (31888)"
# [1] "Alta Bicycle Share Demonstration Station (31999)"
# [1] "Birthday Station (31900)"

all.term <- unique(c(bs2013$Start.Term, bs2013$End.Term))
missing.term2013 <- all.term[!(all.term %in% stationBook$terminalName)]
# 32901 31311 32902 32900
for(miss in missing.term2013){
  print(bs2013[bs2013$End.Term == miss,][1,c(3,4,6,7)])}

# export
selected.names <- c("Duration.Sec", "Start.Date", "Start.Terminal", "End.Date", "End.Terminal", "Type")
write.csv(bs2011[selected.names], "./processed data/bs2011combined.csv", row.names=F)
write.csv(bs2013[selected.names], "./processed data/bs2013combined.csv", row.names=F)         



