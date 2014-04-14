##
## Project BikeShare
##
## Full Analysis
##
## April 13th, 2014
##

# load stationBook data
load("./stationInfo.Rda")

# import user defined functions
source("./script/functions.R")

# Example: 2012 data
# first load the preprocessed data for 2012
bs2012 <- read.csv("./data/processed data/bs2012combined.csv", stringsAsFactors=F)
str(bs2012)

# convert the type of Start.Date and End.Date to datetime
bs2012$Start.Date <- strptime(bs2012$Start.Date, "%m/%d/%Y %H:%M")
bs2012$End.Date <- strptime(bs2012$End.Date, "%m/%d/%Y %H:%M")
#bs2012$Type <- as.factor(bs2012$Type)

summary(as.Date(bs2012$Start.Date))

# try the createDistanceMatrix function
# arguments of createDistanceMatrix: data, start.date, end.date, (opt)nday
res <- createDistanceMatrix(bs2012, "2012-03-01", "2012-06-30")
dm <- res$distMatrix
util <- res$popStation

# use DBSCAN to identify clusters
# arguments of createCluster: distance.matrix, threshold (per week), min.neighbor
clust <- createCluster(dm, 37, 3)$data

# plot onto a map
dc.map <- get_map(location="Washinton, DC", zoom=12, maptype="roadmap", 
color="bw", source="google")

bs.plot <- plotCluster(clust, util)
print(bs.plot)

