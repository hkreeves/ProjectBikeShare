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

# Add new features: Day, Month, Quarter, Long.Trip
bs2012$Day <- weekdays(bs2012$Start.Date)
bs2012$Month <- months(bs2012$Start.Date)
bs2012$Quarter <- quarters(bs2012$Start.Date)
bs2012$Long.Trip <- bs2012$Duration.Sec > 1860

day.lvl <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mon.lvl <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
qtr.lvl <- c("Q1", "Q2", "Q3", "Q4")

bs2012$Day <- factor(bs2012$Day, ordered=T, levels=day.lvl)
bs2012$Month <- factor(bs2012$Month, ordered=T, levels=mon.lvl)
bs2012$Quarter <- factor(bs2012$Quarter, ordered=T, levels=qtr.lvl)
bs2012$Type <- as.factor(bs2012$Type)

# subset data according to date range or any other criteria
bs0 <- subset(bs2013, Quarter == "Q3")

# create graph data from the dataset
bs.graph <- createGraph(bs0)

# try the createDistanceMatrix function
res <- createDistanceMatrix(bs.graph)
dm <- res$distMatrix
util <- res$popStation

# use DBSCAN to identify clusters
# arguments of createCluster: distance.matrix, threshold (per week), min.neighbor
clust <- createCluster(dm, 42, 3)$data

# plot onto a map
dc.map <- get_map(location="Washington, DC", zoom=12, maptype="roadmap", 
color="color", source="google")

bs.plot <- plotCluster(clust, util)
bs.plot <- plotLink(bs.graph, clust, 42, bs.plot) 
print(bs.plot)

