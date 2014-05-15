##
## Project BikeShare
##
## Full Analysis
##
## Author: Yinyue Hu
## Last updated: April 22nd, 2014
##

# load stationBook data
load("./stationInfo.Rda")

# import user defined functions
source("./script/functions.R")

# Example: 2012 data
# first load the preprocessed data for 2012
bs2013 <- read.csv("./data/processed data/bs2013combined.csv", stringsAsFactors=F)
str(bs2013)

# convert the type of Start.Date and End.Date to datetime
bs2013$Start.Date <- strptime(bs2013$Start.Date, "%m/%d/%Y %H:%M")
bs2013$End.Date <- strptime(bs2013$End.Date, "%m/%d/%Y %H:%M")
#bs2012$Type <- as.factor(bs2012$Type)

# subset data according to date range or any other criteria
bs0 <- subset(bs2013, Quarter == "Q1")

# create graph data from the dataset
bs.graph <- createGraph(bs2013)

# try the createDistanceMatrix function
res <- createDistanceMatrix(bs.graph)
dm <- res$distMatrix
util <- res$popStation

# use DBSCAN to identify clusters
# arguments of createCluster: distance.matrix, threshold (per week), min.neighbor
clust <- createCluster(dm, 37, 3)$data

# plot onto a map
dc.map <- get_map(location="Washington, DC", zoom=12, maptype="roadmap", color="color", source="google")

# plot points
bs.plot <- plotCluster(clust, util)
# plot links, specified by a lower cutoff.
bs.plot <- plotLink(bs.graph, clust, 8, bs.plot) 
print(bs.plot)

