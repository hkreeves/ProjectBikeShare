##
## Project BikeShare
##
## Advanced Clustering
##
## April 17th, 2014
##

library(igraph)

# create a graph on the frequency (per 7 days) information
bs.graph <- createGraph(bs2012, "2012-03-01", "2012-06-30")

# convert to igraph object
bs.g <- graph.data.frame(bs.graph)
summary(bs.g)

# delete vertices (stations) which do not appear in stationBook
bs.g <- delete.vertices(bs.g, V(bs.g)[!V(bs.g)$name %in% stationBook$terminalName])

# layout 1
bs.layout <- layout.fruchterman.reingold(bs.g, weights=get.edge.attribute(bs.g, "Freq"))

# layout 2: geographic coordinates
station <- data.frame(list(TermID=as.integer(V(bs.g)$name)))
station <- merge(station, stationBook[c("terminalName", "lat", "long")], 
    by.x="TermID", by.y="terminalName", suffixes = NULL, all.x=T)
# fill the missing values
geo.layout <- station[2:3]
#geo.layout[is.na(geo.layout$long),] <- colMeans(geo.layout, na.rm=T)
geo.layout <- as.matrix(geo.layout)

plot(bs.g, layout=bs.layout, edge.arrow.size=.5)

# try coreness
coreness = graph.coreness(bs.g)
plot(bs.g, layout=bs.layout, vertex.color=coreness, edge.arrow.size=.5)

# try walktrap algorithm
set.seed(334)
clust.wt <- walktrap.community(bs.g, weights=E(bs.g)$Freq, steps=100)
plot(bs.g, layout=bs.layout, vertex.color=clust.wt$member, edge.arrow.size=.5)

# geographical plot
comm.wt <- as.data.frame(cbind(as.integer(clust.wt$names), clust.wt$membership))
names(comm.wt) <- c("TermID", "Cluster")
util <- data.frame(list(Freq=t/7))
bs.plot <- plotCluster(comm.wt, util, noise=F)