##
## Project BikeShare
##
## Functions
##
## April 13th, 2014
##

library(data.table)
library(plyr)
library(fpc)
library(RColorBrewer)
library(ggmap)

# create graph data in the following format: terminal1, terminal2, frequency
createGraph <- function(bs0, nDay){
  nWeek <- nDay/7

  # find the frequency of each path ID
  # we use data.table to preform aggregation and sorting
  bs0 <- as.data.table(bs0)
  bs0 <- bs0[, .N, by=c("Start.Terminal", "End.Terminal")]
  bs0 <- bs0[order(Start.Terminal, End.Terminal)]

  bs0$N <- bs0$N/nWeek
  bs0 <- as.data.frame(bs0)
  names(bs0) <- c("Term1", "Term2", "Freq")
 
  # return 
  return(bs0)
}

# create distanc matrix for data
createDistanceMatrix <- function(bs.graph, nDay){    
  nWeek <- nDay/7

  ######
  # construct distance matrix (adjacent matrix) using igraph
  ######
  
  bs.g <- graph.data.frame(bs.graph)
  
  # use get.adjacency() to obtain the adjacent matrix  
  dist.mtrx <- as.matrix(get.adjacency(bs.g, attr="Freq")) 
  dist.mtrx <- dist.mtrx + t(dist.mtrx)
  
  # convert frequency to distance using dist=1/(Freq/nWeek + 1)
  dist.mtrx <- 1/(dist.mtrx + 1)
  dist.mtrx <- data.frame(as.integer(rownames(dist.mtrx)), dist.mtrx)
  names(dist.mtrx)[1] <- "TermID"
  
  # get utilization of each station
  all.term <- sort(as.integer(V(bs.g)$name))
  pop.station <- data.frame(list(TermID=all.term, Freq=0))
  pop.start <- tapply(bs.graph$Freq, bs.graph$Term1, sum) / 7
  pop.start <- data.frame(list(TermID=names(pop.start), Out=pop.start))
  pop.station <- merge(pop.station, pop.start, by="TermID", all.x=T)

  pop.end <- tapply(bs.graph$Freq, bs.graph$Term2, sum) / 7
  pop.end <- data.frame(list(TermID=names(pop.end), In=pop.end))
  pop.station <- merge(pop.station, pop.end, by="TermID", all.x=T)
  pop.station[is.na(pop.station)] <- 0
  pop.station$Freq <- pop.station$In + pop.station$Out 

  # return distance matrix and utilization
  return(list(distMatrix=dist.mtrx, popStation=pop.station))
}

# perform clustering analysis using DBSCAN algorithm
createCluster <- function(dist, threshold, minNeighbor){
  eps <- 1/(threshold + 1)
  cluster1 <- dbscan(as.matrix(dist[,-1]), method="dist", eps=eps, MinPts=minNeighbor)
  message("Cluster Assignment:")
  print(table(cluster1$cluster))
  
  bsClust <- data.frame(list(TermID=dist$TermID, Cluster=cluster1$cluster))
  return(list(data=bsClust, clusterObject=cluster1))
}

# plot on a map
plotCluster <- function(clust, util, base=dc.map, noise=T){
  # combine clust and util
  # then merge clust and stationBook on the common variable "TermID"
  clust <- cbind(clust, util$Freq)
  names(clust)[3] <- "Freq"
  station <- merge(clust, stationBook[c("terminalName", "lat", "long")], 
    by.x="TermID", by.y="terminalName", suffixes = NULL, all=F)
  station$lat <- as.numeric(station$lat)
  station$long <- as.numeric(station$long)
  #str(station)
  #print(head(station))

  # clusters
  clust.names <- names(table(clust$Cluster))
  clust.names <- paste("Comm", clust.names)
  if(noise)  clust.names[1] <-  "Unclustered"

  # color scheme
  color <- c("grey32", brewer.pal(9, "Set1"))
  scale_colour_brewer(palette="Set3")

  # plot on the map
  # color coding indicates cluster
  # area of point indicates utilization (per day)
  # area scale
  area.scale <- c(10, 50, 100, 200)
  station.map <- 
    ggmap(dc.map, extend="device") +
    geom_point(
      aes(x=long, 
        y=lat, 
        fill=factor(Cluster), 
        size=sqrt(Freq)),
      shape=21,
      color="darkblue",
      data=station, 
      alpha=0.9) +
    scale_fill_manual(values=color, name="Community", labels=clust.names) +
    scale_size_area(breaks=sqrt(area.scale), labels=area.scale, name="Exchanges per day") +
    theme(axis.title=element_blank())
}

# draw lines for frequent paths on top of the map plot
# "frequent" is specified by a minimum cutoff, minEx
plotLink <- function(bs.graph, clust, minEx=40, base.plot){
  # sum up A-B and B-A
  bs.graph <- merge(bs.graph, bs.graph, by.x=c("Term1", "Term2"), by.y=c("Term2", "Term1"))
  bs.graph$Freq <- bs.graph$Freq.x + bs.graph$Freq.y
  bs.graph <- bs.graph[-c(3,4)]

  station <- merge(bs.graph, stationBook[c("terminalName", "lat", "long")], 
    by.x="Term1", by.y="terminalName", all=F)
  station <- merge(station, stationBook[c("terminalName", "lat", "long")], 
    by.x="Term2", by.y="terminalName", suffixes = c("", ".b"), all=F)
  station <- merge(station, clust, by.x="Term1", by.y="TermID")

  # subset bs.graph
  station <- subset(station, (Term2 > Term1) & (Freq >= minEx))

  bs.plot2 <-  base.plot +
    geom_segment(
      aes(x = long, 
        y = lat, 
        xend = long.b, 
        yend = lat.b, 
        color=factor(Cluster), 
        size=Freq/100), 
      alpha=0.4, 
      lineend="butt", 
      data=station,
      show_guide=F,
    ) + 
    scale_color_manual(values=color) 

  return(bs.plot2)
}

