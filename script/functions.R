##
## Project BikeShare
##
## Functions
##
## April 13th, 2014
##

library(fpc)
library(RColorBrewer)
library(ggmap)

# create distanc matrix for data, optionally within a date range
createDistanceMatrix <- function(bs, beginDate=min(as.Date(bs$Start.Date)), 
endDate=max(as.Date(bs$End.Date)), nDay=NA){
  # if nDay is not specified, meaning that the date range is continuous
  if(is.na(nDay)){
    beginDate <- as.Date(beginDate)
    endDate <- as.Date(endDate)
    nDay <- as.numeric(endDate - beginDate + 1)
    bs0 <- subset(bs, (as.Date(Start.Date) >= beginDate) & (as.Date(Start.Date) <= endDate))}
    
  nWeek <- nDay/7
  #print(str(bs0))

  # create path IDs formated as "StartTerminal-EndTerminal" and the opposite
  bs0$S2E <- paste(bs0$Start.Term, bs0$End.Term, sep="-")
  bs0$E2S <- paste(bs0$End.Term, bs0$Start.Term, sep="-")

  # find the frequency of each path ID
  link1.count <- table(bs0$S2E)
  link1 <- as.data.frame.table(link1.count)
  link2.count <- table(bs0$E2S)
  link2 <- as.data.frame.table(link2.count)

  # merge frequencies between "A-B" and "B-A"
  link.count <- merge(link1, link2, by="Var1", all=T)
  link.count[is.na(link.count)] <- 0

  link.count$total <- link.count$Freq.x + link.count$Freq.y

  # full list of all possible path IDs
  all.term <- sort(unique(c(bs0$Start.Term, bs0$End.Term)))
  all.pair <- cbind(rep(all.term, each=length(all.term)), rep(all.term, length(all.term)))
  full.link <- data.frame(list(Var1=paste(all.pair[,1], all.pair[,2], sep="-"), Freq=rep(0, nrow(all.pair))))

  # merge link.count to full.link
  full.link <- merge(full.link, link.count, by="Var1", all.x=T)
  full.link[is.na(full.link)] <- 0
  full.link$Freq <- full.link$Freq.x + full.link$Freq.y
  full.link <- full.link[,1:2]
  nz.full.link <- subset(full.link, Freq > 0)
  message("Statistics of #exchanges per 7 days for each path:") 
  print(quantile(nz.full.link$Freq/nWeek, seq(0.9, 1, 0.02)))
  message(paste("mean #exchanges per 7 days:", round(mean(full.link$Freq/nWeek), 4)))

  # split the pathID back to Start, End
  # compute the "distance" using the following formula: dist = 1/(Freq/nWeek + 1)
  triples <- data.frame(cbind(all.pair, 1/(full.link$Freq/nWeek + 1)))
  names(triples) <- c("Term1", "Term2", "Dist")

  # reshape the long data to wide
  dist.mtrx <- reshape(triples, timevar="Term1", idvar="Term2", direction="wide")
  
  # get utilization of each station
  pop.base <- data.frame(list(Var1=all.term, Freq=0))
  pop.start <- as.data.frame.table(table(bs0$Start.Term)/nDay)
  pop.start <- merge(pop.base, pop.start, by="Var1", all.x=T)
  pop.start[is.na(pop.start)] <- 0
  pop.start$Freq <- pop.start$Freq.x + pop.start$Freq.y
  pop.start <- pop.start[, c("Var1", "Freq")]

  pop.end <- as.data.frame.table(table(bs0$End.Term)/nDay)
  pop.end <- merge(pop.base, pop.end, by="Var1", all.x=T)
  pop.end[is.na(pop.end)] <- 0
  pop.end$Freq <- pop.end$Freq.x + pop.end$Freq.y
  pop.end <- pop.end[, c("Var1", "Freq")]

  pop.station <- pop.start
  pop.station$Freq <- pop.start$Freq + pop.end$Freq

  colnames(pop.station) <- c("TermID", "Freq")

  # return distance matrix and utilization
  return(list(distMatrix=dist.mtrx[], popStation=pop.station))
}

# perform clustering analysis using DBSCAN algorithm
createCluster <- function(dist, threshold, minNeighbor){
  eps <- 1/(threshold + 1)
  cluster1 <- dbscan(as.matrix(dist[,-1]), method="dist", eps=eps, MinPts=minNeighbor)
  message("Cluster Assignment:")
  print(table(cluster1$cluster))
  
  bsClust <- data.frame(list(TermID=dist$Term2, Cluster=cluster1$cluster))
  return(list(data=bsClust, clusterObject=cluster1))
}

# plot on a map
plotCluster <- function(clust, util, base=dc.map){
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
  clust.names[1] <-  "Unclustered"

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
      aes(x=long, y=lat, col=factor(Cluster), size=sqrt(Freq + 1)),
      data=station, alpha=0.5) +
    scale_colour_manual(values=color, name="Community", labels=clust.names) +
    scale_size_area(breaks=sqrt(area.scale), labels=area.scale, name="Exchanges per day") +
    theme(axis.title=element_blank())
}

