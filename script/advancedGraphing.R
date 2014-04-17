##
## Project BikeShare
##
## Advanced Graphing 
##
## April 16th, 2014
##

bs.graph <- createGraph(bs2012, "2012-03-01", "2012-06-30")
summary(bs.graph)
station <- merge(bs.graph, stationBook[c("terminalName", "lat", "long")], 
  by.x="Term1", by.y="terminalName", all=F)
station <- merge(station, stationBook[c("terminalName", "lat", "long")], 
  by.x="Term2", by.y="terminalName", suffixes = c("", ".b"), all=F)

# plot onto a map
dc.map <- get_map(location="Washinton, DC", zoom=13, maptype="roadmap", 
color="bw", source="google")

bs.plot <- plotCluster(clust, util)
print(bs.plot2)

# try to add lines
bs.plot2 <-  bs.plot +
  geom_segment(aes(x = long, y = lat, xend = long.b, yend = lat.b, color=factor(0, levels=levels(factor(clust$Cluster))), size=Freq/40), 
    alpha=0.4, lineend="round", data=out)
+
  scale_size_continuous(range = c(1, 6))