##
## Project BikeShare
##
## Map plot
##
## April 6th, 2014
##

library(ggmap)

# data passing in
str(small.station)

mar <- 0.01
map.boundary <- c(min(small.station$long)-mar, min(small.station$lat)-mar,
			max(small.station$long)+mar, max(small.station$lat)+mar)
# get a DC map
dc.map <- get_map(location=map.boundary, zoom=12, maptype="roadmap", 
color="bw", source="google")

#dc.map <- get_map("Washington, DC", zoom=13, maptype="watercolor", source="stamen")

ggmap(dc.map)

# cluster plot
color <- c("grey32", brewer.pal(9, "Set1"))
scale_colour_brewer(palette="Set3")

# plot on map
# color coding indicates cluster
# area of point indicates utilization (per day)
# area scale
summary(pop.station$Freq)
area.scale <- c(10, 50, 100, 200)
station.map <- 
  ggmap(dc.map) +
  geom_point(
    aes(x=long, y=lat, col=factor(cluster1$cluster), size=sqrt(pop.station$Freq + 1)),
    data=small.station, alpha=0.5) +
  scale_colour_manual(values=color, name="Community", labels=c("Unclustered", "Comm 1", "Comm 2", "Comm 3")) +
  scale_size_area(breaks=sqrt(area.scale), labels=area.scale, name="Exchanges per day")
  
station.map


