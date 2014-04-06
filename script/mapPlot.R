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

color <- c("grey", brewer.pal(9, "Set1"))
station.map <- 
  ggmap(dc.map) +
  geom_point(aes(x=long, y=lat, size=1, col=color[cluster1$cluster + 1]), data=small.station, alpha=0.7)
station.map

