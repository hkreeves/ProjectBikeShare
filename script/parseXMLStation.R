##
## Project BikeShare
##

# parse XML-formated data of stations into table-formated csv
library(plyr)
library(XML)
url <- "http://capitalbikeshare.com/data/stations/bikeStations.xml"

bikeStation <- xmlToList(url)
str(bikeStation)
# bikeStation has an extra row at the end (.attr). Removed.
# the 10th variable is removalDate which is NULL for a lot the entries. Removed.
stationBook <- ldply(bikeStation[-length(bikeStation)], function(x) data.frame(x[-10], stringsAsFactors=F))
str(stationBook) # 315 obs.

### CAUTION: due to the missing information of out-of-service stations, We decide to fill them in manually
# missing station in 2012Q1: 31210, White House (17th & State Pl NW)
# 38.896432, -77.039510
stationBook <- rbind(stationBook,
data.frame(list(.id="station", id="", name="White House", terminalName=31210, lastCommWithServer="", 
lat="38.896432", long="-77.039510", installed="", locked="", installDate="", temporary="",
public="", nbBikes="", nbEmptyDocks="", latestUpdateTime="")))




# type conversion
stationBook$lat <- as.numeric(stationBook$lat)
stationBook$long <- as.numeric(stationBook$long)

write.csv(stationBook, "F:/Google Drive/Data Projects/ProjectBikeShare/stationBook.csv")

data.date <- date()
save(stationBook, data.date, file="F:/Google Drive/Data Projects/ProjectBikeShare/stationInfo.Rda")
