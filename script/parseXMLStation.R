##
## Project BikeShare
##

# parse XML-formated data of stations into table-formated csv
library(plyr)
library(XML)
url <- "http://capitalbikeshare.com/data/stations/bikeStations.xml"

bikeStation <- xmlToList(url)
str(bikeStation)
# bikeStation has an extra element at the end (.attr). Removed.
# the 10th element is removalDate which is NULL for a lot the entries. Removed
stationBook <- ldply(bikeStation[-length(bikeStation)], function(x) data.frame(x[-10], stringsAsFactors=F))
str(stationBook) # 315 obs.
write.csv(stationBook, "F:/Google Drive/Data Projects/Project BikeShare/stationBook.csv")
