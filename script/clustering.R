##
## Project BikeShare
##
## Exploration in clustering
##
## April 6th, 2014
##

library(fpc)
library(reshape2)
library(RColorBrewer)

# load data
load("./temp.Rda")

# define the date range of data to perform clustering analysis
beginDate <- as.Date("2012-01-01")
endDate <- as.Date("2012-03-31")
# the length of the period in unit of day or week
nDay <- as.numeric(endDate - beginDate + 1)
nWeek <- nDay/7

# create a subset for the date range specified
jan05 <- subset(bs, (as.Date(Start.date) >= beginDate) & (as.Date(Start.date) <= endDate))
str(jan05)

# create path IDs formated as "StartTerminal-EndTerminal" and the opposite
jan05$S2E <- paste(jan05$Start.Term, jan05$End.Term, sep="-")
jan05$E2S <- paste(jan05$End.Term, jan05$Start.Term, sep="-")

# find the frequency of each path ID
link1.count <- table(jan05$S2E)
link1 <- as.data.frame.table(link1.count)
link2.count <- table(jan05$E2S)
link2 <- as.data.frame.table(link2.count)

# merge frequencies between "A-B" and "B-A"
link.count <- merge(link1, link2, by="Var1", all=T)
link.count[is.na(link.count)] <- 0

link.count$total <- link.count$Freq.x + link.count$Freq.y

# full list of all possible path IDs
all.term <- sort(unique(bs$Start.Term))
all.pair <- cbind(rep(all.term, each=length(all.term)), rep(all.term, length(all.term)))
full.link <- data.frame(list(Var1=paste(all.pair[,1], all.pair[,2], sep="-"), Freq=rep(0, nrow(all.pair))))

# a check of terminals not appearing in stationBook (maybe out of service)
all.term[!(all.term %in% stationBook$terminalName)] # one station, 31210, White House (17th & State Pl NW)
### CAUTION: decided to fill 31210 into stationBook instead of omitting it from our data.

# merge link.count to full.link
full.link <- merge(full.link, link.count, by="Var1", all.x=T)
full.link[is.na(full.link)] <- 0
full.link$Freq <- full.link$Freq.x + full.link$Freq.y
full.link <- full.link[,1:2]

# split the pathID back to Start, End
# compute the "distance" using the following formula: dist = 1/(Freq/nWeek + 1)
triples <- data.frame(cbind(all.pair, 1/(full.link$Freq/nWeek + 1)))
names(triples) <- c("Term1", "Term2", "Dist")
# inspect the values of "distance"
#table(triples$Dist)

# reshape the long data to wide
dist.mtrx <- reshape(triples, timevar="Term1", idvar="Term2", direction="wide")

################################ clustering #####################################
# dbscan clustering
# eps is the "distance" threshold. Chosen as 0.035, or 27 exchanges per week. 
# MinPts is the minimum number of neighbors. Chosen as 3. 
cluster1 <- dbscan(as.matrix(dist.mtrx[,-1]), method="dist", eps=0.035, MinPts=3)
# check clusters
table(cluster1$cluster)

# plot
small.station <- stationBook[stationBook$term %in% all.term,]
small.station <- small.station[order(small.station$term),]
small.station$long <- as.numeric(small.station$long)
small.station$lat <- as.numeric(small.station$lat)

color <- brewer.pal(9, "Set1")
plot(small.station$long, small.station$lat, 
  type="p", pch=19, 
  col=color[cluster1$cluster + 1])

############################### popularity analysis #############################
# histogram of pathIDs
freq.table <- hist(full.link$Freq/nDay, breaks=seq(0,30, by=1))
freq.table$counts <- log10(freq.table$counts/2 +1 ) # divided by 2 because "A-B" and "B-A" are counted as two pathIDs
plot(freq.table, col="royalblue", border="white",
ylab="10-based log of number of pathIDs", xlab="Average exchanges per day",
main="Histogram of pathIDs")

# popular paths
pop.path <- full.link[order(full.link$Freq, decreasing=T), ] # for 2012-01 to 2012-02, the most pop path is 31613-31619 
pop.path$Freq <- pop.path$Freq/nDay				      # and 31104-31106

# popular stations
pop.base <- data.frame(list(Var1=all.term, Freq=0))
pop.start <- as.data.frame.table(table(jan05$Start.Term)/nDay)
pop.start <- merge(pop.base, pop.start, by="Var1", all.x=T)
pop.start[is.na(pop.start)] <- 0
pop.start$Freq <- pop.start$Freq.x + pop.start$Freq.y
pop.start <- pop.start[, c("Var1", "Freq")]

pop.end <- as.data.frame.table(table(jan05$End.Term)/nDay)
pop.end <- merge(pop.base, pop.end, by="Var1", all.x=T)
pop.end[is.na(pop.end)] <- 0
pop.end$Freq <- pop.end$Freq.x + pop.end$Freq.y
pop.end <- pop.end[, c("Var1", "Freq")]

pop.station <- pop.start
pop.station$Freq <- pop.start$Freq + pop.end$Freq
#pop.station <- pop.station[order(pop.station$Freq, decreasing=T),]