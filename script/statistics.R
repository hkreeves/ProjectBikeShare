# Add new features: Day, Month, Long.Trip
bs2013$Day <- weekdays(bs2013$Start.Date)
bs2013$Month <- months(bs2013$Start.Date)
bs2013$Quarter <- quarters(bs2013$Start.Date)
bs2013$Long.Trip <- bs2013$Duration.Sec > 1860

day.lvl <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mon.lvl <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
qtr.lvl <- c("Q1", "Q2", "Q3", "Q4")

bs2013$Day <- factor(bs2013$Day, ordered=T, levels=day.lvl)
bs2013$Month <- factor(bs2013$Month, ordered=T, levels=mon.lvl)
bs2013$Quarter <- factor(bs2013$Quarter, ordered=T, levels=qtr.lvl)
bs2013$Type <- as.factor(bs2013$Type)
summary(bs2013)

# number of stations for each month
dt2013 <- as.data.table(bs2013[-c(2,4)])
#station.month.count <- dt2013[, length(unique(c(Start.Terminal, End.Terminal)), by = Month]
station.month.count <- tapply(c(bs2013$Start.Terminal, bs2013$End.Terminal), rep(bs2013$Month, 2), function(x) length(unique(x)))
station.month.count <- data.frame(list(Month=as.ordered(mon.lvl), Count=station.month.count))

# Month-Day heat map
month.day.count <- dt2013[,.N, by=c("Month", "Day")]
month.day.count <- month.day.count[order(Month, Day)]
month.day.count$Count.Per.Station <- month.day.count$N / rep(station.month.count$Count, each=7)

heatmap <- ggplot(month.day.count, aes(x=Day, y=Month, fill=Count.Per.Station)) + 
  geom_tile() + 
  scale_fill_continuous(name="Rides per station", low="white", high="royalblue4")
print(heatmap)

ggplot(month.day.count, aes(x=Day, y=Count.Per.Station, group=Month, color=Month)) + geom_line(size=2, alpha=0.6)

# Day-Type
day.type.count <- dt2013[,.N, by=c("Quarter", "Day", "Type")]
day.type.plot <- ggplot(day.type.count, aes(x=Day, y=N, group=Type, color=Type)) + 
  geom_line(size=2, alpha=0.6) + 
  facet_grid(. ~ Quarter) +
  ylab("Number of rides") +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=10, angle=90))
print(day.type.plot)


# Long.Trip-Type
long.type.count <- dt2013[,.N, by=c("Type", "Long.Trip")]
long.type.count
long.type.plot <- ggplot(long.type.count, aes(x=Type, y=N, fill=Long.Trip)) +
  geom_bar(stat="identity", position="stack", width=0.8) +
  scale_fill_brewer(name="Ride more than 30min", labels=c("No", "Yes"), palette="Set2") +
  ylab("Number of rides")
print(long.type.plot)

# histograms
bs.graph <- createGraph(bs2013)
res <- createDistanceMatrix(bs.graph)
util <- res$popStation
ggplot(util, aes(Freq)) + geom_bar(fill="orange", color="white", binwidth=10) +
  xlab("Ride exchanges per week") +
  ylab("Number of stations")

dur.hist <-ggplot(bs2013, aes(x=Duration.Sec, group=Type, fill=Type)) 
dur.hist + geom_bar(binwidth=5, color="grey32") + facet_grid(Type ~ .) + scale_x_continuous(limits=c(0,120))

ggplot(util, aes(x=In, y=Out)) + geom_point()