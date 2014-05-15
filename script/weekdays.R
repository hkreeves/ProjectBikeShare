#create weekday
bs.wd <- subset(bs2013, weekdays(bs2013$Start.Date) %in% c("Monday", "Tuesday", "Wednesday","Thursday","Friday"))

#create weekend
bs.wn <- subset(bs2013, weekdays(bs2013$Start.Date) %in% c("Saturday","Sunday"))

all.date.in.bs2013 <- unique(as.Date(bs2013$Start.Date))


head(all.date.in.bs0)

table(weekdays(all.date.in.bs2013) %in% c("Monday", "Tuesday", "Wednesday","Thursday","Friday"))

table(weekdays(all.date.in.bs0) %in% c("Saturday", "Sunday"))
nWn <- 104
nWd <- 261

nWn <- getNDay(bs.wn)

