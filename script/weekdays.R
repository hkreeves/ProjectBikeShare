#create weekday
bs.wd <- subset(bs0, weekdays(bs0$Start.Date) %in% c("Monday", "Tuesday", "Wednesday","Thursday","Friday"))

#create weekend
bs.wn <- subset(bs0, weekdays(bs0$Start.Date) %in% c("Saturday","Sunday"))

all.date.in.bs0 <- unique(as.Date(bs0$Start.Date))

head(all.date.in.bs0)

table(weekdays(all.date.in.bs0) %in% c("Saturday", "Sunday"))
nWn <- 104
nWd <- 261
