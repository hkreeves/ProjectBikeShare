table(bs2013$Type)
bs.mem <- subset(bs2013, Type=="Subscriber")
bs.cas <- subset(bs2013, Type=="Casual")

summary(bs2013$Duration.Sec)
boxplot(log10(bs2013$Duration.Sec+1) ~ bs2013$Type)
hist(log10(bs2013$Duration.Sec+1))

prop.table(table(bs2013$Duration.Sec > 1800))
bs.long <- subset(bs2013, Duration.Sec > 1800)
nrow(bs.long)

hist(log(util$Freq),breaks=20)