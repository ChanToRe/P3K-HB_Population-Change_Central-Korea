df <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/Pithouse_Master.csv")
df <- na.omit(df)

P3K1 <- sum(df$P3K.1)
P3K2 <- sum(df$P3K.2)
P3K3 <- sum(df$P3K.3)
HB1 <- sum(df$HB.1)
HB2 <- sum(df$HB.2)

barsize <- c(P3K1, P3K2, P3K3, HB1, HB2)
barname <- c("P3K-1", "P3K-2", "P3K-3", "HB-1", "HB-2")
bardata <- data.frame(barname, barsize)

tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/Pithouse Num(Han&Kang 2021 Fix).tiff", units='in', res=300, width=7, height=5)
barplot(bardata$barsize, space=0, names.arg = bardata$barname, width=c(1, 1, 1, 0.7, 2))
title(xlab="Phase", col.lab="black")
title(ylab="Number of Pithouse", col.lab="black")
dev.off()
