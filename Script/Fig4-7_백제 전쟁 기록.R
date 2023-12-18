library(showtext)
library(sysfonts)
library(here)

#font_add_google('Nanum Gothic', family='NanumGothic')
#showtext_auto()
#par(family="NanumGothic")

df <- read.csv(here("./Data/Bakje_WarRecords.csv"))

win <- subset(df, df$`승패` == "승리")
lose <- subset(df, df$'승패' == "패배")
unknown <- subset(df, df$'승패' == "미상")

tiff(file=here("./Graph/Bakje_WarRecords_win.tiff"), units='in', res=300, width=10, height=3)
plot(~win$`날짜`, col = "red", pch = 21, xlim = c(-50, 700))
rug(win$`날짜`, ticksize = 1, side = 1, lwd = 1, col = "red")
title(xlab="BC/AD", col.lab="black")
dev.off()

tiff(file=here("./Graph/Bakje_WarRecords_lose.tiff"), units='in', res=300, width=10, height=3)
plot(~lose$`날짜`, col = "blue", pch = 21, xlim = c(-50, 700))
rug(lose$'날짜', ticksize = 1, side = 1, lwd = 1, col = "blue")
title(xlab="BC/AD", col.lab="black")
dev.off()

tiff(file=here("./Graph/Bakje_WarRecords_unknown.tiff"), units='in', res=300, width=10, height=3)
plot(~unknown$`날짜`, col = "black", pch = 21, xlim = c(-50, 700))
rug(unknown$'날짜', ticksize = 1, side = 1, lwd = 1, col = "black")
title(xlab="BC/AD", col.lab="black")
dev.off()

tiff(file=here("./Graph/Bakje_WarRecords_all.tiff"), units='in', res=300, width=7, height=4)
par(mfrow = c(3, 1))
par(mar=c(2,1,1.5,1.5))
rug1 <- plot(~win$`날짜`, col = "red", pch = 21, xaxt="n", xlim = c(-50, 700)) +
rug(win$`날짜`, ticksize = 1, side = 1, lwd = 1.5, col = "red")
rug2 <- plot(~lose$`날짜`, col = "blue", pch = 21, xaxt="n", xlim = c(-50, 700)) +
rug(lose$'날짜', ticksize = 1, side = 1, lwd = 1.5, col = "blue")
rug3 <- plot(~unknown$`날짜`, col = "black", pch = 21, xaxt="n", xlim = c(-50, 700)) +
rug(unknown$'날짜', ticksize = 1, side = 1, lwd = 1.5, col = "black")
axis(1,cex.axis=1.5)
title(xlab="BC/AD", col.lab="black")
dev.off()
