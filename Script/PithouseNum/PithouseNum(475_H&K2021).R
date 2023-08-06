library(showtext)
library(sysfonts)
library(here)

#font_add_google('Nanum Gothic', family='NanumGothic')
#showtext_auto()
#par(family="NanumGothic")

df <- read.csv(here("./Data/Pithouse_Master.csv"))
df <- na.omit(df)

P3K1 <- sum(df$`원삼국1`)
P3K2 <- sum(df$`원삼국2`)
P3K3 <- sum(df$`원삼국3`)
HB1 <- sum(df$`한성백제1`)
HB2 <- sum(df$`한성백제2`)
HB3 <- sum(df$`한성백제3`)
HB4 <- sum(df$`한성백제4`)
HB1to2 <- sum(HB1 + HB2)
HB3to4 <- sum(HB3 + HB4)

barsize <- c(P3K1, P3K2, P3K3, HB1to2, HB3to4)
barname <- c("원삼국1", "원삼국2", "원삼국3", "한성1~2", "한성3~4")
bardata <- data.frame(barname, barsize)

tiff(file=here("./Graph/Pithouse Num(Han&Kang2021).tiff"), units='in', res=300, width=7, height=5)
barplot(bardata$barsize, space=0, names.arg = bardata$barname, width=c(1, 1, 1, 1.2, 1.1)) +
  title(main="하한 475년안(한지선&강동석 2021)",
        xlab="Phase",
        ylab="Number of Pithouse",
        col.lab="black")
dev.off()