library(rcarbon)
library(here)
library(showtext)
library(sysfonts)
library(here)

#font_add_google('Nanum Gothic', family='NanumGothic')
#showtext_auto()
#par(family="NanumGothic")

df <- read.csv(here("./Data/C14_Master.csv"))

df <- df[!(df$Size.cm2. == "X" ), ]
df <- df[!(df$Size.cm2. == "" ), ]

#결측치 제거
df <- na.omit(df)

df[, "pop"] <- as.numeric(df$Size.cm2.)
df$BCAD <- BPtoBCAD(df$BP)

#데이터 분할
step1_df <- subset(df, subset = BCAD >= -200 & BCAD < -100)
step2_df <- subset(df, subset = BCAD >= -100 & BCAD < 0)
step3_df <- subset(df, subset = BCAD >= 0 & BCAD < 100)
step4_df <- subset(df, subset = BCAD >= 100 & BCAD < 200)
step5_df <- subset(df, subset = BCAD >= 200 & BCAD < 300)
step6_df <- subset(df, subset = BCAD >= 300 & BCAD < 400)
step7_df <- subset(df, subset = BCAD >= 400 & BCAD < 500)
step8_df <- subset(df, subset = BCAD >= 500 & BCAD < 600)
step9_df <- subset(df, subset = BCAD >= 600 & BCAD < 700)
step10_df <- subset(df, subset = BCAD >= 700 & BCAD < 800)

step1_size <- sum(as.numeric(step1_df$pop))/35000
step2_size <- sum(as.numeric(step2_df$pop))/35000
step3_size <- sum(as.numeric(step3_df$pop))/35000
step4_size <- sum(as.numeric(step4_df$pop))/35000
step5_size <- sum(as.numeric(step5_df$pop))/35000
step6_size <- sum(as.numeric(step6_df$pop))/35000
step7_size <- sum(as.numeric(step7_df$pop))/35000
step8_size <- sum(as.numeric(step8_df$pop))/35000
step9_size <- sum(as.numeric(step9_df$pop))/35000
step10_size <- sum(as.numeric(step10_df$pop))/35000

barsize <- c(step1_size, step2_size, step3_size, step4_size, step5_size, step6_size, step7_size, step8_size, step9_size, step10_size)
barname <- c("-2C", "-1C", "1C", "2C", "3C", "4C", "5C", "6C", "7C", "8C")
bardata <- data.frame(barname, barsize)

tiff(file=here("./Graph/Notused/Demographics(Pithouse Size - 100y).tiff"), units='in', res=300, width=7, height=5)
barplot(bardata$barsize, space=0, names.arg = bardata$barname)
title(xlab="Phase", col.lab="black")
title(ylab="Number of Population", col.lab="black")
dev.off()