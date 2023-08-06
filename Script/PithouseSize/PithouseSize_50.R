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
step1_df <- subset(df, subset = BCAD >= -200 & BCAD < -150)
step2_df <- subset(df, subset = BCAD >= -150 & BCAD < -100)
step3_df <- subset(df, subset = BCAD >=-100 & BCAD < -50)
step4_df <- subset(df, subset = BCAD >= -50 & BCAD < 0)
step5_df <- subset(df, subset = BCAD >= 0 & BCAD < 50)
step6_df <- subset(df, subset = BCAD >= 50 & BCAD < 100)
step7_df <- subset(df, subset = BCAD >= 100 & BCAD < 150)
step8_df <- subset(df, subset = BCAD >= 150 & BCAD < 200)
step9_df <- subset(df, subset = BCAD >= 200 & BCAD < 250)
step10_df <- subset(df, subset = BCAD >= 250 & BCAD < 300)
step11_df <- subset(df, subset = BCAD >= 300 & BCAD < 350)
step12_df <- subset(df, subset = BCAD >= 350 & BCAD < 400)
step13_df <- subset(df, subset = BCAD >= 400 & BCAD < 450)
step14_df <- subset(df, subset = BCAD >= 450 & BCAD < 500)
step15_df <- subset(df, subset = BCAD >= 500 & BCAD < 550)
step16_df <- subset(df, subset = BCAD >= 550 & BCAD < 600)
step17_df <- subset(df, subset = BCAD >= 600 & BCAD < 650)
step18_df <- subset(df, subset = BCAD >= 650 & BCAD < 700)
step19_df <- subset(df, subset = BCAD >= 700 & BCAD < 750)
step20_df <- subset(df, subset = BCAD >= 750 & BCAD < 800)

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
step11_size <- sum(as.numeric(step11_df$pop))/35000
step12_size <- sum(as.numeric(step12_df$pop))/35000
step13_size <- sum(as.numeric(step13_df$pop))/35000
step14_size <- sum(as.numeric(step14_df$pop))/35000
step15_size <- sum(as.numeric(step15_df$pop))/35000
step16_size <- sum(as.numeric(step16_df$pop))/35000
step17_size <- sum(as.numeric(step17_df$pop))/35000
step18_size <- sum(as.numeric(step18_df$pop))/35000
step19_size <- sum(as.numeric(step19_df$pop))/35000
step20_size <- sum(as.numeric(step20_df$pop))/35000

barsize <- c(step1_size, step2_size, step3_size, step4_size, step5_size, step6_size, step7_size, step8_size, step9_size, step10_size,
             step11_size, step12_size, step13_size, step14_size, step15_size, step16_size, step17_size, step18_size, step19_size, step20_size)
barname <- c("-2C", "", "-1C", "", "1C", "", "2C", "", "3C", "",
             "4C", "", "5C", "", "6C", "", "7C", "", "8C", "")
bardata <- data.frame(barname, barsize)

tiff(file=here("./Graph/Demographics(Pithouse Size 50).tiff"), units='in', res=300, width=7, height=5)
barplot(bardata$barsize, space=0, names.arg = bardata$barname)
title(xlab="Phase", col.lab="black")
title(ylab="Number of Population", col.lab="black")
dev.off()