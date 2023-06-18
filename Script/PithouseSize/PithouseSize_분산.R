library(rcarbon)

df <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/C14_Master.csv")

df <- df[!(df$Size.cm2. == "X" ), ]
df <- df[!(df$Size.cm2. == "" ), ]
df <- na.omit(df)
df_1 <- subset(df, df$Type == "백제(방형)" | df$Type == "신라(방형)" | df$Type == "고구려(방형)")
df_2 <- subset(df, df$Type == "백제(呂·凸자형)" | df$Type == "신라(呂·凸자형)")

df_1[, "pop"] <- as.numeric(df_1$Size.cm2.)
df_1$BCAD <- BPtoBCAD(df_1$BP)
df_2[, "pop"] <- as.numeric(df_2$Size.cm2.)
df_2$BCAD <- BPtoBCAD(df_2$BP)

#데이터 분할 방형
step1_df_1 <- subset(df_1, subset = BCAD >= -200 & BCAD < -100)
step2_df_1 <- subset(df_1, subset = BCAD >= -100 & BCAD < 0)
step3_df_1 <- subset(df_1, subset = BCAD >= 0 & BCAD < 100)
step4_df_1 <- subset(df_1, subset = BCAD >= 100 & BCAD < 200)
step5_df_1 <- subset(df_1, subset = BCAD >= 200 & BCAD < 300)
step6_df_1 <- subset(df_1, subset = BCAD >= 300 & BCAD < 400)
step7_df_1 <- subset(df_1, subset = BCAD >= 400 & BCAD < 500)
step8_df_1 <- subset(df_1, subset = BCAD >= 500 & BCAD < 600)
step9_df_1 <- subset(df_1, subset = BCAD >= 600 & BCAD < 700)
step10_df_1 <- subset(df_1, subset = BCAD >= 700 & BCAD < 800)

step1_size_1 <- sum(as.numeric(step1_df_1$pop))/35000
step2_size_1 <- sum(as.numeric(step2_df_1$pop))/35000
step3_size_1 <- sum(as.numeric(step3_df_1$pop))/35000
step4_size_1 <- sum(as.numeric(step4_df_1$pop))/35000
step5_size_1 <- sum(as.numeric(step5_df_1$pop))/35000
step6_size_1 <- sum(as.numeric(step6_df_1$pop))/35000
step7_size_1 <- sum(as.numeric(step7_df_1$pop))/35000
step8_size_1 <- sum(as.numeric(step8_df_1$pop))/35000
step9_size_1 <- sum(as.numeric(step9_df_1$pop))/35000
step10_size_1 <- sum(as.numeric(step10_df_1$pop))/35000

step1_box_1 <- as.numeric(step1_df_1$Size.cm2.)
step2_box_1 <- as.numeric(step2_df_1$Size.cm2.)
step3_box_1 <- as.numeric(step3_df_1$Size.cm2.)
step4_box_1 <- as.numeric(step4_df_1$Size.cm2.)
step5_box_1 <- as.numeric(step5_df_1$Size.cm2.)
step6_box_1 <- as.numeric(step6_df_1$Size.cm2.)
step7_box_1 <- as.numeric(step7_df_1$Size.cm2.)
step8_box_1 <- as.numeric(step8_df_1$Size.cm2.)
step9_box_1 <- as.numeric(step9_df_1$Size.cm2.)
step10_box_1 <- as.numeric(step10_df_1$Size.cm2.)

tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/Boxplot(Pithouse Size - 방형).tiff", units='in', res=300, width=7, height=4)
boxplot(step1_box_1, step2_box_1, step3_box_1, step4_box_1, step5_box_1, step6_box_1, step7_box_1, step8_box_1, step9_box_1, step10_box_1)
title(xlab="Phase", col.lab="black")
title(ylab="Size", col.lab="black")
dev.off()

#여철자형
step1_df_2 <- subset(df_2, subset = BCAD >= -200 & BCAD < -100)
step2_df_2 <- subset(df_2, subset = BCAD >= -100 & BCAD < 0)
step3_df_2 <- subset(df_2, subset = BCAD >= 0 & BCAD < 100)
step4_df_2 <- subset(df_2, subset = BCAD >= 100 & BCAD < 200)
step5_df_2 <- subset(df_2, subset = BCAD >= 200 & BCAD < 300)
step6_df_2 <- subset(df_2, subset = BCAD >= 300 & BCAD < 400)
step7_df_2 <- subset(df_2, subset = BCAD >= 400 & BCAD < 500)
step8_df_2 <- subset(df_2, subset = BCAD >= 500 & BCAD < 600)
step9_df_2 <- subset(df_2, subset = BCAD >= 600 & BCAD < 700)
step10_df_2 <- subset(df_2, subset = BCAD >= 700 & BCAD < 800)

step1_size_2 <- sum(as.numeric(step1_df_2$pop))/35000
step2_size_2 <- sum(as.numeric(step2_df_2$pop))/35000
step3_size_2 <- sum(as.numeric(step3_df_2$pop))/35000
step4_size_2 <- sum(as.numeric(step4_df_2$pop))/35000
step5_size_2 <- sum(as.numeric(step5_df_2$pop))/35000
step6_size_2 <- sum(as.numeric(step6_df_2$pop))/35000
step7_size_2 <- sum(as.numeric(step7_df_2$pop))/35000
step8_size_2 <- sum(as.numeric(step8_df_2$pop))/35000
step9_size_2 <- sum(as.numeric(step9_df_2$pop))/35000
step10_size_2 <- sum(as.numeric(step10_df_2$pop))/35000

step1_box_2 <- as.numeric(step1_df_2$Size.cm2.)
step2_box_2 <- as.numeric(step2_df_2$Size.cm2.)
step3_box_2 <- as.numeric(step3_df_2$Size.cm2.)
step4_box_2 <- as.numeric(step4_df_2$Size.cm2.)
step5_box_2 <- as.numeric(step5_df_2$Size.cm2.)
step6_box_2 <- as.numeric(step6_df_2$Size.cm2.)
step7_box_2 <- as.numeric(step7_df_2$Size.cm2.)
step8_box_2 <- as.numeric(step8_df_2$Size.cm2.)
step9_box_2 <- as.numeric(step9_df_2$Size.cm2.)
step10_box_2 <- as.numeric(step10_df_2$Size.cm2.)

tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/Boxplot(Pithouse Size - 여철자형).tiff", units='in', res=300, width=7, height=4)
boxplot(step1_box_2, step2_box_2, step3_box_2, step4_box_2, step5_box_2, step6_box_2, step7_box_2, step8_box_2, step9_box_2, step10_box_2)
title(xlab="Phase", col.lab="black")
title(ylab="Size", col.lab="black")
dev.off()