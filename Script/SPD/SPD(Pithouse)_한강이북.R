library(rcarbon)
# P3K : Proto Three Kingdom period of Korea peninsula
# HB : Bakje(Hanseong period)

#한강이북
data_North <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/C14_Master.csv")
data_North <- na.omit(data)
data_North <- data_North[(data_North$권역 == "한강 이북"), ]

# Data Processing
caldates_North <- calibrate(x=data_North$BP, errors=data_North$Error, calCurves="intcal20", normalised=FALSE, verbose=FALSE)
bins_North <- binPrep(sites=data_North$Site, ages=data_North$BP, h=100)

# Make SPD(raw)
spd.raw_North <- spd(x=caldates_North, bins=bins_North, timeRange=c(2200, 1200))

# Make SPD(rollin-mean : 50)
spd.rol_North <- spd(x=caldates_North, bins=bins_North, timeRange=c(2200, 1200), runm=50)

#Plot
tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/SPD(한강 이북).tiff", units='in', res=300, width=7, height=5)
plot(spd.rol_North, calendar = "BCAD")
plot(spd.raw_North, add=TRUE, calendar="BCAD", type="simple", col="black", lwd=1, lty=2)
legend("topleft",legend=c("Observed SPD", "Rolling-Mean : 50"),col=c(1,"grey"),lty=c(2,1),lwd=c(1,5),cex=0.8,bg="white")
dev.off()