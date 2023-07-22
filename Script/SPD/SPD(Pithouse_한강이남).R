library(rcarbon)
# P3K : Proto Three Kingdom period of Korea peninsula
# HB : Bakje(Hanseong period)

# Data Load
data_South <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/C14_Master.csv")
data_South <- na.omit(data)
data_South <- data_South[(data_South$권역 == "한강 이남"), ]

# Data Processing
caldates_South <- calibrate(x=data_South$BP, errors=data_South$Error, calCurves="intcal20", normalised=FALSE, verbose=FALSE)
bins_South <- binPrep(sites=data_South$Site, ages=data_South$BP, h=100)

# Make SPD(raw)
spd.raw_South <- spd(x=caldates_South, bins=bins_South, timeRange=c(2200, 1200))

# Make SPD(rollin-mean : 50)
spd.rol_South <- spd(x=caldates_South, bins=bins_South, timeRange=c(2200, 1200), runm=50)

#Plot
tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/SPD(한강 이남).tiff", units='in', res=300, width=7, height=5)
plot(spd.rol_South, calendar = "BCAD")
plot(spd.raw_South, add=TRUE, calendar="BCAD", type="simple", col="black", lwd=1, lty=2)
legend("topleft",legend=c("Observed SPD", "Rolling-Mean : 50"),col=c(1,"grey"),lty=c(2,1),lwd=c(1,5),cex=0.8,bg="white")
dev.off()