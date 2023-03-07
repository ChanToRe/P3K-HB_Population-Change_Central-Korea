library(rcarbon)
# P3K : Proto Three Kingdom period of Korea peninsula
# HB : Bakje(Hanseong period)

# Data Load
data <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/C14_Master.csv")
data <- na.omit(data)

# Data Processing
caldates <- calibrate(x=data$BP, errors=data$Error, calCurves="intcal20", normalised=FALSE, verbose=FALSE)
bins <- binPrep(sites=data$Site, ages=data$BP, h=100)

# Make SPD(raw)
spd.raw <- spd(x=caldates, bins=bins, timeRange=c(2200, 1200))

# Make SPD(rollin-mean : 50)
spd.rol <- spd(x=caldates, bins=bins, timeRange=c(2200, 1200), runm=50)

#Plot
tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/SPD(pithouse).png", units='in', res=300, width=7, height=5)
plot(spd.rol, calendar = "BCAD")
plot(spd.raw, add=TRUE, calendar="BCAD", type="simple", col="black", lwd=1, lty=2)
legend("topleft",legend=c("Observed SPD", "Rolling-Mean : 50"),col=c(1,"grey"),lty=c(2,1),lwd=c(1,5),cex=0.8,bg="white")
dev.off()