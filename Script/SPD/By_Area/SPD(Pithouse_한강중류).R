library(rcarbon)
library(showtext)
library(sysfonts)
library(here)

#font_add_google('Nanum Gothic', family='NanumGothic')
#showtext_auto()
#par(family="NanumGothic")

# Data Load
data <- read.csv(here("./Data/C14_Master.csv"))
data <- subset(data,subset = Area == "한강중류")
data <- na.omit(data)


# Data Processing
caldates <- calibrate(x=data$BP, errors=data$Error, calCurves="intcal20", normalised=FALSE, verbose=FALSE)
bins <- binPrep(sites=data$Site, ages=data$BP, h=100)

# Make SPD(raw)
spd.raw <- spd(x=caldates, bins=bins, timeRange=c(2200, 1200))

# Make SPD(rollin-mean : 50)
spd.rol <- spd(x=caldates, bins=bins, timeRange=c(2200, 1200), runm=50)

#Plot
tiff(file=here("./Graph/SPD(한강중류).tiff"), units='in', res=300, width=7, height=5)
plot(spd.rol, calendar = "BCAD")
plot(spd.raw, add=TRUE, calendar="BCAD", type="simple", col="black", lwd=1, lty=2)
legend("topleft",legend=c("Observed SPD", "Rolling-Mean : 50"),col=c(1,"grey"),lty=c(2,1),lwd=c(1,5),cex=0.8,bg="white")
dev.off()