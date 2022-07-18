library(rcarbon)

#Data Load
data <- read.csv("/Users/jch/Desktop/P3K~Bakje(Hanseong) central region demography/Code/Data/Korea_CR_14C.csv")

#Data Processing
caldates <- calibrate(x=data$BP, errors=data$Error, calCurves="intcal20", normalised=FALSE, verbose=FALSE)
bins <- binPrep(sites=data$Site, ages=data$BP, h=100)

# Make SPD(raw, rollin-mean : 50)
spd.raw <- spd(x=caldates, bins, timeRange =c(2200, 1200))
plot(spd.raw, lwd=2) +
  title("SPD(Raw)")

spd.rol <- spd(x=caldates, bins=bins, timeRange=c(2200, 1200), runm=50)
plot(spd.rol, lwd=2) +
  title("SPD(rolling-mean : 50)")