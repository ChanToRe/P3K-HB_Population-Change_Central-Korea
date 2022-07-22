library(rcarbon)

#Data Load
data <- read.csv("/Users/jch/Desktop/P3K~Bakje(Hanseong) central region demography/Code/Data/Korea_CR_14C.csv")
MC_data<- subset(data, BP>1500)

#Data Processing
MC_caldates <- calibrate(x=MC_data$BP, errors=MC_data$Error, calCurves="intcal20", normalised=FALSE, verbose=FALSE)
MC_bins <- binPrep(sites=MC_data$Site, ages=MC_data$BP, h=100)

# Make SPD
spd.raw <- spd(x=MC_caldates, bins=MC_bins, timeRange=c(2200, 1500))

# Model Test(Exponential / 1000 simulations)
set.seed(12345)
uniMod <- modelTest(x=MC_caldates, bins=MC_bins, errors=MC_data$Error, timeRange=c(2200, 1500), model="uniform", nsim=1000, runm=50)
plot(uniMod, calendar="BCAD", lwd=2)
plot(spd.raw, add=TRUE, calendar="BCAD", type="simple", col="Navy", lwd=1, lty=2)
title("Model Test(Uniform)")
legend("topleft",legend=c("Rolling-Mean : 50","Observed SPD","CI","Positive Deviation","Negative Deviation"),col=c(1,1,"lightgrey","indianred","royalblue"),lty=c(1,2,1,1,1),lwd=c(2,1,5,5,5),cex=0.8,bg="white")

plot(uniMod, calendar="BCAD", type='roc') +
  title("Local Growth Rates Test(Uniform)")

summary(uniMod)