library(rcarbon)

#Data Load
data <- read.csv("/Users/jch/Desktop/P3K~Bakje(Hanseong) central region demography/Code/Data/Korea_CR_14C.csv")
MC_data<- subset(data, BP>1500)

#Data Processing
MC_caldates <- calibrate(x=MC_data$BP, errors=MC_data$Error, calCurves="intcal20", normalised=FALSE, verbose=FALSE)
MC_bins <- binPrep(sites=MC_data$Site, ages=MC_data$BP, h=100)

# Model Test(Uniform / 1000 simulations)
set.seed(12345)
uniMod <- modelTest(x=MC_caldates, bins=MC_bins, errors=MC_data$Error, timeRange=c(2200, 1500), model="uniform", nsim=1000, runm=50)
plot(uniMod, lwd=2) +
  title("Model Test(Uniform)")

plot(uniMod, type='roc') +
  title("Local Growth Rates Test(Uniform)")

summary(uniMod)