library(rcarbon)
library(nimbleCarbon)
library(truncnorm)
library(coda)
library(here)
library(latex2exp)
library(dplyr)

# Load Data
data <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/C14_Master.csv")

# Calibrate Data
obs.caldates <- calibrate(data$BP, data$Error, calCurves='intcal20')
index <- which.CalDates(obs.caldates, BP<2200&BP>1200, p=0.5)
obs.caldates <- obs.caldates[index]

# relevant CRA & CRA-Errors
CRA <- data$BP[index]
Errors <- data$Error[index]
SiteID <- data$Name[index]

# Extract Median Calibrated Dates
medDates <- medCal(obs.caldates)
obs.data <- data.frame(CRA=CRA, Error=Errors, MedCalDate=medDates, SiteID=SiteID)

##### Exponential model #####
exp <- nimbleCode({
  for (i in 1:N){
    theta[i] ~ dExponentialGrowth(a=2200, b=1200, r=r);
    mu[i] <- interpLin(z=theta[i], x=calBP[], y=C14BP[]);
    sigmaCurve[i] <- interpLin(z=theta[i], x=calBP[], y=C14err[]);
    sd[i] <- (sigma[i]^2+sigmaCurve[i]^2)^(1/2);
    X[i] ~ dnorm(mean=mu[i], sd=sd[i]);
  }
  r ~ dnorm(0, sd=0.0004);
})

data(intcal20)
constants <- list(N=length(obs.caldates), calBP=intcal20$CalBP, C14BP=intcal20$C14Age, C14err=intcal20$C14Age.sigma)
data <- list(X=obs.data$CRA, sigma=obs.data$Error)

initsFunction.exp <- function() list(r=rnorm(1, sd=0.0004), theta=as.numeric(obs.data$MedCalDate))

mcmc.exp.samples <- nimbleMCMC(code = exp,constants = constants,data = data,niter = 10000, nchains = 3, thin=6, nburnin = 1000, progressBar = FALSE, monitors=c('r','theta'), initsFunction.exp, samplesAsCodaMCMC=TRUE,setSeed=c(4, 5, 6))

gelman.diag(mcmc.exp.samples$samples)$psrf[1:2,]

tiff(file="/Users/jch/Desktop/exp_r.tiff", units='in', res=300, width=7, height=7)
par(mfrow=c(1,1))
postHPDplot(mcmc.exp.samples$samples$chain1[, 'r'], xlab='', ylab='', show.hpd.val=FALSE)
title(main="Exp-model(Pithouse):r", cex=1.2)
dev.off()

rhat.exp = gelman.diag(mcmc.exp.samples$samples)
ess.exp = effectiveSize(mcmc.exp.samples$samples)
params.exp = list(r = c(mcmc.exp.samples$samples$chain1[,'r'],mcmc.exp.samples$samples$chain2[,'r'],mcmc.exp.samples$samples$chain3[,'r']))

pp.check.exp.cal=postPredSPD(obs.data$CRA,obs.data$Error,calCurve = 'intcal20',model = dExponentialGrowth,a = 3400,b=1850,params=params.m1,nsim = 500,ncores = 5,verbose=FALSE,method='calsample')

tiff(file="/Users/jch/Desktop/exp_model.tiff", units='in', res=300, width=7, height=7)
par(mfrow=c(1, 1))
plot(pp.check.exp.cal, interval = 0.95,calendar='BCAD')
legend('topleft',legend=c('95% Prediction Interval','Positive Deviation','Negative Deviation','Observed SPD'),lwd=c(5,5,5,2),col=c('lightgrey','red','blue','black'),bty='n', cex=c(1, 1, 1, 1))
dev.off()

tiff(file="/Users/jch/Desktop/exp_fit.tiff", units='in', res=300, width=7, height=7)
par(mfrow=c(1, 1))
postHPDplot(postPredCor(pp.check.double_exp.cal), xlab="Pearson's Correlation coefficient",ylab='Density',xlim=c(0,1))
dev.off()

compare.models(mcmc.double_exp.samples, mcmc.exp.samples)