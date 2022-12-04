library(rcarbon)
library(nimbleCarbon)
library(truncnorm)
library(coda)
library(here)
library(latex2exp)
library(dplyr)

# Load Data
data <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/C14_Grain_Master.csv")

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

# Define Model
modelPlot(model=dDoubleExponentialGrowth, a=2200, b=1200, params=list(r1=rnorm(1000, sd=0.0004), r2=rnorm(1000, sd=0.0004), mu=round(runif(1000,1200,2200))))

# Average growth rate
double_exp <- nimbleCode({
  for (i in 1:N){
    theta[i] ~ dDoubleExponentialGrowth(a=2200, b=1200, r1=r1, r2=r2, mu=changept);
    mu[i] <- interpLin(z=theta[i], x=calBP[], y=C14BP[]);
    sigmaCurve[i] <- interpLin(z=theta[i], x=calBP[], y=C14err[]);
    sd[i] <- (sigma[i]^2+sigmaCurve[i]^2)^(1/2);
    X[i] ~ dnorm(mean=mu[i], sd=sd[i]);
  }
  r1 ~ dnorm(0, sd=0.0004);
  r2 ~ dexp(1/0.0004);
  chp ~ T(dnorm(1700, sd=100), 1200, 2200); 
  changept <- round(chp);
})

# Define Constants and Data ####
data(intcal20)
constants <- list(N=length(obs.caldates), calBP=intcal20$CalBP, C14BP=intcal20$C14Age, C14err=intcal20$C14Age.sigma)
data <- list(X=obs.data$CRA, sigma=obs.data$Error)

# Define Initialisation Function
initsFunction.double_exp <- function() list(r1=rnorm(1, sd=0.0004), r2=rexp(1, 1/0.0004), chp=round(rtruncnorm(1, mean=1700, sd=100, a=1200, b=2200)), theta=as.numeric(obs.data$MedCalDate))

# Run MCMC ####
mcmc.double_exp.samples<- nimbleMCMC(code=double_exp, constants=constants, data=data, niter=10000, nchains=3, thin=6, nburnin=1000, summary=FALSE, monitors=c('r1', 'r2', 'chp', 'theta'), WAIC=TRUE, samplesAsCodaMCMC=TRUE, inits=initsFunction.double_exp, setSeed=c(1, 2, 3))

# Quick Summaries
gelman.diag(mcmc.double_exp.samples$samples)$psrf[1:3,]

# Plot
tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/Grain_MPD.tiff", units='in', res=300, width=8, height=3)
par(mfrow=c(1,3))
postHPDplot(mcmc.double_exp.samples$samples$chain1[, 'r1'], xlab='', ylab='', show.hpd.val=FALSE) +
  title(main="dExp-model(Grain):r1", cex=1.2)
  mtext(TeX('$r_1$'),side = 1,line=1.5,cex = 1, padj=1)
postHPDplot(mcmc.double_exp.samples$samples$chain1[, 'r2'], xlab='', ylab='', show.hpd.val=FALSE)
  title(main="dExp-model(Grain):r2", cex=1.2)
  mtext(TeX('$r_2$'),side = 1,line=1.5,cex = 1, padj=1)
postHPDplot(abs(round(BPtoBCAD(mcmc.double_exp.samples$samples$chain1[, 'chp']))), xlab='', ylab='', show.hpd.val=FALSE, xlim=c(150, 350))
  title(main="dExp-model(Grain):c", cex=1.2)
  mtext(TeX('$AD$'),side = 1,line=1.5,cex = 1, padj=1)
dev.off()

rhat.double_exp <- gelman.diag(mcmc.double_exp.samples$samples)
ess.double_exp <- effectiveSize(mcmc.double_exp.samples$samples)
params.double_exp <- list(r1 = c(mcmc.double_exp.samples$samples$chain1[, 'r1'], mcmc.double_exp.samples$samples$chain2[, 'r1'], mcmc.double_exp.samples$samples$chain3[, 'r1']),
                          r2 = c(mcmc.double_exp.samples$samples$chain1[, 'r2'], mcmc.double_exp.samples$samples$chain2[, 'r2'], mcmc.double_exp.samples$samples$chain3[, 'r2']),
                          mu = round(c(mcmc.double_exp.samples$samples$chain1[, 'chp'], mcmc.double_exp.samples$samples$chain2[, 'chp'], mcmc.double_exp.samples$samples$chain3[, 'chp'])))
pp.check.double_exp.cal <- postPredSPD(obs.data$CRA, obs.data$Error, calCurve = 'intcal20', model = dDoubleExponentialGrowth, a = 2200, b=1200, params=params.double_exp, nsim = 500, ncores = 5, verbose=FALSE, method='calsample')

tiff(file="/Users/jch/Documents/github/P3K-HB_Demography/Graph/Demographics(Grain_dExp_model).tiff", units='in', res=300, width=7, height=5)
par(mfrow=c(1, 1))
plot(pp.check.double_exp.cal, interval = 0.95,calendar='BCAD')
  title(main="dExp-model(Grain)", cex=1.2)
  legend('topleft',legend=c('95% Prediction Interval','Positive Deviation','Negative Deviation','Observed SPD'),lwd=c(5,5,5,2),col=c('lightgrey','red','blue','black'),bty='n', cex=c(1, 1, 1, 1))
dev.off()
