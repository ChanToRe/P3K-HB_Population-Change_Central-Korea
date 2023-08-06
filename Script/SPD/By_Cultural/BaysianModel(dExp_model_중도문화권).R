library(rcarbon)
library(nimbleCarbon)
library(truncnorm)
library(coda)
library(here)
library(latex2exp)
library(dplyr)
library(showtext)
library(sysfonts)

#font_add_google('Nanum Gothic', family='NanumGothic')
#showtext_auto()
#par(family="NanumGothic")

# Load Data
data <- read.csv(here("./Data/C14_Master.csv"))
data <- subset(data, subset = Type == "백제(呂·凸자형)" | (Type == "신라(呂·凸자형)" | (Type == "신라(방형)" & (Area == "영서" | Area == "경기북부"))))

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


##### Double-Exponential model #####
double_exp <- nimbleCode({
  for (i in 1:N){
    theta[i] ~ dDoubleExponentialGrowth(a=2200, b=1200, r1=r1, r2=r2, mu=changept);
    mu[i] <- interpLin(z=theta[i], x=calBP[], y=C14BP[]);
    sigmaCurve[i] <- interpLin(z=theta[i], x=calBP[], y=C14err[]);
    sd[i] <- (sigma[i]^2+sigmaCurve[i]^2)^(1/2);
    X[i] ~ dnorm(mean=mu[i], sd=sd[i]);
  }
  r1 ~ dnorm(0, sd=0.007);
  r2 ~ dnorm(0, sd=0.007);
  chp ~ T(dnorm(1700, sd=100), 1200, 2200);
  changept <- round(chp);
})

# Define Constants and Data ####
data(intcal20)
constants <- list(N=length(obs.caldates), calBP=intcal20$CalBP, C14BP=intcal20$C14Age, C14err=intcal20$C14Age.sigma)
data <- list(X=obs.data$CRA, sigma=obs.data$Error)

# Define Initialisation Function
initsFunction.double_exp <- function() list(r1=rnorm(1, sd=0.007), r2=rnorm(1, 0.007), chp=round(rtruncnorm(1, mean=1700, sd=100, a=1200, b=2200)), theta=as.numeric(obs.data$MedCalDate))

# Run MCMC ####
mcmc.double_exp.samples<- nimbleMCMC(code=double_exp, constants=constants, data=data, niter=10000, nchains=3, thin=6, nburnin=1000, summary=FALSE, monitors=c('r1', 'r2', 'chp', 'theta'), WAIC=TRUE, samplesAsCodaMCMC=TRUE, inits=initsFunction.double_exp, setSeed=c(1, 2, 3))

# Quick Summaries
gelman.diag(mcmc.double_exp.samples$samples)$psrf[1:3,]

# Plot
tiff(file=here("./Graph/dexp_HPD(중도문화권).tiff"), units='in', res=300, width=8, height=3)
par(mfrow=c(1,3))
postHPDplot(mcmc.double_exp.samples$samples$chain1[, 'r1'], xlab='', ylab='', show.hpd.val=FALSE)
title(main="중도문화권(dExp-model):r1", cex=1.2)
mtext(TeX('$r_1$'),side = 1,line=1.5,cex = 1, padj=1)
postHPDplot(mcmc.double_exp.samples$samples$chain1[, 'r2'], xlab='', ylab='', show.hpd.val=FALSE)
title(main="중도문화권(dExp-model):r2", cex=1.2)
mtext(TeX('$r_2$'),side = 1,line=1.5,cex = 1, padj=1)
postHPDplot(abs(round(BPtoBCAD(mcmc.double_exp.samples$samples$chain1[, 'chp']))), xlab='', ylab='', show.hpd.val=FALSE, xlim=c(100, 500))
title(main="중도문화권(dExp-model):c", cex=1.2)
mtext(TeX('$AD$'),side = 1,line=1.5,cex = 1, padj=1)
dev.off()

rhat.double_exp <- gelman.diag(mcmc.double_exp.samples$samples)
ess.double_exp <- effectiveSize(mcmc.double_exp.samples$samples)
params.double_exp <- list(r1 = c(mcmc.double_exp.samples$samples$chain1[, 'r1'], mcmc.double_exp.samples$samples$chain2[, 'r1'], mcmc.double_exp.samples$samples$chain3[, 'r1']),
                          r2 = c(mcmc.double_exp.samples$samples$chain1[, 'r2'], mcmc.double_exp.samples$samples$chain2[, 'r2'], mcmc.double_exp.samples$samples$chain3[, 'r2']),
                          mu = round(c(mcmc.double_exp.samples$samples$chain1[, 'chp'], mcmc.double_exp.samples$samples$chain2[, 'chp'], mcmc.double_exp.samples$samples$chain3[, 'chp'])))
pp.check.double_exp.cal <- postPredSPD(obs.data$CRA, obs.data$Error, calCurve = 'intcal20', model = dDoubleExponentialGrowth, a = 2200, b=1200, params=params.double_exp, nsim = 500, ncores = 10, verbose=FALSE, method='calsample')

tiff(file=here("./Graph/dexp_model(중도문화권).tiff"), units='in', res=300, width=7, height=5)
par(mfrow=c(1, 1))
plot(pp.check.double_exp.cal, interval = 0.95,calendar='BCAD')
legend('topleft',legend=c('95% Prediction Interval','Positive Deviation','Negative Deviation','Observed SPD'),lwd=c(5,5,5,2),col=c('lightgrey','red','blue','black'),bty='n', cex=c(1, 1, 1, 1))
dev.off()

tiff(file=here("./Graph/dexp_fit(중도문화권).tiff"), units='in', res=300, width=7, height=7)
par(mfrow=c(1, 1))
postHPDplot(postPredCor(pp.check.double_exp.cal), xlab="중도문화권 PCC",ylab='Density',xlim=c(0,1))
dev.off()

mcmc.double_exp.samples$WAIC