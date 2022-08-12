library(ADMUR)
library(DEoptimR)

data <- read.csv("/Users/jch/Documents/github/P3K-HB_Demography/Data/Korea_CR_14C.csv")

CalArray <- makeCalArray(calcurve=intcal20, calrange=c(1200, 2200))
PD <- phaseCalibrator(data, CalArray, remove.external = TRUE)

SPD <- summedPhaseCalibrator( data=data, calcurve=intcal20, calrange=c(1200,2200) )

CPL.1 <- JDEoptim(lower=0, upper=1, fn=objectiveFunction, PDarray=PD, type='CPL', NP=20)
CPL.2 <- JDEoptim(lower=rep(0,3), upper=rep(1,3), fn=objectiveFunction, PDarray=PD, type='CPL', NP=60)
CPL.3 <- JDEoptim(lower=rep(0,5), upper=rep(1,5), fn=objectiveFunction, PDarray=PD, type='CPL', NP=80)
CPL.4 <- JDEoptim(lower=rep(0,7), upper=rep(1,7), fn=objectiveFunction, PDarray=PD, type='CPL', NP=100)

# exponential has a single parameter, which can be negative (decay).
exp <- JDEoptim(lower=-0.01, upper=0.01, fn=objectiveFunction, PDarray=PD, type='exp', NP=20)

# uniform has no parameters so a search is not required.
uniform <- objectiveFunction(NULL, PD, type='uniform')

# convert parameters to model PDs
CPL1 <- convertPars(pars=CPL.1$par, years=1200:2200, type='CPL')
CPL2 <- convertPars(pars=CPL.2$par, years=1200:2200, type='CPL')
CPL3 <- convertPars(pars=CPL.3$par, years=1200:2200, type='CPL')
CPL4 <- convertPars(pars=CPL.4$par, years=1200:2200, type='CPL')
EXP <- convertPars(pars=exp$par, years=1200:2200, type='exp')

# Plot SPD and five competing models:
plotPD(SPD)
cols <- c('firebrick','orchid2','coral2','steelblue','goldenrod3')
lines(CPL1$year, CPL1$pdf, col=cols[1], lwd=2)
lines(CPL2$year, CPL2$pdf, col=cols[2], lwd=2)
lines(CPL3$year, CPL3$pdf, col=cols[3], lwd=2)
lines(CPL4$year, CPL4$pdf, col=cols[4], lwd=2)
lines(EXP$year, EXP$pdf, col=cols[5], lwd=2)
legend <- c('1-CPL','2-CPL','3-CPL','4-CPL','exponential')
legend(x=1300, y=0.0025, cex=0.7, lwd=2, col=cols, bty='n', legend=legend)