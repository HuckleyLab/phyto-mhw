#set Acclimation Response Ratio for terrestrial and marine organisms
# from Gunderson and Stillman model-averaged results (Gunderson pers. comm.)
ARRland <- 0.11
ARRoce <- 0.24


dT <- seq(-5,5)
dTmaxPerfect <- dT
dTmaxOce <- dT*ARRoce
dTmaxLand <- dT*ARRland


quartz(width=4, height=4)
# png(width=4, height=4, units='in', res=300, file='figures/acclimation_diagram.png')
par(mai=c(1,1,0.2, 0.2), las=1)
plot(dT, dTmaxPerfect, type='l', lwd=2, lty=2, xlab='Change in temperature (°C)', ylab='Change in Tmax (°C)')
lines(dT, dTmaxOce, col='#1f78b4', lwd=5)
lines(dT, dTmaxLand, col='#33a02c', lwd=5)

dev.off()