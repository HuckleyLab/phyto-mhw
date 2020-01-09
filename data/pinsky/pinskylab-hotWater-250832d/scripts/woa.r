
# Read in World Ocean Atlas data
nms <- c('lat', 'lon', 'm0', 'm5', 'm10', 'm15', 'm20', 'm25', 'm30', 'm35', 'm40', 'm45', 'm50', 'm55', 'm60', 'm65', 'm70', 'm75', 'm80', 'm85', 'm90', 'm95', 'm100', 'm125', 'm150', 'm175', 'm200', 'm225', 'm250', 'm275', 'm300', 'm325', 'm350', 'm375', 'm400', 'm425', 'm450', 'm475', 'm500', 'm550', 'm600', 'm650', 'm700', 'm750', 'm800', 'm850', 'm900', 'm950', 'm1000', 'm1050', 'm1100', 'm1150', 'm1200', 'm1250', 'm1300', 'm1350', 'm1400', 'm1450', 'm1500', 'm1550', 'm1600', 'm1650', 'm1700', 'm1750', 'm1800', 'm1850', 'm1900', 'm1950', 'm2000', 'm2100', 'm2200', 'm2300', 'm2400', 'm2500', 'm2600', 'm2700', 'm2800', 'm2900', 'm3000', 'm3100', 'm3200', 'm3300', 'm3400', 'm3500', 'm3600', 'm3700', 'm3800', 'm3900', 'm4000', 'm4100', 'm4200', 'm4300', 'm4400', 'm4500', 'm4600', 'm4700', 'm4800', 'm4900', 'm5000', 'm5100', 'm5200', 'm5300', 'm5400', 'm5500')
dps <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400, 425, 450, 475, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400, 1450, 1500, 1550, 1600, 1650, 1700, 1750, 1800, 1850, 1900, 1950, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900, 5000, 5100, 5200, 5300, 5400, 5500) # the depths associated with each column (after lat/lon)

woa <- read.csv('data_dl/woa/woa13_decav_t00an01v2.csv', skip=2, row.names=NULL, header=FALSE, col.names=nms) # use col.names to force all columns to be read (file doesn't have trailing ,s for blank columns)


# Initial plot (slow)

col = rgb(0,0,0,0.01)
plot(dps, woa[1,3:ncol(woa)], type='l', ylab='temperature', xlab='depth (m)', ylim=c(-2,30), col=col, xlim=c(0,1000))
for(i in 2:nrow(woa)){
	lines(dps, woa[i,3:ncol(woa)], col=col)
}


###########################################
# average temperature within lat bands
###########################################
woalat <- aggregate(list(m0=woa$m0, m5=woa$m5, m10=woa$m10, m15=woa$m15, m20=woa$m20, m25=woa$m25, m30=woa$m30, m35=woa$m35, m40=woa$m40, m45=woa$m45, m50=woa$m50, m55=woa$m55, m60=woa$m60, m65=woa$m65, m70=woa$m70, m75=woa$m75, m80=woa$m80, m85=woa$m85, m90=woa$m90, m95=woa$m95, m100=woa$m100, m125=woa$m125, m150=woa$m150, m175=woa$m175, m200=woa$m200, m225=woa$m225, m250=woa$m250, m275=woa$m275, m300=woa$m300, m325=woa$m325, m350=woa$m350, m375=woa$m375, m400=woa$m400, m425=woa$m425, m450=woa$m450, m475=woa$m475, m500=woa$m500, m550=woa$m550, m600=woa$m600, m650=woa$m650, m700=woa$m700, m750=woa$m750, m800=woa$m800, m850=woa$m850, m900=woa$m900, m950=woa$m950, m1000=woa$m1000, m1050=woa$m1050, m1100=woa$m1100, m1150=woa$m1150, m1200=woa$m1200, m1250=woa$m1250, m1300=woa$m1300, m1350=woa$m1350, m1400=woa$m1400, m1450=woa$m1450, m1500=woa$m1500, m1550=woa$m1550, m1600=woa$m1600, m1650=woa$m1650, m1700=woa$m1700, m1750=woa$m1750, m1800=woa$m1800, m1850=woa$m1850, m1900=woa$m1900, m1950=woa$m1950, m2000=woa$m2000, m2100=woa$m2100, m2200=woa$m2200, m2300=woa$m2300, m2400=woa$m2400, m2500=woa$m2500, m2600=woa$m2600, m2700=woa$m2700, m2800=woa$m2800, m2900=woa$m2900, m3000=woa$m3000, m3100=woa$m3100, m3200=woa$m3200, m3300=woa$m3300, m3400=woa$m3400, m3500=woa$m3500, m3600=woa$m3600, m3700=woa$m3700, m3800=woa$m3800, m3900=woa$m3900, m4000=woa$m4000, m4100=woa$m4100, m4200=woa$m4200, m4300=woa$m4300, m4400=woa$m4400, m4500=woa$m4500, m4600=woa$m4600, m4700=woa$m4700, m4800=woa$m4800, m4900=woa$m4900, m5000=woa$m5000, m5100=woa$m5100, m5200=woa$m5200, m5300=woa$m5300, m5400=woa$m5400, m5500=woa$m5500), by=list(lat=woa$lat), FUN=mean, na.rm=TRUE)



# plot lat averages, color by lat
#col = rgb(0,0,0,0.1)
cols <- rainbow(nrow(woalat))
plot(dps, woalat[1,2:ncol(woalat)], type='l', ylab='temperature', xlab='depth (m)', ylim=c(-2,30), col=cols[1], xlim=c(0,1000))
for(i in 2:nrow(woalat)){
	lines(dps, woalat[i,2:ncol(woalat)], col=cols[i])
}

abline(v=100, lty=2)

# plot SST vs. lat
plot(woalat$lat, woalat$m0, type='l')
abline(v=5)


# plot all profiles at 0.5°
inds <- which(woa$lat==0.5)
cols <- rainbow(length(inds))
plot(dps, woa[inds[1],3:ncol(woa)], type='l', ylab='temperature', xlab='depth (m)', ylim=c(-2,30), col=cols[1], xlim=c(0,1000))
for(i in 2:length(inds)){
	lines(dps, woa[inds[i],3:ncol(woa)], col=cols[i])
}


# plot all profiles where depth < 1000
inds <- which(is.na(woa$m1050))
col = rgb(0,0,0,0.1)
plot(dps, woa[inds[1],3:ncol(woa)], type='l', ylab='temperature', xlab='depth (m)', ylim=c(-2,30), col=col, xlim=c(0,1000))
for(i in 2:length(inds)){
	lines(dps, woa[inds[i],3:ncol(woa)], col=col)
}

##################
# Average all
##################

woaave <- apply(woa[,3:ncol(woa)], MARGIN=2, FUN=mean, na.rm=TRUE)

plot(dps, woaave, type='l')

plot(dps, woaave, type='l', xlim=c(0,300))

woaave['m50'] - woaave['m0']
woaave['m300'] - woaave['m0']


#######################
# Average change in temperature
#######################

mean(woa$m50 - woa$m0, na.rm=TRUE)
mean(woa$m300 - woa$m0, na.rm=TRUE)
mean(woa$m1000 - woa$m0, na.rm=TRUE)