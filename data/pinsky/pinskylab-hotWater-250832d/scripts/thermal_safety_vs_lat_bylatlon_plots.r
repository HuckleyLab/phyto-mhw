# Exploratory plots for TSM

require(mgcv)
require(data.table)
require(ggplot2)
require(lattice)

dat <- fread('temp/warmingtolerance_byspecies.csv')


#############################
##### Thab only plots ###
#############################
# plot un-adjusted thab vs. adjustments for elevation (all from climatology)
	cex = 0.5
	quartz(width=10,height=4)
	# pdf(width=10, height=4, file='figures/thab.adj_vs_thab.pdf')
	par(mfrow=c(1,3), las=1, mai=c(0.5, 0.5, 0.3, 0.05), mgp=c(2,1,0))

	dat[Realm=='Terrestrial',plot(thabsum, thabsum.adj-thabsum, xlim=c(-10,45), ylim=c(-16,16), col='green', main='thabsum elevation adjustment', cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial',plot(thabmaxmo, thabmaxmo.adj-thabmaxmo, xlim=c(-10,45), ylim=c(-16,16), col='green', main='thabmaxmo elevation adjustment', cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial',plot(thabmaxhr, thabmaxhr.adj-thabmaxhr, xlim=c(-10,45), ylim=c(-16,16), col='green', main='thabmaxhr elevation adjustment', cex=cex)]
	abline(h=0)

	dev.off()


# plot NM vs. Thab_maxhr (climatology, elevation-corrected). Terrestrial only.
	cex = 0.5
	quartz(width=10,height=4)
	# pdf(width=10, height=4, file='figures/NM_vs_thabmaxhr.adj.pdf')
	par(mfrow=c(1,4), las=1, mai=c(0.5, 0.5, 0.3, 0.05), mgp=c(2,1,0), omi=c(0,0,0.25,0))

	dat[Realm=='Terrestrial',plot(thabmaxhr.adj, NM_1cm_airshade, main='1 cm shade', cex=cex)]
	abline(0,1)

	dat[Realm=='Terrestrial',plot(thabmaxhr.adj, NM_2m_airshade, main='2 m shade', cex=cex)]
	abline(0,1)

	dat[Realm=='Terrestrial',plot(thabmaxhr.adj, NM_exposed_bodytemp, main='Exposed body', cex=cex)]
	abline(0,1)

	dat[Realm=='Terrestrial',plot(thabmaxhr.adj, NM_exposed_air, main='Exposed air', cex=cex)]
	abline(0,1)

	mtext('NicheMapper vs. air temperatures', side=3, outer=TRUE)

	dev.off()
	

# plot NM and Thab vs. latitude (no elevation correction)
	# ylims <- dat[,range(thabsum, thabmaxmo, thabmaxhr, NM_exposed_air, NM_exposed_bodytemp, na.rm=TRUE)]
	ylims <- dat[,range(thabsum, thabmaxmo, NM_1cm_airshade, NM_2m_airshade, NM_exposed_air, NM_exposed_bodytemp, na.rm=TRUE)]
	xlims <- c(-80, 80) 
	cex=0.5
	setkey(dat, lat)

	quartz(width=8, height=4)
	# pdf(width=8, height=4, file='figures/thab_vs_lat.pdf')
	par(mfrow=c(1,2))
	
	dat[Realm=='Terrestrial', plot(lat, thabsum, ylim=ylims, xlim=xlims, cex=cex, main='Terrestrial', xlab='Latitude', ylab='Temperature °C')]
		dat[Realm=='Terrestrial' & !is.na(lat) & !is.na(thabsum),lines(lat, predict(loess(thabsum~lat)))]
	dat[Realm=='Terrestrial', points(lat, thabmaxmo, col='pink', cex=cex)]
		dat[Realm=='Terrestrial' & !is.na(lat) & !is.na(thabmaxmo),lines(lat, predict(loess(thabmaxmo~lat)), col='pink')]
	dat[Realm=='Terrestrial', points(lat, NM_2m_airshade, col='red', cex=cex)]
		dat[Realm=='Terrestrial' & !is.na(lat) & !is.na(NM_1cm_airshade),lines(lat, predict(loess(NM_1cm_airshade~lat)), col='red')]
	dat[Realm=='Terrestrial', points(lat, NM_exposed_air, col='purple', cex=cex)]
		dat[Realm=='Terrestrial' & !is.na(lat) & !is.na(NM_exposed_air),lines(lat, predict(loess(NM_exposed_air~lat)), col='purple')]
	dat[Realm=='Terrestrial', points(lat, NM_exposed_bodytemp, col='darkslateblue', cex=cex)]
		dat[Realm=='Terrestrial' & !is.na(lat) & !is.na(NM_exposed_bodytemp),lines(lat, predict(loess(NM_exposed_bodytemp~lat)), col='darkslateblue')]

	dat[Realm=='Marine', plot(lat, thabsum, ylim=ylims, xlim=xlims, cex=cex, main='Marine', xlab='Latitude', ylab='Temperature °C')]
		dat[Realm=='Marine' & !is.na(lat) & !is.na(thabsum),lines(lat, predict(loess(thabsum~lat)))]
	dat[Realm=='Marine', points(lat, thabmaxmo, col='pink', cex=cex)]
		dat[Realm=='Marine' & !is.na(lat) & !is.na(thabmaxmo),lines(lat, predict(loess(thabmaxmo~lat)), col='pink')]
	dat[Realm=='Marine', points(lat, thabmaxhr, col='red', cex=cex)]
		dat[Realm=='Marine' & !is.na(lat) & !is.na(thabmaxhr),lines(lat, predict(loess(thabmaxhr~lat)), col='red')]


	legend('topleft', col=c('black', 'pink', 'red', 'purple', 'darkslateblue'), legend=c('Summer', 'Warmest month', 'Daily max (land: 2m shade)', 'Air - exposed', 'Body - exposed'), pch=1, cex=0.6)

	dev.off()

#############################
##### Tmax plots ###
#############################
# Examine Thab values used for Tmax adjustments: plot tmax_acc with thabsum, thabmaxmo, thabmaxhr (like Fig. S6C in Sunday et al. 2014 PNAS)
	setkey(dat, thabmaxhr.adj)
	cex1 <- 0.6
	cex2 <- 0.4

	quartz(width=8, height=4)
	# pdf(width=8, height=4, file='figures/acclimation_corrections.pdf')
	par(mfrow=c(1,2))
	dat[Realm=='Terrestrial',plot(1:.N, tmax_acc, col='grey', cex=cex1, main='Terrestrial', ylab='°C', xlab='Rank order', ylim=c(-10,45))]
	dat[Realm=='Terrestrial',points(1:.N, thabmaxhr.adj, col='black', cex=cex1)]
	dat[Realm=='Terrestrial',points(1:.N, thabmaxmo.adj, col='red', cex=cex1)]
	dat[Realm=='Terrestrial',points(1:.N, thabsum.adj, col='pink', cex=cex2)]

	dat[Realm=='Marine',plot(1:.N, tmax_acc, col='grey', cex=cex1, main='Marine', ylab='°C', xlab='Rank order', ylim=c(-10,45))]
	dat[Realm=='Marine',points(1:.N, thabmaxhr.adj, col='black', cex=cex1)]
	dat[Realm=='Marine',points(1:.N, thabmaxmo.adj, col='red', cex=cex1)]
	dat[Realm=='Marine',points(1:.N, thabsum.adj, col='pink', cex=cex2)]

	legend('topleft', legend=c('tmax_acc', 'thabmaxhr.adj', 'thabmaxmo.adj', 'thabsum.adj'), col=c('grey', 'black', 'red', 'pink'), pch=1, cex=0.5)

	dev.off()

# plot adjustments for acclimation and elevation and duration vs un-adjusted tmax
# acclimation: accounts for difference between experimental acclimation temperature and max monthly habitat temperature
# duration: accounts for difference between 30 or 90 days and acclimation duration time
	cex=0.7
	
	quartz(width=5,height=6.5)
	# pdf(width=5, height=6.5, file='figures/tmax.adjDelta_vs_tmax.pdf')
	par(mfrow=c(3,2), las=1, mai=c(0.5, 0.5, 0.3, 0.05), mgp=c(2,1,0))

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tmax, tmax.accsum-tmax, xlim=c(3,60), ylim=c(-6,6), col='green', main='acclimation to thabsum', cex=cex, cex.main=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tmax, tmax.accsum-tmax, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tmax, tmax.accsum-tmax, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tmax, tmax.accsum-tmax, col='blue', pch=2, cex=cex)]

	legend('topright', legend=c('Terrestrial', 'Marine', 'crit', 'leth'), col=c('green', 'blue', 'black', 'black'), pch=c(1,1,1,2), cex=cex)
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tmax, tmax.accmo-tmax, xlim=c(3,60), ylim=c(-6,6), col='green', main='acclimation to thab_maxmo', cex=cex, cex.main=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tmax, tmax.accmo-tmax, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tmax, tmax.accmo-tmax, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tmax, tmax.accmo-tmax, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tmax, tmax.accsum.elev-tmax, xlim=c(3,60), ylim=c(-6,6), col='green', main='acclimation to thabsum\nand elevation adjustment', cex=cex, cex.main=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tmax, tmax.accsum.elev-tmax, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tmax, tmax.accsum.elev-tmax, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tmax, tmax.accsum.elev-tmax, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tmax, tmax.accmo.elev-tmax, xlim=c(3,60), ylim=c(-6,6), col='green', main='acclimation to thab_maxmo\nand elevation adjustment', cex=cex, cex.main=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tmax, tmax.accmo.elev-tmax, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tmax, tmax.accmo.elev-tmax, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tmax, tmax.accmo.elev-tmax, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tmax, tmax.accsum.elev.dur-tmax, xlim=c(3,60), ylim=c(-6,6), col='green', main='acclimation to thabsum & time\nand elevation adjustment', cex=cex, cex.main=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tmax, tmax.accsum.elev.dur-tmax, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tmax, tmax.accsum.elev.dur-tmax, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tmax, tmax.accsum.elev.dur-tmax, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tmax, tmax.accmo.elev.dur-tmax, xlim=c(3,60), ylim=c(-6,6), col='green', main='acclimation to thab_maxmo & time\nand elevation adjustment', cex=cex, cex.main=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tmax, tmax.accmo.elev.dur-tmax, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tmax, tmax.accmo.elev.dur-tmax, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tmax, tmax.accmo.elev.dur-tmax, col='blue', pch=2, cex=cex)]
	abline(h=0)


	dev.off()


# Examine acclimation, elevation, and duration adjustments by plotting adjusted vs. unadjusted Tmax: 

	quartz(width=5, height=6.5)
	# pdf(width=5, height=6.5, file='figures/tmax.adj_vs_tmax.pdf')	
	par(mfrow=c(3,2), las=1, mai=c(0.5, 0.5, 0.3, 0.05), mgp=c(2,1,0), omi=c(0,0,0.3,0))
		
		# plot tmax.acc vs. tmax
	dat[, plot(tmax, tmax.accmo, type='n', main='Acclimation to max month')] # set up axes
	dat[Realm=='Terrestrial', points(tmax, tmax.accmo, col='green')]
	dat[Realm=='Marine', points(tmax, tmax.accmo, col='blue')]
	abline(0,1)

	dat[, plot(tmax, tmax.accsum, type='n', main='Acclimation to summer')]
	dat[Realm=='Terrestrial', points(tmax, tmax.accsum, col='green')]
	dat[Realm=='Marine', points(tmax, tmax.accsum, col='blue')]
	abline(0,1)

		# plot tmax.acc.elev vs. tmax.acc
	dat[, plot(tmax.accmo, tmax.accmo.elev, type='n', main='Adjust for elevation')]
	dat[Realm=='Terrestrial', points(tmax.accmo, tmax.accmo.elev, col='green')]
	dat[Realm=='Marine', points(tmax.accmo, tmax.accmo.elev, col='blue')]
	abline(0,1)

	dat[, plot(tmax.accsum, tmax.accsum.elev, type='n', main='Adjust for elevation')]
	dat[Realm=='Terrestrial', points(tmax.accsum, tmax.accsum.elev, col='green')]
	dat[Realm=='Marine', points(tmax.accsum, tmax.accsum.elev, col='blue')]
	abline(0,1)

		# plot tmax.acc.elev.dur vs. tmax.acc.elev
	dat[, plot(tmax.accmo.elev, tmax.accmo.elev.dur, type='n', main='Adjust for duration')]
	dat[Realm=='Terrestrial', points(tmax.accmo.elev, tmax.accmo.elev.dur, col='green')]
	dat[Realm=='Marine', points(tmax.accmo.elev, tmax.accmo.elev.dur, col='blue')]
	abline(0,1)

	dat[, plot(tmax.accsum.elev, tmax.accsum.elev.dur, type='n', main='Adjust for duration')]
	dat[Realm=='Terrestrial', points(tmax.accsum.elev, tmax.accsum.elev.dur, col='green')]
	dat[Realm=='Marine', points(tmax.accsum.elev, tmax.accsum.elev.dur, col='blue')]
	abline(0,1)

	mtext('Tmax adjustments', side=3, outer=TRUE, line=0.5)

	dev.off()


# plot tmax.adj with NM_2m_airshade (land) & thab_maxhr (ocean) against latitude
	setkey(dat, lat)
	ylims <- dat[,c(0,max(c(thabmaxhr.adj, thabmaxhr, tmax, tmax.accsum.elev, predict(loess(NM_exposed_bodytemp~lat, na.action=na.exclude))), na.rm=TRUE))]
	xlims <- c(-80, 80)

	quartz(width=8, height=4)
	# pdf(width=8, height=4, file='figures/tmax_and_thabmaxhr_vs_lat.pdf')
	par(mfrow=c(1,2), mai=c(0.7,0.7,0.5,0.2), mgp=c(2,0.7,0), cex.axis=0.8)
	dat[Realm=='Terrestrial',plot(lat, NM_2m_airshade, ylim=ylims, xlim=xlims, main='Terrestrial', ylab='Temperature (°C)', xlab='Latitude (°N)', cex=0.7)]
	dat[Realm=='Terrestrial',points(lat, thabmaxhr, col='grey', cex=0.7)] # w/out elevation adjustment
	dat[Realm=='Terrestrial',points(lat, tmax.accsum.elev, col='red', cex=0.7)]
	dat[Realm=='Terrestrial',points(lat, tmax, col='pink', cex=0.5)] # w/out tmax_acc and tmax_metric adjustment

		dat[Realm=='Terrestrial',lines(lat, predict(loess(NM_2m_airshade~lat, na.action=na.exclude)))]
		dat[Realm=='Terrestrial',lines(lat, predict(loess(thabmaxhr~lat, na.action=na.exclude)), col='grey')]
		dat[Realm=='Terrestrial',lines(lat, predict(loess(NM_exposed_bodytemp~lat, na.action=na.exclude)), col='purple')]

		dat[Realm=='Terrestrial' & !is.na(tmax.accsum.elev),lines(lat, predict(loess(tmax.accsum.elev~lat, na.action=na.exclude)), col='red')]
		dat[Realm=='Terrestrial' & !is.na(tmax.accsum.elev.dur),lines(lat, predict(loess(tmax.accsum.elev.dur~lat, na.action=na.exclude)), col='red', lty=2)] # with tmax duration adjustment as well (fewer data points)
		dat[Realm=='Terrestrial',lines(lat, predict(loess(tmax~lat, na.action=na.exclude)), col='pink')]

	dat[Realm=='Marine',plot(lat, thabmaxhr.adj, ylim=ylims, xlim=xlims, main='Marine', ylab='Temperature (°C)', xlab='Latitude (°N)', cex=0.7)]
	dat[Realm=='Marine',points(lat, tmax.accsum.elev, col='red', cex=0.7)]
	dat[Realm=='Marine',points(lat, tmax, col='pink', cex=0.5)] # w/out tmax_acc adjustment

		dat[Realm=='Marine',lines(lat, predict(loess(thabmaxhr.adj~lat, na.action=na.exclude)))]
		dat[Realm=='Marine',lines(lat, predict(loess(tmax.accsum.elev~lat, na.action=na.exclude)), col='red')]
		dat[Realm=='Marine' & !is.na(tmax.accsum.elev.dur),lines(lat, predict(loess(tmax.accsum.elev.dur~lat, na.action=na.exclude)), col='red', lty=2)]
		dat[Realm=='Marine',lines(lat, predict(loess(tmax~lat, na.action=na.exclude)), col='pink')]

	legend('topleft', legend=c('NM_2m_airshade/thabmaxhr', 'thabmaxhr', 'NM_exposed_bodytemp', 'tmax.accsum.elev (tmax_acc&elevation)', 'tmax.accsum.elev.dur (tmax_acc&elevation&duration)', 'tmax'), col=c('black', 'grey', 'purple', 'red', 'red', 'pink'), pch=c(1,1,NA,1,NA,1), lty=c(1,1,1,1,2,1), cex=0.5, bty='n')

	dev.off()



#############################
##### TSM plots ###
#############################
# plot un-adjusted tsm vs. adjustments for acclimation, elevation, and duration (only using air temperature from climatologies)
# former accounts for difference between experimental acclimation temperature and max monthly habitat temperature
	cex=0.7
	ylims=c(-15,12)
	xlims=c(-10,30)
	
	quartz(width=6,height=6)
	# pdf(width=6, height=6, file='figures/tsm_adj_vs_tsm.pdf')
	par(mfrow=c(3,3), las=1, mai=c(0.5, 0.5, 0.3, 0.05), mgp=c(2,0.7,0), cex.main=0.8, cex.lab=0.8)

	plot(0,0,bty='n', xaxt='n', yaxt='n', xlab='', ylab='', col='white')

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_maxhr_accsum-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabsum\nno elevation correction', cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tsm_maxhr, tsm_maxhr_accsum-tsm_maxhr, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accsum-tsm_maxhr, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accsum-tsm_maxhr, col='blue', pch=2, cex=cex)]
	legend('topright', legend=c('Terrestrial', 'marine', 'crit', 'leth'), col=c('green', 'blue', 'black', 'black'), pch=c(1,1,1,2), cex=0.7)
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_maxhr_accmo-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thab_maxmo\nno elevation correction', cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tsm_maxhr, tsm_maxhr_accmo-tsm_maxhr, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accmo-tsm_maxhr, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accmo-tsm_maxhr, col='blue', pch=2, cex=cex)]
	abline(h=0)
	
	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_maxhr_elev-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='no acclimation\nw/ elevation correction', cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tsm_maxhr, tsm_maxhr_elev-tsm_maxhr, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_elev-tsm_maxhr, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_elev-tsm_maxhr, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_maxhr_accsum.elev-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabsum\nw/ elevation correction', cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tsm_maxhr, tsm_maxhr_accsum.elev-tsm_maxhr, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accsum.elev-tsm_maxhr, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accsum.elev-tsm_maxhr, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_maxhr_accmo.elev-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thab_maxmo\nw/ elevation correction', cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tsm_maxhr, tsm_maxhr_accmo.elev-tsm_maxhr, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accmo.elev-tsm_maxhr, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accmo.elev-tsm_maxhr, col='blue', pch=2, cex=cex)]
	abline(h=0)

	plot(0,0,bty='n', xaxt='n', yaxt='n', xlab='', ylab='', col='white')

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_maxhr_accsum.elev.dur-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabsum\nw/ elevation&duration correction', cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tsm_maxhr, tsm_maxhr_accsum.elev.dur-tsm_maxhr, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accsum.elev.dur-tsm_maxhr, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accsum.elev.dur-tsm_maxhr, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_maxhr_accmo.elev.dur-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thab_maxmo\nw/ elevation&duration correction', cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='crit',points(tsm_maxhr, tsm_maxhr_accmo.elev.dur-tsm_maxhr, col='blue', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accmo.elev.dur-tsm_maxhr, col='green', pch=2, cex=cex)]
	dat[Realm=='Marine' & tmax_metric=='leth',points(tsm_maxhr, tsm_maxhr_accmo.elev.dur-tsm_maxhr, col='blue', pch=2, cex=cex)]
	abline(h=0)

	dev.off()


# plot un-adjusted tsm vs. adjustments for acclimation, elevation, and duration (for NicheMapper)
# former accounts for difference between experimental acclimation temperature and max monthly habitat temperature
	cex=0.7
	ylims=c(-10,10)
	xlims=c(-10,30)
	
	quartz(width=6,height=6)
	# pdf(width=6, height=6, file='figures/tsm_NM_1cm_airshade_vs_tsm.pdf')
	par(mfrow=c(3,3), las=1, mai=c(0.5, 0.5, 0.3, 0.05), mgp=c(2,0.7,0), cex.main=0.8, cex.lab=0.7)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_NM_1cm_airshade-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='no acclimation\nno elevation correction', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_NM_1cm_airshade-tsm_maxhr, col='green', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_NM_1cm_airshade_accsum-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabsum\nno elevation correction', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_NM_1cm_airshade_accsum-tsm_maxhr, col='green', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_NM_1cm_airshade_accmo-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabmaxmo\nno elevation correction', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_NM_1cm_airshade_accmo-tsm_maxhr, col='green', pch=2, cex=cex)]
	abline(h=0)

	plot(0,0,bty='n', xaxt='n', yaxt='n', xlab='', ylab='', col='white')
	legend('topright', legend=c('Terrestrial', 'marine', 'crit', 'leth'), col=c('green', 'blue', 'black', 'black'), pch=c(1,1,1,2), cex=0.7, bty='n')

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_NM_1cm_airshade_accsum.elev-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabsum\nw/elevation correction', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_NM_1cm_airshade_accsum.elev-tsm_maxhr, col='green', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_NM_1cm_airshade_accmo.elev-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabmaxmo\nw/ elevation correction', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_NM_1cm_airshade_accmo.elev-tsm_maxhr, col='green', pch=2, cex=cex)]
	abline(h=0)

	plot(0,0,bty='n', xaxt='n', yaxt='n', xlab='', ylab='', col='white')

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_NM_1cm_airshade_accsum.elev.dur-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabsum\nw/elevation&duration correction', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_NM_1cm_airshade_accsum.elev.dur-tsm_maxhr, col='green', pch=2, cex=cex)]
	abline(h=0)

	dat[Realm=='Terrestrial' & tmax_metric=='crit',plot(tsm_maxhr, tsm_NM_1cm_airshade_accmo.elev.dur-tsm_maxhr, xlim=xlims, ylim=ylims, col='green', main='acclimation to thabmaxmo\nw/ elevation&duration correction', cex=cex)]
	dat[Realm=='Terrestrial' & tmax_metric=='leth',points(tsm_maxhr, tsm_NM_1cm_airshade_accmo.elev.dur-tsm_maxhr, col='green', pch=2, cex=cex)]
	abline(h=0)


	dev.off()


# tsm_maxhr by Class boxplot
	ylims=c(-7,30)
	quartz(width=8, height=4)
	# pdf(width=8, height=4, file='figures/tsm_maxhr_vs_Class.pdf')
	par(mfrow=c(1,2), las=2)
	boxplot(tsm_maxhr_adjadjsum ~ Class, cex.axis=0.5, data=droplevels(dat[Realm=='Terrestrial',]), main='terrestrial\ntsm_maxhr_adjadjsum', ylim=ylims, ylab='Thermal Safety Margin °C')
		abline(h=0, lty=2, col='grey')
	boxplot(tsm_maxhr_adjadjsum ~ Class, cex.axis=0.5, data=droplevels(dat[Realm=='Marine',]), main='marine\ntsm_maxhr_adjadjsum', ylim=ylims)
		abline(h=0, lty=2, col='grey')

	dev.off()
	

# tsm_NM_2m_airshade (terrestrial non-amphibian) and tsm_NM_shade_body_wet (amphibian) and tsm_maxhr (marine) by lat
	setkey(dat, lat)
	xlims <- c(-80,80)
	ylims <- dat[,range(tsm_NM_favorable, tsm_maxhr_accsum.elev, na.rm=TRUE)]
	cols1 <- c('#b2df8a70', '#a6cee370') # land, ocean light transparent
	cols2 <- c('#33a02c', '#1f78b4') # land, ocean dark
	cex=0.5

	l1 <- dat[Realm=='Terrestrial' & !is.na(tsm_NM_favorable),loess(tsm_NM_favorable~lat, na.action=na.exclude)]
	l2 <- dat[Realm=='Marine' & !is.na(tsm_maxhr_accsum.elev),loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)]
	p1 <- predict(l1, se=TRUE)
	p2 <- predict(l2, se=TRUE)
	p1$lat <- dat[Realm=='Terrestrial' & !is.na(tsm_NM_favorable), lat]
	p2$lat <- dat[Realm=='Marine' & !is.na(tsm_maxhr_accsum.elev), lat]

	quartz(width=5, height=4)
	# pdf(width=5, height=4, file='figures/tsm_vs_lat_loess_noexposed.pdf')
	par(las=1, mai=c(0.7, 0.7, 0.3, 0.1), mgp=c(2.5,0.7,0))
	
	plot(-100,0, xlim=xlims, ylim=ylims, main='Thermal Safety Margins', xlab='Latitude (°N)', ylab='Thermal safety margin (°C)') # set up
	abline(h=0, lty=2, col='grey')

		# terrestrial shade or wet skin (favorable microclimate)
	dat[Realm=='Terrestrial', points(lat, tsm_NM_favorable, col=cols1[1], cex=cex)]
		with(p1, polygon(c(lat, rev(lat)), c(fit+1.96*se.fit, rev(fit-1.96*se.fit)), col=cols1[1], border=NA)) # 95%CI
		with(p1, lines(lat, fit, col=cols2[1], lwd=2)) # mean

		# marine
	dat[Realm=='Marine', points(lat, tsm_maxhr_accsum.elev, col=cols1[2], cex=cex)]
		with(p2, polygon(c(lat, rev(lat)), c(fit+1.96*se.fit, rev(fit-1.96*se.fit)), col=cols1[2], border=NA))
		with(p2, lines(lat, fit, col=cols2[2], lwd=2))

	legend('bottomleft', legend=c('Terrestrial shade (+wet skin for amphibians)', 'Marine surface'), pch=1, col=cols1, lty=1, bty='n', cex=0.5)

	dev.off()



# tsm_NM_2m_airshade (terrestrial non-amphibian) and tsm_NM_shade_body_wet (amphibian) and tsm_maxhr (marine) by lat
# also show tsm_NM_exposed_bodytemp for comparison
	setkey(dat, lat)
	xlims <- c(-80,80)
	ylims <- dat[,range(tsm_NM_favorable, tsm_NM_exposed_bodytemp_accsum.elev, tsm_maxhr_accsum.elev, na.rm=TRUE)]
	cols1 <- c('#AAAAAA70', '#b2df8a70', '#a6cee370') # exposed, land, ocean light transparent
	cols2 <- c('#808080', '#33a02c', '#1f78b4') # exposed, land, ocean dark
	cex=0.5
		

	quartz(width=5, height=4)
	# pdf(width=5, height=4, file='figures/tsm_vs_lat_loess.pdf')
	# pdf(width=5, height=4, file='figures/tsm_vs_lat_loess_only_landshade.pdf')
	# pdf(width=5, height=4, file='figures/tsm_vs_lat_loess_only_landshade&exposed.pdf')
	par(las=1, mai=c(0.7, 0.7, 0.3, 0.1), mgp=c(2.5,0.7,0))
	
	plot(-100,0, xlim=xlims, ylim=ylims, main='Thermal Safety Margins', xlab='Latitude (°N)', ylab='Thermal safety margin (°C)') # set up
	abline(h=0, lty=2, col='grey')

		# exposed
	dat[Realm=='Terrestrial', points(lat, tsm_NM_exposed_bodytemp_accsum.elev, col=cols1[1], cex=cex)]
		dat[Realm=='Terrestrial' & !is.na(tsm_NM_exposed_bodytemp_accsum.elev),lines(lat, predict(loess(tsm_NM_exposed_bodytemp_accsum.elev~lat, na.action=na.exclude)), col=cols2[1], lwd=2)]
			# could add polygon of loess +/- SE

		# terrestrial shade or wet skin
	dat[Realm=='Terrestrial', points(lat, tsm_NM_favorable, col=cols1[2], cex=cex)]
		dat[Realm=='Terrestrial' & !is.na(tsm_NM_favorable),lines(lat, predict(loess(tsm_NM_favorable~lat, na.action=na.exclude)), col=cols2[2], lwd=2)]
			# could add polygon of loess +/- SE

		# marine
	dat[Realm=='Marine', points(lat, tsm_maxhr_accsum.elev, col=cols1[3], cex=cex)]
		dat[Realm=='Marine' & !is.na(tsm_maxhr_accsum.elev),lines(lat, predict(loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)), col=cols2[3], lwd=2)]
			# could add polygon of loess +/- SE

	legend('bottomleft', legend=c('Terrestrial exposed body', 'Terrestrial shade (wet skin for amphibians)', 'Marine surface'), pch=1, col=cols1, lty=1, bty='n', cex=0.5)

	dev.off()



# tsm_NM_1cm_airshade (terrestrial) and tsm_maxhr (marine) by lat NO AMPHIBIANS
	setkey(dat, lat)
	xlims <- c(-80,80)
	ylims <- dat[Class != 'Amphibia',range(tsm_NM_1cm_airshade_accsum.elev, tsm_NM_exposed_bodytemp_accsum.elev, tsm_maxhr_accsum.elev, na.rm=TRUE)]

	quartz(width=5, height=4)
	# pdf(width=5, height=4, file='figures/tsm_vs_lat_loess_no_amphib.pdf')
	par(las=1, mai=c(0.7, 0.7, 0.3, 0.1), mgp=c(2.5,0.7,0))
	
	dat[Realm=='Terrestrial' & Class != 'Amphibia', plot(lat, tsm_NM_exposed_bodytemp_accsum.elev, col='grey', xlim=xlims, ylim=ylims, main='Thermal Safety Margins no Amphibians')]
		dat[Realm=='Terrestrial' & !is.na(tsm_NM_exposed_bodytemp_accsum.elev) & Class != 'Amphibia',lines(lat, predict(loess(tsm_NM_exposed_bodytemp_accsum.elev~lat, na.action=na.exclude)), col='grey')]
		abline(h=0, lty=2, col='grey')

	dat[Realm=='Terrestrial' & Class != 'Amphibia', points(lat, tsm_NM_1cm_airshade_accsum.elev, col='green')]
		dat[Realm=='Terrestrial' & !is.na(tsm_NM_1cm_airshade_accsum.elev) & Class != 'Amphibia',lines(lat, predict(loess(tsm_NM_1cm_airshade_accsum.elev~lat, na.action=na.exclude)), col='green')]
	dat[Realm=='Marine' & Class != 'Amphibia', points(lat, tsm_maxhr_accsum.elev, col='blue')]
		dat[Realm=='Marine' & !is.na(tsm_maxhr_accsum.elev) & Class != 'Amphibia',lines(lat, predict(loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)), col='blue')]

	legend('bottomleft', legend=c('Terrestrial 1cm shade', 'Marine surface', 'Terrestrial exposed body'), pch=1, col=c('green', 'blue', 'grey'), lty=1, bty='n', cex=0.5)

	dev.off()


# Amphibians: tsm_NM_2m_airshade vs. tsm_NM_exposed_body_wet
	dat[animal.type=='amphibian', plot(tsm_NM_2m_airshade, tsm_NM_exposed_body_wet)]
		abline(0,1, lty=2)
		
	dat[animal.type=='amphibian', mean(tsm_NM_2m_airshade - tsm_NM_exposed_body_wet)] #



# tsm_maxhr vs. lat by Class
	require(RColorBrewer)
	setkey(dat, lat) # sort by lat
	ylims <- dat[,range(c(tsm_maxhr, tsm_maxhr_adjadjsum), na.rm=TRUE)]
	xlims <- c(-90,90)
	cols <- brewer.pal(12, 'Paired')[c(1:12, 1:8)] # repeat to get to 20
	pchs <- c(rep(1, 10), rep(4,10))
	i1 <- dat$Realm=='Terrestrial' & !is.na(dat$tsm_maxhr)
	i2 <- dat$Realm=='Marine' & !is.na(dat$tsm_maxhr)
	i3 <- dat$Realm=='Terrestrial' & !is.na(dat$tsm_maxhr_adjadjsum)
	i4 <- dat$Realm=='Marine' & !is.na(dat$tsm_maxhr_adjadjsum)
	
	quartz(width=8, height=6)
	# pdf(width=8, height=6, file='figures/tsm_maxhr_vs_lat_by_Class.pdf')
	par(mfrow=c(2,2), mgp=c(2,0.7,0), las=1, cex.axis=0.8, mai=c(0.7, 0.7, 0.2, 0.1))
	
		# tsm_maxhr
	dat[i1, plot(tsm_maxhr ~ lat_max, main='Terrestrial', ylim=ylims, xlim=xlims, ylab='Thermal Safety Margin °C \n(tsm_maxhr)', col=cols[as.numeric(Class)], pch=pchs[as.numeric(Class)])]
		dat[i1 & Class=='Reptilia',lines(lat, predict(loess(tsm_maxhr~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Reptilia'])]
		dat[i1 & Class=='Amphibia',lines(lat, predict(loess(tsm_maxhr~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Amphibia'])]
		dat[i1 & Class=='Insecta',lines(lat, predict(loess(tsm_maxhr~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Insecta'])]
	legend('topright', legend=levels(dat$Class), col=cols, pch=pchs, cex=0.3)

	dat[i2, plot(tsm_maxhr ~ lat_max, main='Marine', ylim=ylims, xlim=xlims, col=cols[as.numeric(Class)], pch=pchs[as.numeric(Class)])]
		dat[i2 & Class=='Actinopterygii',lines(lat, predict(loess(tsm_maxhr~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Actinopterygii'])]
		dat[i2 & Class=='Bivalvia',lines(lat, predict(loess(tsm_maxhr~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Bivalvia'])]

		# tsm_maxhr_adjadjsum
	plot(tsm_maxhr_adjadjsum ~ lat_max, data=dat[i3,], main='Terrestrial', ylim=ylims, xlim=xlims, ylab='Thermal Safety Margin °C \n(tsm_maxhr_adjadjsum)', col=cols[as.numeric(dat$Class[i3])], pch=pchs[as.numeric(dat$Class[i3])])
		dat[i3 & Class=='Reptilia',lines(lat, predict(loess(tsm_maxhr_adjadjsum~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Reptilia'])]
		dat[i3 & Class=='Amphibia',lines(lat, predict(loess(tsm_maxhr_adjadjsum~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Amphibia'])]
		dat[i3 & Class=='Insecta',lines(lat, predict(loess(tsm_maxhr_adjadjsum~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Insecta'])]

	plot(tsm_maxhr_adjadjsum ~ lat_max, data=dat[i4,], main='Marine', ylim=ylims, xlim=xlims, col=cols[as.numeric(dat$Class[i4])], pch=pchs[as.numeric(dat$Class[i4])])
		dat[i4 & Class=='Actinopterygii',lines(lat, predict(loess(tsm_maxhr_adjadjsum~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Actinopterygii'])]
		dat[i4 & Class=='Bivalvia',lines(lat, predict(loess(tsm_maxhr_adjadjsum~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Bivalvia'])]


	dev.off()


# tsm_NM_1cm_airshade vs. lat by Class
	require(RColorBrewer)
	setkey(dat, lat) # sort by lat
	ylims <- dat[,range(c(tsm_NM_1cm_airshade, tsm_maxhr_accsum.elev), na.rm=TRUE)]
	xlims <- c(-90,90)
	cols <- brewer.pal(12, 'Paired')[c(1:12, 1:8)] # repeat to get to 20
	pchs <- c(rep(1, 10), rep(4,10))
	i1 <- dat$Realm=='Terrestrial' & !is.na(dat$tsm_maxhr_accsum.elev)
	i2 <- dat$Realm=='Marine' & !is.na(dat$tsm_maxhr_accsum.elev)
	i3 <- dat$Realm=='Terrestrial' & !is.na(dat$tsm_NM_1cm_airshade)
	i2 <- dat$Realm=='Marine' & !is.na(dat$tsm_maxhr_accsum.elev)
	
	quartz(width=8, height=6)
	# pdf(width=8, height=6, file='figures/tsm_maxhr_w_NM_vs_lat_by_Class.pdf')
	par(mfrow=c(2,2), mgp=c(2,0.7,0), las=1, cex.axis=0.8, mai=c(0.7, 0.7, 0.2, 0.1))
	
		# tsm_maxhr_accsum.elev
	plot(tsm_maxhr_accsum.elev ~ lat_max, data=dat[i1,], main='Terrestrial', ylim=ylims, xlim=xlims, ylab='Thermal Safety Margin °C \n(tsm_maxhr_accsum.elev)', col=cols[as.numeric(dat$Class[i1])], pch=pchs[as.numeric(dat$Class[i1])])
		dat[i1 & Class=='Reptilia',lines(lat, predict(loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Reptilia'])]
		dat[i1 & Class=='Amphibia',lines(lat, predict(loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Amphibia'])]
		dat[i1 & Class=='Insecta',lines(lat, predict(loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Insecta'])]

		abline(h=0, col='grey', lty=2)

	plot(tsm_maxhr_accsum.elev ~ lat_max, data=dat[i2,], main='Marine', ylim=ylims, xlim=xlims, col=cols[as.numeric(dat$Class[i2])], pch=pchs[as.numeric(dat$Class[i2])])
		dat[i2 & Class=='Actinopterygii',lines(lat, predict(loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Actinopterygii'])]
		dat[i2 & Class=='Bivalvia',lines(lat, predict(loess(tsm_maxhr_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Bivalvia'])]

		abline(h=0, col='grey', lty=2)

		# tsm_NM_1cm_airshade_accsum.elev
	plot(tsm_NM_1cm_airshade_accsum.elev ~ lat_max, data=dat[i1,], main='terrestrial NicheMapper', ylim=ylims, xlim=xlims, ylab='Thermal Safety Margin °C \n(tsm_NM_1cm_airshade_accsum.elev)', col=cols[as.numeric(dat$Class[i1])], pch=pchs[as.numeric(dat$Class[i1])])
		dat[i1 & Class=='Reptilia',lines(lat, predict(loess(tsm_NM_1cm_airshade_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Reptilia'])]
		dat[i1 & Class=='Amphibia',lines(lat, predict(loess(tsm_NM_1cm_airshade_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Amphibia'])]
		dat[i1 & Class=='Insecta',lines(lat, predict(loess(tsm_NM_1cm_airshade_accsum.elev~lat, na.action=na.exclude)), col=cols[levels(dat$Class)=='Insecta'])]

		abline(h=0, col='grey', lty=2)

	plot(0,0,bty='n', xaxt='n', yaxt='n', xlab='', ylab='', col='white')
	legend('topright', legend=levels(dat$Class), col=cols, pch=pchs, cex=0.7, ncol=3, bty='n')


	dev.off()


# tsm_maxhr: plot model result on top of data (unfolded lat)
	cols <- c('#b2df8a', '#1f78b4') # land, ocean
	cols2 <- c('#b2df8a88', '#1f78b488') # land, ocean faded somewhat
	cols3 <- c('#b2df8a55', '#1f78b455') # land, ocean faded

	quartz(width=5, height=5)
	plot(dat$tsm_maxhr~dat$lat, type="n", ylab="Thermal safety margin", xlab="latitude")

	with(dat[dat$Realm=="marine",], points(tsm_maxhr~lat, col=cols2[2], pch=16))
	with(dat[dat$Realm=="terrestrial",], points(tsm_maxhr~lat, col=cols2[1], pch=16))

#	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], polygon(c(lat, rev(lat)), c(tsm+se, rev(tsm-se)), col=cols3[2], border=FALSE))
#	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], polygon(c(lat, rev(lat)), c(tsm+se, rev(tsm-se)), col=cols3[1], border=FALSE))

	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], lines(tsm_maxhr~lat, col=cols[2], lwd=3)) # current TSM
	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], lines(tsm_maxhr~lat, col=cols[1], lwd=3))

#	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], polygon(c(lat, rev(lat)), c(tsm_fut+sefut, rev(tsm_fut-sefut)), col=cols3[2], border=FALSE))
#	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], polygon(c(lat, rev(lat)), c(tsm_fut+sefut, rev(tsm_fut-sefut)), col=cols3[1], border=FALSE))
	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], lines(tsm_fut85~lat, col=cols[2], lwd=3, lty=2)) # future TSM
	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], lines(tsm_fut85~lat, col=cols[1], lwd=3, lty=2))

	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], lines(tsm_sum~lat, col=cols[2], lwd=1, lty=1)) # current TSM summer
	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], lines(tsm_sum~lat, col=cols[1], lwd=1, lty=1))

	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], lines(tsm_fut85sum~lat, col=cols[2], lwd=1, lty=2)) # future TSM summer
	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], lines(tsm_fut85sum~lat, col=cols[1], lwd=1, lty=2))

	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], lines(tsm_futacc85~lat, col=cols[2], lwd=3, lty=2)) # future TSM acc
	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], lines(tsm_futacc85~lat, col=cols[1], lwd=3, lty=2))

	with(newdat[newdat$Realm=='Marine' & newdat$lat > -80,], lines(tsm_futacc85sum~lat, col=cols[2], lwd=1, lty=2)) # future TSM acc summer
	with(newdat[newdat$Realm=='Terrestrial' & newdat$lat > -55 & newdat$lat < 70,], lines(tsm_futacc85sum~lat, col=cols[1], lwd=1, lty=2))


	legend('topleft', legend=c('land', 'ocean'), col=cols, lwd=1, bty='n')


######################################
## Combined Tmax, Thab, TSM plots
######################################

# TSM vs. Tb
par(mfrow=c(1,2))
dat[Realm=='Terrestrial', plot(tb_favorableGHCND95, tsm_favorableGHCND95)]
dat[Realm=='Marine', plot(tb_favorableGHCND95, tsm_favorableGHCND95)]

	# lattice plot
	latshingle <- shingle(dat$lat, intervals=matrix(c(seq(-90,60,by=15), seq(-60,90,by=15)),byrow=FALSE, ncol=2))
	xyplot(tsm_favorableGHCND95 ~ tb_favorableGHCND95 | latshingle,
		data = dat[Realm=='Terrestrial',])
		
# TSM vs. Tmax
	# lattice plot
	latshingle <- shingle(dat$lat, intervals=matrix(c(seq(-90,60,by=15), seq(-60,90,by=15)),byrow=FALSE, ncol=2))
	xyplot(tsm_favorableGHCND95 ~ tmax.accsum.elev | latshingle,
		data = dat[Realm=='Terrestrial',])