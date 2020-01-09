#################
## Parameters and functions
#################
source('scripts/error.bar.R')
require(data.table)
require(RColorBrewer)
require(png)

# takes values 0-1 and turns them into colors. takes ColorBrewer pallete as argument.
colramp <- function(x, pal='RdBu', alpha=255){
	cr <- colorRamp(brewer.pal(9, pal))
	i <- !is.na(x)
	cols <- rgb(cr(x[i]), alpha=rep(alpha, sum(i)), maxColorValue=255)
	out <- rep(NA, length(x))
	out[i] <- cols
	return(out)
}


cols <- c('#33a02c', '#1f78b4') # land, ocean
cols2 <- c('#33a02c33', '#1f78b433') # land, ocean partially transparent
cols3 <- c('#b2df8a', '#a6cee3') # land, ocean light
cols4 <- c('#b2df8a55', '#a6cee355') # land, ocean light, partially transparent
cols5 <- c('#ff7f00', '#e31a1c') # orange/red for Te favorable/exposed
cols6 <- c('#ff7f0055', '#e31a1c55') # orange/red for Te favorable/exposed, partially transparent
lw <- 2 # line width


#############################################################################
## Fig. 1
## hottest hours across latitudes, warming of hottest hours across latitudes
#############################################################################

# read in data
lstmaxhrlat <- fread('temp/hadex2txxlat.csv') # uses HadEx2
sstmaxhrlat <- fread('temp/sstmaxhrlat.csv')
lstfutwarmlat <- read.csv('temp/warmingmaxhr_bylat_land.csv', row.names=1) # uses TXx
sstfutwarmlat <- read.csv('temp/warmingmaxhr_bylat_ocean.csv', row.names=1)

# plot marine and Terrestrial comparisons

	quartz(width=5,height=2.5)
	# pdf('figures/fig1.pdf', width=5, height=2.5)
	# png('figures/fig1.png', width=5, height=2.5, units='in', res=300)
	xlims=c(-90, 90)
	par(mfrow=c(1,2), mai=c(0.2, 0.65, 0.2, 0.1), omi = c(0.4,0,0,0), mgp=c(1.6,0.4,0), las=1, tcl=-0.3, cex.axis=0.9)

	# Mean and max hr
	i1 <- complete.cases(lstmaxhrlat)
	i2 <- complete.cases(sstmaxhrlat)
	print(sum(lstmaxhrlat$n[i1])) # land sample size
	print(sum(sstmaxhrlat$n[i2])) # ocean sample size

	plot(0, 0, xlab='', ylab=expression(paste('Temperature (', degree, 'C)')), ylim=c(0,45), xlim=xlims, type='n') # just to set it up

	lstmaxhrlat[i1, polygon(c(lat, rev(lat)), c(mean-sd, rev(mean+sd)), col=cols2[1], border=NA)]
	sstmaxhrlat[i2, polygon(c(lat, rev(lat)), c(mean-sd, rev(mean+sd)), col=cols2[2], border=NA)]
	lstmaxhrlat[i1, lines(lat, mean, col=cols[1], lwd=lw)]
	sstmaxhrlat[i2, lines(lat, mean, col=cols[2], lwd=lw)]

	mtext(side=3, "a", adj=0)
	axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
	mtext(side=1, expression(paste('Latitude (', degree, 'N)')), outer=TRUE, cex=1, adj=0.26, line=0.5)

	
	# Future warming
	i1.1 <- complete.cases(lstfutwarmlat) & lstfutwarmlat$lat < -66.25 # south of southern ocean
	i1.2 <- complete.cases(lstfutwarmlat) & lstfutwarmlat$lat > -52.75 # north of southern ocean
	i2 <- complete.cases(sstfutwarmlat) 
	print(sum(lstfutwarmlat$tasmax26n[i1.1|i1.2])) # land sample size
	print(sum(lstfutwarmlat$tasmax85n[i1.1|i1.2])) # land sample size
	print(sum(sstfutwarmlat$tosmax26n[i2])) # ocean sample size
	print(sum(sstfutwarmlat$tosmax85n[i2])) # ocean sample size

	plot(0, 0, xlab='', ylab=expression(paste('Projected warming (', Delta*degree, 'C)')), ylim=c(0,7), xlim=xlims, type='n') # just to set it up
		# rcp85
	with(lstfutwarmlat[i1.1,], polygon(c(lat, rev(lat)), c(tasmax85+tasmax85sd, rev(tasmax85-tasmax85sd)), col=cols2[1], border=NA))
	with(lstfutwarmlat[i1.2,], polygon(c(lat, rev(lat)), c(tasmax85+tasmax85sd, rev(tasmax85-tasmax85sd)), col=cols2[1], border=NA))
	with(sstfutwarmlat[i2,], polygon(c(lat, rev(lat)), c(tosmax85+tosmax85sd, rev(tosmax85-tosmax85sd)), col=cols2[2], border=NA))
	with(lstfutwarmlat[i1.1,], lines(lat, tasmax85, col=cols[1], lwd=lw))
	with(lstfutwarmlat[i1.2,], lines(lat, tasmax85, col=cols[1], lwd=lw))
	with(sstfutwarmlat, lines(lat, tosmax85, col=cols[2], lwd=lw))

		# rcp26
	with(lstfutwarmlat[i1.1,], polygon(c(lat, rev(lat)), c(tasmax26+tasmax26sd, rev(tasmax26-tasmax26sd)), col=cols2[1], border=NA))
	with(lstfutwarmlat[i1.2,], polygon(c(lat, rev(lat)), c(tasmax26+tasmax26sd, rev(tasmax26-tasmax26sd)), col=cols2[1], border=NA))
	with(sstfutwarmlat[i2,], polygon(c(lat, rev(lat)), c(tosmax26+tosmax26sd, rev(tosmax26-tosmax26sd)), col=cols2[2], border=NA))
	with(lstfutwarmlat[i1.1,], lines(lat, tasmax26, col=cols[1], lwd=lw, lty=2))
	with(lstfutwarmlat[i1.2,], lines(lat, tasmax26, col=cols[1], lwd=lw, lty=2))
	with(sstfutwarmlat, lines(lat, tosmax26, col=cols[2], lwd=lw, lty=2))

	legend('topleft', legend=c('RCP 8.5', 'RCP 2.6'), col='black', lty=c(1,2), lwd=1, bty='n', cex=0.7)

	axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
	mtext(side=3, "b", adj=0)
	mtext(side=1, expression(paste('Latitude (', degree, 'N)')), outer=TRUE, cex=1, adj=0.88, line=0.5)

	# Legend
	legend(-190,-0.7, legend=c('terrestrial', 'marine'), col=c(cols), lty=c('solid','solid'), lwd=2, bty='n', cex=0.7, xpd=NA) # xpd NA to clip to device region. specify dash/space length with hex codes.

	dev.off()


#############################################################################
## Fig. 2
## (A) Tmax and Thab on land and 
## (B) at sea
## (C) Thermal safety margin of marine and terrestrial species
#############################################################################

# read in data
tsmpreds <- fread('temp/warmingtolerance_bylatlon.csv')
tsm <- fread('temp/warmingtolerance_byspecies.csv', stringsAsFactors=TRUE)
tsmpredsfut <- fread('temp/warmingtolerance_bylat_to2100.csv')
fish <- readPNG('data/images/fish-1739110_640.png')
lizard <- readPNG('data/images/lizard-1299577_640.png')


# reorder by lat
setkey(tsm, lat)
setkey(tsmpreds, lat)
setkey(tsmpredsfut, lat)

# adjust clipart colors
lizard[,,1:3] <- aperm(array(col2rgb(cols2[1]), dim=c(3,640,320)), c(2,3,1))/256 # land. Aperm needed since arrays filled by first dimension first.
fish[,,1:3] <- aperm(array(col2rgb(cols2[2]), dim=c(3,414,640)), c(2,3,1))/256 # land. Aperm needed since arrays filled by first dimension first.

# plot

	quartz(width=5, height=6)

	# pdf(width=5, height=6, file='figures/fig2.pdf')
	# png('figures/fig2.png', width=5, height=6, units='in', res=300)
	xlims <- c(-78, 78)
	layout(matrix(c(1,2,3), nrow=3, ncol=1), heights=c(1,70/65,2))
	par(las=1, mai=c(0, 0, 0.2, 2), omi=c(0.5,0.7,0,0), mgp=c(3, 0.7, 0), cex.lab=0.9, cex.axis=0.8, tcl=-0.4, cex.lab=1.5, cex.axis=1.2, bty='L')
	
	# A: Tmax and body temp Land
		ylimsA <- tsm[Realm=='Terrestrial',range(c(tb_favorableGHCND95, tb_sunGHCND95), na.rm=TRUE)]
		
		plot(-100, -100, ylim=ylimsA, xlim=xlims, main='', ylab='', xlab='', xaxt='n')
			# Tb in favorable and full sun microclimates
		tsm[Realm=='Terrestrial', points(lat, tb_favorableGHCND95, col=cols6[1], cex=0.7)]
		tsm[Realm=='Terrestrial', points(lat, tb_sunGHCND95, col=cols6[2], cex=0.7)]

				# prediction ribbons
		tsmpreds[Realm=='Terrestrial' & lat<57 & lat>-47, polygon(c(lat, rev(lat)), c(tb_favorableGHCND95-tb_favorableGHCND95se, rev(tb_favorableGHCND95+tb_favorableGHCND95se)), col=cols6[1], border=NA)] # 95% CIs
		tsmpreds[Realm=='Terrestrial' & lat<57 & lat>-47, polygon(c(lat, rev(lat)), c(tb_sunGHCND95-tb_sunGHCND95se, rev(tb_sunGHCND95+tb_sunGHCND95se)), col=cols6[2], border=NA)]

				# prediction lines
		tsmpreds[Realm=='Terrestrial' & lat<57 & lat>-47, lines(lat, tb_favorableGHCND95, col=cols5[1])]
		tsmpreds[Realm=='Terrestrial' & lat<57 & lat>-47, lines(lat, tb_sunGHCND95, col=cols5[2])]

			# Tmax points, ribbons, lines
		tsm[Realm=='Terrestrial', points(lat, tmax.accsum.elev, col=rgb(0,0,0,0.5), cex=0.7)]
		tsmpreds[Realm=='Terrestrial' & lat<57 & lat>-55, polygon(c(lat, rev(lat)), c(tmax-tmaxse, rev(tmax+tmaxse)), col=rgb(0,0,0,0.33), border=NA)] 
		tsmpreds[Realm=='Terrestrial' & lat<57 & lat>-55, lines(lat, tmax, col=rgb(0,0,0,1))]
		
		text(-76,ylimsA[2]-3, 'a', cex=1.5)
		rasterImage(lizard, -70, 49-10, -57, 75-10)

		axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
		axis(1, at=c(-50, 0, 50), labels=FALSE, tcl=-0.4)

		leg1 <- c(as.expression(bquote('T'['b,exposed'])), as.expression(bquote('T'['b,protected'])), as.expression(bquote('T'[max]*minute)))
		legend(80,ylimsA[2], bty='n', col=c(cols5[2:1], rgb(0,0,0,1)), pch=1, legend=leg1, border=NA, cex=1, pt.cex=1, y.intersp=1.5, xpd=TRUE)

	# B: Tmax and body temp ocean
		ylimsB <- range(c(tsm[Realm=='Marine',c(tb_favorableGHCND95, tb_sunGHCND95, tmax.accsum.elev)], tsmpreds[Realm=='Marine' & lat<80 & lat>-80, tmax+tmaxse]), na.rm=TRUE)

		plot(-100, -100, ylim=ylimsB, xlim=xlims, main='', ylab='', xlab='', xaxt='n')

			# operative temperatures
				# points
		tsm[Realm=='Marine', points(lat, tb_favorableGHCND95, col=cols6[1], cex=0.7)]
		tsm[Realm=='Marine', points(lat, tb_sunGHCND95, col=cols6[2], cex=0.7)]

				# prediction ribbons
		tsmpreds[Realm=='Marine' & lat<80 & lat>-80, polygon(c(lat, rev(lat)), c(tb_sunGHCND95-tb_sunGHCND95se, rev(tb_sunGHCND95+tb_sunGHCND95se)), col=cols6[2], border=NA)]
		tsmpreds[Realm=='Marine' & lat<80 & lat>-80, polygon(c(lat, rev(lat)), c(tb_favorableGHCND95-tb_favorableGHCND95se, rev(tb_favorableGHCND95+tb_favorableGHCND95se)), col=cols6[1], border=NA)] # 95% CIs

				# prediction lines
		tsmpreds[Realm=='Marine' & lat<80 & lat>-80, lines(lat, tb_sunGHCND95, col=cols5[2])]
		tsmpreds[Realm=='Marine' & lat<80 & lat>-80, lines(lat, tb_favorableGHCND95, col=cols5[1])]

			# Tmax
		tsm[Realm=='Marine' & !is.na(tb_favorableGHCND95),points(lat, tmax.accsum.elev, col=rgb(0,0,0,0.5), cex=0.7)] 		
		tsmpreds[Realm=='Marine' & lat<80 & lat>-80, polygon(c(lat, rev(lat)), c(tmax-tmaxse, rev(tmax+tmaxse)), col=rgb(0,0,0,0.33), border=NA)] # 95% CIs
		tsmpreds[Realm=='Marine' & lat<80 & lat>-80, lines(lat, tmax, col=rgb(0,0,0,1))]

		text(-76,ylimsB[2]-3, 'b', cex=1.5)
		rasterImage(fish, -70, 63-18*414/640-20, -70+26, 63-20)
		axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
		axis(1, at=c(-50, 0, 50), labels=FALSE, tcl=-0.4)

#		leg2 <- c(as.expression(bquote('T'['e,exposed'])), as.expression(bquote('T'['e,favorable'])), as.expression(bquote('T'[max])))
#		legend(80,70, bty='n', col=c(cols5[2:1], rgb(0,0,0,1)), pch=1, legend=leg2, border=NA, cex=1, pt.cex=1, y.intersp=1.5, xpd=TRUE)

	# C: Thermal safety margin
		ylimsC <- range(tsm$tsm_favorableGHCND95, na.rm=TRUE)

		linds1 <- tsm[, Realm=='Terrestrial'] # indices for land (points)
		oinds1 <- tsm[, Realm=='Marine'] # for ocean (points)
		linds2 <- tsmpreds[, Realm=='Terrestrial' & lat > -50 & lat < 60] # indices for land (where data were available) (lines)
		oinds2 <- tsmpreds[, Realm=='Marine' & lat > -80 & lat < 75] # for ocean (where data were available) (lines)

		# set up plot
	tsm[linds1 & animal.type!='amphibian', plot(0, -100, type='n', xlab='', ylab='Thermal safety margin (°C)', ylim=ylimsC, xlim=xlims, log='')]

		# points: solid for Terrestrial (square, triangle), open for marine (circle, cross)
	tsm[linds1 & animal.type!='amphibian', points(lat, tsm_favorableGHCND95, col=cols2[1], pch=15, cex=0.7)] # current TSM in favorable Realms
	tsm[linds1 & animal.type=='amphibian', points(lat, tsm_favorableGHCND95, col=cols2[1], pch=17, cex=0.7)]
	tsm[oinds1 & mobility=='sessile', points(lat, tsm_favorableGHCND95, col=cols2[2], pch=1, cex=0.7)]
	tsm[oinds1 & mobility!='sessile', points(lat, tsm_favorableGHCND95, col=cols2[2], pch=4, cex=0.7)]
	
		# predictions
		tsmpreds[linds2, polygon(c(lat, rev(lat)), c(tsm_favorableGHCND95-tsm_favorableGHCND95se, rev(tsm_favorableGHCND95+tsm_favorableGHCND95se)), col=cols2[1], border=NA)] # 95% CIs
		tsmpreds[oinds2, polygon(c(lat, rev(lat)), c(tsm_favorableGHCND95-tsm_favorableGHCND95se, rev(tsm_favorableGHCND95+tsm_favorableGHCND95se)), col=cols2[2], border=NA)]

		tsmpreds[linds2, lines(lat, tsm_favorableGHCND95, col=cols[1], lwd=2)] # mean prediction
		tsmpreds[oinds2, lines(lat, tsm_favorableGHCND95, col=cols[2], lwd=2)]

		# 2081-2100
		tsmpredsfut[linds2, polygon(c(lat, rev(lat)), c(tsm_2081_2100_acc_rcp85-tsm_2081_2100_acc_rcp85se, rev(tsm_2081_2100_acc_rcp85+tsm_2081_2100_acc_rcp85se)), col=cols2[1], border=NA)] # 95% CIs
		tsmpredsfut[oinds2, polygon(c(lat, rev(lat)), c(tsm_2081_2100_acc_rcp85-tsm_2081_2100_acc_rcp85se, rev(tsm_2081_2100_acc_rcp85+tsm_2081_2100_acc_rcp85se)), col=cols2[2], border=NA)]

		tsmpredsfut[linds2, lines(lat, tsm_2081_2100_acc_rcp85, col=cols[1], lwd=2, lty=2)] # mean prediction
		tsmpredsfut[oinds2, lines(lat, tsm_2081_2100_acc_rcp85, col=cols[2], lwd=2, lty=2)]

		# legend and subfigure label
		text(-76,ylimsC[2]-0.4, 'c', cex=1.5)

		legend(80,25, bty='n', lty=c('solid', 'solid', '22', NA, NA, NA, NA), lwd=c(2, 2, 2, NA, NA, NA, NA), pch=c(NA, NA, NA, 15, 17, 1, 4), col=c(cols, '#00000080', cols[1], cols[1], cols[2], cols[2]), legend=c('Terrestrial', 'Marine', 'Future', 'Terrestrial non-amphibian', 'Terrestrial amphibian', 'Marine sedentary', 'Marine mobile'), border=NA, cex=1, pt.cex=1, y.intersp=1.2, xpd=TRUE)

		

	# labels
	mtext('Temperature (°C)', side=2, line=3, las=3, adj=0.8, outer=TRUE)
	mtext('Thermal safety margin\nin favorable microclimates (°C)', side=2, line=2.5, las=3, adj=0.1, outer=TRUE, cex=1)
	mtext('Latitude (°N)', side=1, outer=TRUE, line=2, adj=0.25, cex=0.9)
	axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
	
	
	
	dev.off()


# sample sizes
	# part a
tsm[Realm=='Terrestrial' & !is.na(lat) & !is.na(tb_favorableGHCND95), .N] # Tb in protected microclimates on land
tsm[Realm=='Terrestrial' & !is.na(lat) & !is.na(tb_sunGHCND95), .N] # Tb in exposed microclimates on land
tsm[Realm=='Terrestrial' & !is.na(lat) & !is.na(tmax.accsum.elev), .N] # Tmax on land
	# part b
tsm[Realm=='Marine' & !is.na(lat) & !is.na(tb_favorableGHCND95), .N]
tsm[Realm=='Marine' & !is.na(lat) & !is.na(tb_sunGHCND95), .N]
tsm[Realm=='Marine' & !is.na(tb_favorableGHCND95) & !is.na(lat) & !is.na(tmax.accsum.elev), .N] 		
	# part c
tsm[linds1 & !is.na(lat) & !is.na(tsm_favorableGHCND95), .N]
tsm[oinds1 & !is.na(lat) & !is.na(tsm_favorableGHCND95), .N]

	
##############
## Fig. S1
## Tmax corrections
##############
dat <- fread('temp/warmingtolerance_byspecies.csv')

# plot
	setkey(dat, thabsum.adj)
	cex1 <- 0.6
	cex2 <- 0.4

	quartz(width=6.5, height=2.5)
	# pdf(width=6.5, height=2.5, file='figures/figS1.pdf')
	# png('figures/figS1.png', width=6.5, height=2.5, units='in', res=300)
	par(mfrow=c(1,3), las=1, mai=c(0.5,0.5,0.2, 0.1), mgp=c(2.5,0.8,0))
	dat[Realm=='Terrestrial',plot(1:.N, tmax_acc, col='grey', cex=cex1, main='', ylab='°C', xlab='Rank order', ylim=c(-10,60))]
	dat[Realm=='Terrestrial', points(1:.N, Tmax, col='blue', cex=cex2)]
	dat[Realm=='Terrestrial', points(1:.N, tmax.accsum.elev, col='purple', cex=cex2)]
	dat[Realm=='Terrestrial',points(1:.N, thabsum.adj, col='black', cex=cex2)]
	mtext(side=3, "a", adj=0)

	legend('bottomright', legend=c('Experimental acclimation temperature', 'Local acclimation temperature', 'Experimental Tmax', "Acclimated Tmax'"), col=c('grey', 'black', 'blue', 'purple'), pch=1, cex=0.6)

	dat[Realm=='Marine',plot(1:.N, tmax_acc, col='grey', cex=cex1, main='', ylab='°C', xlab='Rank order', ylim=c(-5,45))]
	dat[Realm=='Marine', points(1:.N, Tmax, col='blue', cex=cex2)]
	dat[Realm=='Marine', points(1:.N, tmax.accsum.elev, col='purple', cex=cex2)]
	dat[Realm=='Marine',points(1:.N, thabsum.adj, col='black', cex=cex2)]
	mtext(side=3, "b", adj=0)


	plot(-10,-10, xlim=range(dat$tmax.accsum.elev, na.rm=TRUE), ylim=range(dat$tmax.accsumbyspp.elev, na.rm=TRUE), main='', ylab=as.expression(bquote('T'['max, by species']*minute~'(°C)')), xlab=as.expression(bquote('T'[max]*minute~'(°C)')))
	abline(0,1, col='grey')	
	dat[,points(tmax.accsum.elev, tmax.accsumbyspp.elev, col='black', cex=cex1)]
	mtext(side=3, "c", adj=0)


	dev.off()


##############
## Fig. S2
## uncertainty in peak locations
## average TSM in latitude bands
##############
preds <- fread('temp/warmingtolerance_bylatlon.csv')
peaks <- list(readRDS('temp/peaks_modTbGHCND95_terr.rds'), readRDS('temp/peaks_modTmax_terr.rds'), readRDS('temp/peaks_modGHCND95_terr.rds'))
TSMbylat <- fread('temp/warmingtolerance_bylatband_land.csv')
	setkey(TSMbylat, latband)

# plotting parameters
# order: tb_favorable terr, tmax terr, tsm terr
rlm <- rep(c('Terrestrial'), 3)
ylims <- list(c(17,32), c(34,48), c(11,22))
xlims <- c(-50, 50)
vars <- list(quote(tb_favorableGHCND95), quote(tmax), quote(tsm_favorableGHCND95))
labs <- c('a', 'b', 'c')
xlabshist <- c('# peaks', '# peaks', '# valleys')
ylabs <- c('Tb (°C)', 'Tmax (°C)', 'TSM (°C)')

	quartz(width=4.8, height=4)
	# pdf(width=5.05, height=4, file='figures/figS2.pdf') # pdf is wider so that x-axis prints fully
	# png('figures/figS2.png', width=4.8, height=4, units='in', res=300)

	layout(matrix(c(1,3,4,6,2,3,5,6,7,9,10,10,8,9,10,10), nrow=4, byrow=TRUE), heights=c(1,5,1,5), widths=c(4,1,4,1))
	par(mai=c(0.5, 0.4, 0.05, 0.1), mgp=c(1.8,0.4,0), tcl=-0.2, las=1, omi=c(0,0,0,0), cex.axis=1, cex.lab=1.3)

	for(i in 1:length(peaks)){
		# top plot
		opar <- par(mai=c(0, 0.4, 0, 0.1)) # have to expand slightly so that x-axis lines up with middle plot
		hist(unlist(peaks[[i]]$peaklocs), breaks=seq(-85,85,by=5), col='grey', main='', xaxt='n', yaxt='n', ylab='', xlim=xlims)
		mtext(side=2, labs[i], adj=3, padj=0, las=1)
		par(opar)

		# middle plot
		with(preds[Realm==rlm[i],], plot(lat, eval(vars[[i]]), type='l', ylim=ylims[[i]], xlim=xlims, bty='l', xlab='Latitude (°N)', ylab=ylabs[i])) # plot the basic prediction for -50 to 50
 		for(j in 1:50) lines(preds[Realm==rlm[i], lat], peaks[[i]]$y[[j]], col=rgb(0,0,0,0.05), lty=1) # plot the uncertainty in lines (a sample)
# 		mtext('°C', line=1.7, side=2, cex=1, las=1, padj=0, outer=FALSE)

		# right plot
		opar <- par(mai=c(0.5, 0, 0.8, 0), cex.lab=1, cex.axis=0.7, mgp=c(1.6, 0.3, 0))
		hist(peaks[[i]]$npeaks, breaks=seq(0.5,4.5,by=1), col='grey', main='', yaxt='n', xlab=xlabshist[i])
		par(opar)
	}

	# TSM averages in latitude bands	
	TSMbylat[,plot(latband, mean, type='l', xlab='Latitude (°N)', ylab='TSM (°C)', bty='l')]
	TSMbylat[,arrows(latband, mean-se, latband, mean+se, length=0, angle=90, code=3)]
	mtext(side=2, 'd', adj=3.5, padj=-5, las=1)

	dev.off()

	# sample sizes
	TSMbylat[,sum(n)]

##############
## Fig. S3
## future thermal safety margin (TSM)
## TSM with acclimation
##############

# read in data
wt <- fread('temp/warmingtolerance_bylat_to2100.csv')
tsmpreds <- fread('temp/warmingtolerance_bylatlon.csv')
lstmaxhrfutlat <- fread('temp/lstmaxhrlat_2081-2100.csv')
sstmaxhrfutlat <- fread('temp/sstmaxhrlat_2081-2100.csv')


# set data to plot (where data were available)
	linds <- wt[,Realm=='Terrestrial' & lat > -50 & lat < 50] # indices for land
	oinds <- wt[,Realm=='Marine' & lat > -75 & lat < 75] # for ocean

	linds2 <- tsmpreds[, Realm=='Terrestrial' & lat > -50 & lat < 50] # indices for land (where data were available)
	oinds2 <- tsmpreds[, Realm=='Marine' & lat > -75 & lat < 75] # for ocean (where data were available)

# plot
	lw <- 2 # line width
	xlims <- c(-80, 80)
	ylims <- wt[linds | oinds, range(c(tsm_2081_2100_rcp85-tsm_2081_2100_rcp85se, tsm_2081_2100_acc_rcp26+tsm_2081_2100_acc_rcp26se), na.rm=TRUE)]


	quartz(width=6.5, height=2)
	# pdf(width=6.5, height=2, file='figures/figS3.pdf')
	# png('figures/figS3.png', width=6.5, height=2, units='in', res=300)

	par(mfrow=c(1,3), las=1, mai=c(0.45, 0.45, 0.2, 0.05), omi=c(0,0,0,1), mgp=c(1.8, 0.55, 0), cex.lab=1, cex.axis=0.8, tcl=-0.3)

	# Future temperatues
		i.1 <- complete.cases(lstmaxhrfutlat) & lstmaxhrfutlat$lat < -66.25 # for polygon plotting
		i.2 <- complete.cases(lstmaxhrfutlat) & lstmaxhrfutlat$lat > -52.75 # for polygon plotting
		i2 <- complete.cases(sstmaxhrfutlat)

		plot(0, 0, xlab='Latitude (°N)', ylab='Projected temperature (°C)', ylim=c(0,50), xlim=xlims, type='n') # just to set it up
			# rcp85
		with(lstmaxhrfutlat[i.2,], polygon(c(lat, rev(lat)), c(tasmax85+tasmax85sd, rev(tasmax85-tasmax85sd)), col=cols2[1], border=NA))
		with(sstmaxhrfutlat[i2,], polygon(c(lat, rev(lat)), c(tosmax85+tosmax85sd, rev(tosmax85-tosmax85sd)), col=cols2[2], border=NA))
		with(lstmaxhrfutlat[i.2,], lines(lat, tasmax85, col=cols[1], lwd=1))
		with(sstmaxhrfutlat, lines(lat, tosmax85, col=cols[2], lwd=1))

			#rcp26
		with(lstmaxhrfutlat[i.2,], lines(lat, tasmax26, col=cols[1], lwd=1, lty=2))
		with(sstmaxhrfutlat, lines(lat, tosmax26, col=cols[2], lwd=1, lty=2))

		axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
		mtext(side=3, "a", adj=0)

			# sample sizes
		with(lstmaxhrfutlat[i.2,], sum(tasmax85n))
		with(sstmaxhrfutlat, sum(tosmax85n))
		with(lstmaxhrfutlat[i.2,], sum(tasmax26n))
		with(sstmaxhrfutlat, sum(tosmax26n))
		

	# Future thermal safety margin
	plot(-100, 100, xlim=xlims, ylim=ylims, xlab='Latitude (°N)', ylab='Thermal safety margin (°C)')

		# rcp26
	wt[linds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_rcp26-tsm_2081_2100_rcp26se, rev(tsm_2081_2100_rcp26+tsm_2081_2100_rcp26se)), col=cols4[1], border=NA)] # 95% CIs
	wt[linds, lines(lat, tsm_2081_2100_rcp26, col=cols3[1], lwd=1, lty=2)]

	wt[oinds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_rcp26-tsm_2081_2100_rcp26se, rev(tsm_2081_2100_rcp26+tsm_2081_2100_rcp26se)), col=cols4[2], border=NA)] # 95% CIs
	wt[oinds, lines(lat, tsm_2081_2100_rcp26, col=cols3[2], lwd=1, lty=2)]

		# rcp86
	wt[linds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_rcp85-tsm_2081_2100_rcp85se, rev(tsm_2081_2100_rcp85+tsm_2081_2100_rcp85se)), col=cols2[1], border=NA)] # 95% CIs
	wt[linds, lines(lat, tsm_2081_2100_rcp85, col=cols[1], lwd=1)]

	wt[oinds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_rcp85-tsm_2081_2100_rcp85se, rev(tsm_2081_2100_rcp85+tsm_2081_2100_rcp85se)), col=cols2[2], border=NA)] # 95% CIs
	wt[oinds, lines(lat, tsm_2081_2100_rcp85, col=cols[2], lwd=1)]
	
	abline(h=0, col='grey', lty=2)
	axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
	mtext(side=3, "b", adj=0)


		
	# Future TSMs with acclimation
	plot(-100, 100, xlim=xlims, ylim=ylims, xlab='Latitude (°N)', ylab='Thermal safety margin (°C)')


		# rcp26
	wt[linds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_acc_rcp26-tsm_2081_2100_acc_rcp26se, rev(tsm_2081_2100_acc_rcp26+tsm_2081_2100_acc_rcp26se)), col=cols4[1], border=NA)] # 95% CIs
	wt[linds, lines(lat, tsm_2081_2100_acc_rcp26, col=cols3[1], lwd=1, lty=2)]

	wt[oinds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_acc_rcp26-tsm_2081_2100_acc_rcp26se, rev(tsm_2081_2100_acc_rcp26+tsm_2081_2100_acc_rcp26se)), col=cols4[2], border=NA)] # 95% CIs
	wt[oinds, lines(lat, tsm_2081_2100_acc_rcp26, col=cols3[2], lwd=1, lty=2)]

		# rcp86
	wt[linds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_acc_rcp85-tsm_2081_2100_acc_rcp85se, rev(tsm_2081_2100_acc_rcp85+tsm_2081_2100_acc_rcp85se)), col=cols2[1], border=NA)] # 95% CIs
	wt[linds, lines(lat, tsm_2081_2100_acc_rcp85, col=cols[1], lwd=1)]

	wt[oinds, polygon(c(lat, rev(lat)), c(tsm_2081_2100_acc_rcp85-tsm_2081_2100_acc_rcp85se, rev(tsm_2081_2100_acc_rcp85+tsm_2081_2100_acc_rcp85se)), col=cols2[2], border=NA)] # 95% CIs
	wt[oinds, lines(lat, tsm_2081_2100_acc_rcp85, col=cols[2], lwd=1)]
	
	abline(h=0, col='grey', lty=2)
	axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
	mtext(side=3, "c", adj=0)
	
	# legend
	legend(90, 18, legend=c('terrestrial', 'marine', 'RCP 8.5', 'RCP 2.6'), col=c(cols,'black', 'black'), lty=c(1,1,1,2), lwd=c(1,1,1,1), bty='n', cex=1, xpd=NA)



	dev.off()
	



##############
## Fig. S4
## TSM with annual, monthly, or hourly temperatures
## with 95% or max temperatures
## with or without marine behavioral thermoregulation
## in exposed microclimates
##############

# read in data
tsmpreds <- fread('temp/warmingtolerance_bylatlon.csv')
tsm <- fread('temp/warmingtolerance_byspecies.csv', stringsAsFactors=TRUE) # for calculating lats where we have data
	tsm[Realm=='Terrestrial', range(lat, na.rm=TRUE)]
	tsm[Realm=='Terrestrial' & !is.na(tsm_favorableGHCND95), range(lat, na.rm=TRUE)]
	tsm[Realm=='Terrestrial' & !is.na(tsm_ann), range(lat, na.rm=TRUE)]


# plot
	linds <- tsmpreds[, Realm=='Terrestrial' & lat > -72 & lat < 56] # indices for land (where data were available)
	oinds <- tsmpreds[, Realm=='Marine' & lat > -75 & lat < 75] # for ocean (where data were available)
#	ylimsAB <- tsmpreds[linds | oinds, range(tsm_ann, tsm_sum, tsm_maxmo, tsm_favorable_noacc, na.rm=TRUE)]
	ylimsAB <- c(8,40)
	xlims <- c(-75, 75)

	colsS3 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))(5)[2:5]
	colsS3_2 <- paste(colorRampPalette(brewer.pal(9, 'YlOrRd'))(5), '55', sep='')[2:5]

	quartz(width=4.5, height=5)
	# pdf(width=4.5, height=5, file='figures/figS4.pdf')
	# png('figures/figS4.png', width=4.5, height=5, units='in', res=300)
	par(mfrow=c(2,2), mai=c(0.7,0.3,0.2, 0.1), omi=c(0,0.3,0,0), las=1, mgp=c(2,0.5,0), tcl=-0.3)
	
	# A) land
		plot(-100, 100, xlim=xlims, ylim=ylimsAB, xlab='Latitude (°N)', ylab='')
		mtext('Thermal safety margin (°C)', side=2, outer=TRUE, adj=0.94, las=3, padj=-0.5, cex=0.8)
		
		# tsm_favorable maxhr GAMMs
		tsmpreds[linds, polygon(c(lat, rev(lat)), c(tsm_favorable_noacc-tsm_favorable_noaccse, rev(tsm_favorable_noacc+tsm_favorable_noaccse)), col=colsS3_2[1], border=NA)] # 95% CIs
		tsmpreds[linds, lines(lat, tsm_favorable_noacc, col=colsS3[1], lwd=2)] # mean prediction

		# tsm_maxmo GAMMs
		tsmpreds[linds, polygon(c(lat, rev(lat)), c(tsm_maxmo-tsm_maxmose, rev(tsm_maxmo+tsm_maxmose)), col=colsS3_2[2], border=NA)] # 95% CIs
		tsmpreds[linds, lines(lat, tsm_maxmo, col=colsS3[2], lwd=2)] # mean prediction

		# tsm_sum GAMMs
		tsmpreds[linds, polygon(c(lat, rev(lat)), c(tsm_sum-tsm_sumse, rev(tsm_sum+tsm_sumse)), col=colsS3_2[3], border=NA)] # 95% CIs
		tsmpreds[linds, lines(lat, tsm_sum, col=colsS3[3], lwd=2)] # mean prediction
		
		# tsm_ann GAMMs
		tsmpreds[linds, polygon(c(lat, rev(lat)), c(tsm_ann-tsm_annse, rev(tsm_ann+tsm_annse)), col=colsS3_2[4], border=NA)] # 95% CIs
		tsmpreds[linds, lines(lat, tsm_ann, col=colsS3[4], lwd=2)] # mean prediction

		mtext(side=3, "a", adj=0)
		axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)
		legend('top', legend=c('Average Year', 'Warmest Season', 'Warmest Month', 'Warmest Hour'), col=rev(colsS3_2), pch=15, lwd=1, border=NA, cex=0.5, bty='n', title='Terrestrial', title.col=cols[1])

	# B) ocean
		plot(-100, 100, xlim=xlims, ylim=ylimsAB, xlab='Latitude (°N)', ylab='')
		
		# tsm_favorable hour GAMMs
		tsmpreds[oinds, polygon(c(lat, rev(lat)), c(tsm_favorable_noacc-tsm_favorable_noaccse, rev(tsm_favorable_noacc+tsm_favorable_noaccse)), col=colsS3_2[1], border=NA)] # 95% CIs
		tsmpreds[oinds, lines(lat, tsm_favorable_noacc, col=colsS3[1], lwd=2)] # mean prediction

		# tsm_maxmo GAMMs
		tsmpreds[oinds, polygon(c(lat, rev(lat)), c(tsm_maxmo-tsm_maxmose, rev(tsm_maxmo+tsm_maxmose)), col=colsS3_2[2], border=NA)] # 95% CIs
		tsmpreds[oinds, lines(lat, tsm_maxmo, col=colsS3[2], lwd=2)] # mean prediction

		# tsm_sum GAMMs
		tsmpreds[oinds, polygon(c(lat, rev(lat)), c(tsm_sum-tsm_sumse, rev(tsm_sum+tsm_sumse)), col=colsS3_2[3], border=NA)] # 95% CIs
		tsmpreds[oinds, lines(lat, tsm_sum, col=colsS3[3], lwd=2)] # mean prediction

		# tsm_ann GAMMs
		tsmpreds[oinds, polygon(c(lat, rev(lat)), c(tsm_ann-tsm_annse, rev(tsm_ann+tsm_annse)), col=colsS3_2[4], border=NA)] # 95% CIs
		tsmpreds[oinds, lines(lat, tsm_ann, col=colsS3[4], lwd=2)] # mean prediction

		mtext(side=3, "b", adj=0)
		axis(1, at=seq(-80, 80, by=10), labels=FALSE, tcl=-0.2)

	legend('topleft', legend=c('Average Year', 'Warmest Season', 'Warmest Month', 'Warmest Hour'), col=rev(colsS3_2), pch=15, lwd=1, border=NA, cex=0.5, bty='n', title='Marine', title.col=cols[2])

	# C) Marine TSM with and without behavioral thermoregulation (no need to worry about GHCND since doesn't affect ocean)
		ylimsC <- c(7,22)

		oinds2 <- tsmpreds[, Realm=='Marine'] # for ocean (where data were available)

		plot(-100, -100, type='n', xlab='', ylab='', ylim=ylimsC, xlim=xlims) # set up plot
		mtext('Thermal safety margin (°C)', side=2, outer=TRUE, adj=0.2, las=3, padj=-0.5, cex=0.8)

		# Behavioral thermoregulation
		tsmpreds[oinds2, polygon(c(lat, rev(lat)), c(tsm_favorableGHCND95-tsm_favorableGHCND95se, rev(tsm_favorableGHCND95+tsm_favorableGHCND95se)), col=cols2[2], border=NA)]
		tsmpreds[oinds2, lines(lat, tsm_favorableGHCND95, col=cols[2], lwd=2)]

		# No behavioral thermoregulation (doesn't matter if this uses GHCND or not... plot here is for marine)
		tsmpreds[oinds2, polygon(c(lat, rev(lat)), c(tsm_favorableGHCND95_nomarineBT-tsm_favorableGHCND95_nomarineBTse, rev(tsm_favorableGHCND95_nomarineBT+tsm_favorableGHCND95_nomarineBTse)), col=cols2[2], border=NA)]
		tsmpreds[oinds2, lines(lat, tsm_favorableGHCND95_nomarineBT, col=cols[2], lwd=2, lty=2)]

		mtext(side=3, "c", adj=0)
		legend('topleft', bty='n', lty=c('solid', '22'), lwd=c(1, 1), col=c(cols[2]), legend=c('Thermoregulation', 'No thermoregulation'), border=NA, cex=0.5, pt.cex=1, y.intersp=1.2)


	# D) Terrestrial TSM in exposed habitats or favorable habitats
		ylimsC <- c(-15,20)

		linds2 <- tsmpreds[, Realm=='Terrestrial' & lat > -50 & lat < 50] # indices for land (where data were available)

		plot(-100, -100, type='n', xlab='', ylab='', ylim=ylimsC, xlim=xlims) # set up plot

		# favorable
		tsmpreds[linds2, polygon(c(lat, rev(lat)), c(tsm_favorableGHCND95-tsm_favorableGHCND95se, rev(tsm_favorableGHCND95+tsm_favorableGHCND95se)), col=cols2[1], border=NA)] # 95% CIs
		tsmpreds[linds2, lines(lat, tsm_favorableGHCND95, col=cols[1], lwd=2)] # mean prediction

		# exposed
		tsmpreds[linds2, polygon(c(lat, rev(lat)), c(tsm_exposedGHCND95-tsm_exposedGHCND95se, rev(tsm_exposedGHCND95+tsm_exposedGHCND95se)), col=cols2[1], border=NA)] # 95% CIs
		tsmpreds[linds2, lines(lat, tsm_exposedGHCND95, col=cols[1], lwd=2, lty=2)] # mean prediction

		abline(h=0, lty=2, col='grey')

		mtext(side=3, "d", adj=0)
		legend(20,12, bty='n', lty=c('solid', '22'), lwd=c(1, 1), col=cols[1], legend=c('Shade', 'Full sun'), border=NA, cex=0.5, pt.cex=1, y.intersp=1.2)


	dev.off()
	

################
## Fig. S5
## Topt vs. Tmax
################
microbe_data <- read.csv("data/topt_tmax/Chen_2015_fbv009supp.csv")
insect_data <- read.csv("data/topt_tmax/Deutsch_thermal_curves.csv")
lizard_data <- read.csv("data/topt_tmax/Huey_2009_supp_data.csv")
alldata <- data.frame(topt=c(microbe_data$t_opt, insect_data$Topt, lizard_data$newTopt), 
                    tmax=c(microbe_data$t_max, insect_data$CTmax, lizard_data$CTmax), 
                    group=c(rep("microbes", nrow(microbe_data)), rep("insects", nrow(insect_data)), rep("lizards", nrow(lizard_data))))
	sum(!is.na(alldata$topt) & !is.na(alldata$tmax))

preds <- read.csv('temp/topt_tmax_fit.csv')

# plot with model
	quartz(height=4, width=4)
	# pdf(height=4, width=4, file='figures/figS5.pdf')
	# png('figures/figS5.png', width=4, height=4, units='in', res=300)

	par(mai=c(1,1,0.1, 0.1), las=1)
	plot(tmax ~ topt, data=alldata, col='#00000055', xlab=as.expression(bquote('T'['opt'])), ylab=as.expression(bquote('T'['max'])))
	abline(0,1, col='grey', lty=2)

	with(preds, polygon(c(topt, rev(topt)), c(tmax-tmax.se, rev(tmax+tmax.se)), col='#00000055', border=NA))
	with(preds, lines(topt, tmax, col='#000000'))


	dev.off()
	
###########################
# Table S1
# data by Class
###########################
dat <- fread('temp/warmingtolerance_byspecies.csv', stringsAsFactors=TRUE)

tab <- as.data.frame.matrix(table(dat$Class, dat$Realm))
tab$Class <- row.names(tab)

tab <- merge(tab, unique(dat[,.(Phylum, Class)])) # add Phylum

sum(tab$Marine) # 88
sum(tab$Terrestrial) # 318

# reformat
#tab <- tab[order(tab$Phylum, tab$Class) ,c('Phylum', 'Class', 'Terrestrial', 'Marine')]
tab1 <- tab[tab$Terrestrial>0, c('Phylum', 'Class', 'Terrestrial')]
tab2 <- tab[tab$Marine>0, c('Phylum', 'Class', 'Marine')]
names(tab1)[3] <- 'Species with Tmax'
names(tab2)[3] <- 'Species with Tmax'
tab1$Realm <- 'Terrestrial'
tab2$Realm <- 'Marine'
tab1 <- tab1[order(tab1$Phylum, tab1$Class), ]
tab2 <- tab2[order(tab2$Phylum, tab2$Class), ]

out <- rbind(tab1, tab2)
out <- out[,c('Realm', 'Phylum', 'Class', 'Species with Tmax')]

#examine
out

# write out
write.csv(out, file='tables/TableS1.csv', row.names=FALSE)


###########################
# Supplementary Dataset 1
# Tmax data
###########################
dat <- fread('temp/warmingtolerance_byspecies.csv', stringsAsFactors=TRUE)

out <- dat[, .(Genus, Species, Realm, lat, lon, altitude, Tmax, tmax_metric, tmax_acc, citation, Phylum, Class, Order, Family, demers_pelag, mobility, length, weight)]

setkey(out, Realm, Genus, Species, citation)

out

write.csv(out, quote=FALSE, na='', file='tables/datasetS1.csv', row.names=FALSE)


###########################
# Supplementary Dataset 2
# Extirpation
###########################
dat <- fread('temp/warmingtolerance_byspecies.csv', stringsAsFactors=TRUE)

out <- dat[, .(Genus, Species, Realm, lat, lon, altitude, Tmax, tmax_metric, tmax_acc, citation, Phylum, Class, Order, Family, demers_pelag, mobility, length, weight)]

setkey(out, Realm, Genus, Species, citation)

out

write.csv(out, quote=FALSE, na='', file='tables/datasetS1.csv', row.names=FALSE)