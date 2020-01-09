# seasonality and spatial gradients on land and ocean (SD of monthly means)

library(raster)
library(reshape2)
library(RColorBrewer)
library(maps)

source('analysis/image.scale.r') # for plotting color bar on image

load('data/udel/udel1986_2005.rdata') # tempsarray lst 0.5x0.5°
	lstarray <- tempsarray
#load('data/ersstv4/ersst1986_2005.rdata') # tempsarray sst
load('data/oisst/oisst0.5deg_1986_2005.rdata') # tempsarray sst 0.5x0.5°
	sstarray <- tempsarray
	rm(tempsarray)


lstar <- lstarray
sstar <- sstarray

# calculate mean annual climatology (for comparison against variability)
	lstclim <- apply(lstar, MARGIN=c(1,2), FUN=mean) # mean through time
	sstclim <- apply(sstar, MARGIN=c(1,2), FUN=mean)

	# reformat sst to start at date line (-180 to 180) to match lstclim
	sstlons <- as.numeric(colnames(sstclim))
	sstclim <- sstclim[, c(which(sstlons > 180), which(sstlons < 180))]
	newlons <- sstlons[c(which(sstlons > 180), which(sstlons < 180))]
	newlons[newlons >180] <- newlons[newlons >180] - 360
	colnames(sstclim) <- newlons

# analyze seasonal variability
	lstseason <- apply(lstar, MARGIN=c(1,2), FUN=sd) # SD through time
	sstseason <- apply(sstar, MARGIN=c(1,2), FUN=sd)

		# reformat sst to start at date line (-180 to 180), to match lst
		lons <- as.numeric(colnames(sstseason))
		neworder <- c(lons[lons>180], lons[lons<180])
		sstseason_reformat <- sstseason[,as.character(neworder)]
		lons2 <- neworder
		lons2[lons2>180] <- lons2[lons2>180] - 360
		colnames(sstseason_reformat) <- lons2

		# plot spatial patterns on one map
		cols <- colorRampPalette(brewer.pal(9, "OrRd"))(100)
		zlims <- range(c(lstseason, sstseason), na.rm=TRUE)
		image(t(sstseason_reformat), col=cols, zlim=zlims, main='Seasonality (°C)')
		image(t(lstseason), col=cols, zlim=zlims, add=TRUE)
		
		#copy to original
		sstseason <- sstseason_reformat
		
	mean(lstseason, na.rm=TRUE) # 9.01 °C
	mean(sstseason, na.rm=TRUE) # 1.48 °C
	mean(lstseason, na.rm=TRUE)/mean(sstseason, na.rm=TRUE) # 6.1x

	# write out arrays
	save(lstseason, file='temp/lstseason.rdata')
	save(sstseason, file='temp/sstseason.rdata')

	# lat means	
	lstseasonlat <- apply(lstseason, MARGIN=1, FUN=mean, na.rm=TRUE) # mean within lats
	sstseasonlat <- apply(sstseason, MARGIN=1, FUN=mean, na.rm=TRUE)

	lstseasonlatsd <- apply(lstseason, MARGIN=1, FUN=sd, na.rm=TRUE) # sd within lat bands
	sstseasonlatsd <- apply(sstseason, MARGIN=1, FUN=sd, na.rm=TRUE)

	lstlats <- as.numeric(names(lstseasonlat))
	sstlats <- as.numeric(names(sstseasonlat))
	
	# examine lat gradients
	i <- !is.na(lstseasonlat)
	lstseasonlat2 <- lstseasonlat[i]
	lstseasonlatsd2 <- lstseasonlatsd[i]
	lstseasonlatsd2[is.na(lstseasonlatsd2)] <- 0
	lstlats2 <- lstlats[i]

	i <- !is.na(sstseasonlat)
	sstseasonlat2 <- sstseasonlat[i]
	sstseasonlatsd2 <- sstseasonlatsd[i]
	sstseasonlatsd2[is.na(sstseasonlatsd2)] <- 0
	sstlats2 <- sstlats[i]

	plot(-200,0, xlim=c(-90,90), ylim=c(-2,18), xlab='lat', ylab='Seasonality (°C)')
	polygon(c(lstlats2, rev(lstlats2)), c(lstseasonlat2 - lstseasonlatsd2, rev(lstseasonlat2 + lstseasonlatsd2)), col='light green', border=NA)
	lines(lstlats2, lstseasonlat2, col='dark green')
	polygon(c(sstlats2, rev(sstlats2)), c(sstseasonlat2 - sstseasonlatsd2, rev(sstseasonlat2 + sstseasonlatsd2)), col='light blue', border=NA)
	lines(sstlats2, sstseasonlat2, col='dark blue')

	
	# write out lat means
	write.csv(data.frame(lat=lstlats, season=as.numeric(lstseasonlat), sd=lstseasonlatsd), file='temp/lstseasonlat.csv', row.names=FALSE)
	write.csv(data.frame(lat=sstlats, season=as.numeric(sstseasonlat), sd=sstseasonlatsd), file='temp/sstseasonlat.csv', row.names=FALSE)



# analyze seasonal ranges
	# function to calculate average annual range
	aveannrng <- function(x, yrs){
		annrngs <- aggregate(list(rng=x), by=list(year=yrs), FUN= function(x) diff(range(x))) # calc range for each year
		return(mean(annrngs$rng)) # return average annual range
	}

	yrs <- as.numeric(gsub('_[[:digit:]]+', '', gsub('X', '', dimnames(lstar)[[3]]))) # extract year from array names
	lstrng <- apply(lstar, MARGIN=c(1,2), FUN=aveannrng, yrs=yrs) # average annual range
	yrs <- as.numeric(gsub('_[[:digit:]]+', '', gsub('X', '', dimnames(sstar)[[3]])))
	sstrng <- apply(sstar, MARGIN=c(1,2), FUN=aveannrng, yrs=yrs)

		# image(t(lstrng), col=rainbow(100))
		# image(t(sstrng), col=rainbow(100))

	# reformat sst to start at date line (-180 to 180), to match lst
	lons <- as.numeric(colnames(sstrng))
	neworder <- c(lons[lons>180], lons[lons<180])
	sstrng_reformat <- sstrng[,as.character(neworder)]
	lons2 <- neworder
	lons2[lons2>180] <- lons2[lons2>180] - 360
	colnames(sstrng_reformat) <- lons2

		# plot spatial patterns
		cols <- colorRampPalette(brewer.pal(9, "OrRd"))(100)
		zlims <- range(c(lstrng, sstrng), na.rm=TRUE)
		image(t(sstrng_reformat), col=cols, zlim=zlims, main='Range (°C)')
		image(t(lstrng), col=cols, zlim=zlims, add=TRUE)
		
		#copy to original
		sstrng <- sstrng_reformat


	mean(lstrng, na.rm=TRUE) # 26.2
	mean(sstrng, na.rm=TRUE) # 4.14
	mean(lstrng, na.rm=TRUE)/mean(sstrng, na.rm=TRUE) # 6.3

	# write out arrays
	save(lstrng, file='temp/lstrng.rdata')
	save(sstrng, file='temp/sstrng.rdata')

	# mean within lats
	lstrnglat <- apply(lstrng, MARGIN=1, FUN=mean, na.rm=TRUE) 
	sstrnglat <- apply(sstrng, MARGIN=1, FUN=mean, na.rm=TRUE)
	
	# examine
	plot(as.numeric(names(lstrnglat)), lstrnglat, type='l', col='green', xlab='lat', ylab='range', ylim=c(0,50))
	lines(as.numeric(names(sstrnglat)), sstrnglat, col='blue')
	
	# write out
	write.csv(data.frame(lat=as.numeric(names(lstrnglat)), range=as.numeric(lstrnglat)), file='temp/lstrnglat.csv', row.names=FALSE)
	write.csv(data.frame(lat=as.numeric(names(sstrnglat)), range=as.numeric(sstrnglat)), file='temp/sstrnglat.csv', row.names=FALSE)
	
	
	
# spatial gradients
	lstclima <- apply(lstar, MARGIN=c(1,2), FUN=mean) # average over time
	lstclimr <- raster(lstclima[nrow(lstclima):1,], xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+proj=longlat +datum=WGS84') # make a raster from the climatology
	lstterr <- terrain(lstclimr, opt='slope', unit='tangent', neighbors=8) # returns °C/m
	lstgrad <- as.matrix(lstterr)

	sstclima <- apply(sstar, MARGIN=c(1,2), FUN=mean) # average over time
	sstclimr <- raster(sstclima[nrow(sstclima):1,], xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+proj=longlat +datum=WGS84') # make a raster from the climatology
	sstterr <- terrain(sstclimr, opt='slope', unit='tangent', neighbors=8) # returns °C/m
	sstgrad <- as.matrix(sstterr)

		# plot
		cols <- colorRampPalette(brewer.pal(9, "OrRd"))(100)
		image(lstgrad, col=cols)
		image(sstgrad, col=cols)

	# latitudinal means
	lstgradlat <- apply(lstgrad, MARGIN=1, FUN=mean, na.rm=TRUE)
	lstgradlatsd <- apply(lstgrad, MARGIN=1, FUN=sd, na.rm=TRUE)
	
	sstgradlat <- apply(sstgrad, MARGIN=1, FUN=mean, na.rm=TRUE)
	sstgradlatsd <- apply(sstgrad, MARGIN=1, FUN=sd, na.rm=TRUE)

	lats <- yFromRow(lstterr) # same for sst and lst, so this works
	lstgradlat <- data.frame(lat=lats, grad=lstgradlat*1000, sd=lstgradlatsd*1000) # convert so units of °C/km
	sstgradlat <- data.frame(lat=lats, grad=sstgradlat*1000, sd=sstgradlatsd*1000)
	
	# plot
	i <- complete.cases(lstgradlat)
	i2 <- complete.cases(sstgradlat)

	quartz(w=4,h=4)
	plot(-200,0, xlim=c(-90,90), ylim=c(-0.03,0.14), xlab='lat', ylab='Gradients (°C)')
	with(lstgradlat[i,], polygon(c(lat, rev(lat)), c(grad - sd, rev(grad + sd)), col='light green', border=NA))
	with(lstgradlat[i,], lines(lat, grad, col='dark green'))
	with(sstgradlat[i2,], polygon(c(lat, rev(lat)), c(grad - sd, rev(grad + sd)), col='light blue', border=NA))
	with(sstgradlat[i2,], lines(lat, grad, col='dark blue'))
	
	# write out
	write.csv(lstgradlat, file='temp/lstgradlat.csv', row.names=FALSE)
	write.csv(sstgradlat, file='temp/sstgradlat.csv', row.names=FALSE)



### Plots
	# Climatology map (for comparison)	
	zlims <- range(c(lstclim, sstclim), na.rm=TRUE)
	bks <- seq(zlims[1], zlims[2],length.out=101)
	cols <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks)-1)

	quartz(width=5, height=5)
	# png('figures/climatology_mar_vs_terr_map.png', width=5, height=5, units='in', res=300)
	layout(matrix(c(1,2), nrow=2, ncol=1), heights=c(4,1))

	par(mar=c(3,3,1,1), las=1, mgp=c(1.6,0.5,0), tck=-0.015, cex.axis=0.8)
	image(y=seq(-89.75, 89.75, by=0.5), x=seq(-179.75, 179.75, by=0.5), z=t(lstclim), col=cols, breaks=bks, main='', xlab='Longitude', ylab='Latitude')
	image(y=seq(-89.75, 89.75, by=0.5), x=seq(-179.75, 179.75, by=0.5), z=t(sstclim), col=cols, zlim=zlims, add=TRUE)
	map(add=TRUE, interior=FALSE, col='dark grey')
		
		# scale bar
	par(mar=c(3,1,1,1), mgp=c(1.4, 0.1, 0))
	image.scale(zlim=zlims, col=cols, breaks=bks, horiz=TRUE, xlab='Mean annual temperature (°C)')

	dev.off()
	

	# Seasonality map
	zlims <- range(c(lstseason, sstseason), na.rm=TRUE)
	bks <- seq(zlims[1], zlims[2],length.out=101)
	cols <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks)-1)

	quartz(width=5, height=5)
	# png('figures/seasonality_mar_vs_terr_map.png', width=5, height=5, units='in', res=300)
	layout(matrix(c(1,2), nrow=2, ncol=1), heights=c(4,1))

	par(mar=c(3,3,1,1), las=1, mgp=c(1.6,0.5,0), tck=-0.015, cex.axis=0.8)
	image(y=seq(-89.75, 89.75, by=0.5), x=seq(-179.75, 179.75, by=0.5), z=t(lstseason), col=cols, breaks=bks, main='', xlab='Longitude', ylab='Latitude')
	image(y=seq(-89.75, 89.75, by=0.5), x=seq(-179.75, 179.75, by=0.5), z=t(sstseason), col=cols, zlim=zlims, add=TRUE)
	map(add=TRUE, interior=FALSE, col='dark grey')
		
		# scale bar
	par(mar=c(3,1,1,1), mgp=c(1.4, 0.1, 0))
	image.scale(zlim=zlims, col=cols, breaks=bks, horiz=TRUE, xlab='Seasonality (°C)')

	dev.off()

	# Seasonality by latitude
	cols <- c('#b2df8a', '#1f78b4')
	lw <- 3 # line width
	quartz(width=4,height=4)
	# pdf('analysis/figures/seasonality_mar_vs_terr.pdf', width=3, height=3)
	par(mfrow=c(1,1), mai=c(0.6, 0.6, 0.1, 0.1), mgp=c(2,0.4,0), las=1, tcl=-0.2)
	with(lstseasonlat, plot(lat, season, col=cols[1], ylab='Seasonality (°C)', xlab='Latitude (°)', lwd=lw, type='l', ylim=c(0,14), xlim=c(-90,90)))
	with(sstseasonlat, lines(lat, season, col=cols[2], lwd=lw))

	legend('topleft', legend=c('terrestrial', 'marine'), col=cols, lty=c(1,1), bty='n', cex=0.7)

	dev.off()
	
	# Range map
	zlims <- range(c(lstrng, sstrng), na.rm=TRUE)
	bks <- seq(zlims[1], zlims[2],length.out=101)
	cols <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks)-1)

	quartz(width=5, height=5)
	# png('figures/range_mar_vs_terr_map.png', width=5, height=5, units='in', res=300)
	layout(matrix(c(1,2), nrow=2, ncol=1), heights=c(4,1))

	par(mar=c(3,3,1,1), las=1, mgp=c(1.6,0.5,0), tck=-0.015, cex.axis=0.8)
	image(y=seq(-89.75, 89.75, by=0.5), x=seq(-179.75, 179.75, by=0.5), z=t(sstrng), col=cols, breaks=bks, main='', xlab='Longitude', ylab='Latitude')
	image(y=seq(-89.75, 89.75, by=0.5), x=seq(-179.75, 179.75, by=0.5), z=t(lstrng), col=cols, zlim=zlims, add=TRUE)
	map(add=TRUE, interior=FALSE, col='dark grey')
			
		# scale bar
	par(mar=c(3,1,1,1), mgp=c(1.4, 0.1, 0))
	image.scale(zlim=zlims, col=cols, breaks=bks, horiz=TRUE, xlab='Range (°C)')

	dev.off()


	# Spatial gradients by latitude
	cols <- c('#b2df8a', '#1f78b4')
	lw <- 3 # line width
	quartz(width=4,height=4)
	# pdf('figures/spatialgrads_mar_vs_terr.pdf', width=3, height=3)
	par(mfrow=c(1,1), mai=c(0.6, 0.6, 0.1, 0.1), mgp=c(2,0.4,0), las=1, tcl=-0.2)
	with(lstgradlat, plot(lat, grad*1000, col=cols[1], xlab='Latitude (°)', ylab='Spatial gradients (°C/km)', lwd=lw, type='l', ylim=c(0,0.06), xlim=c(-90,90)))
	with(sstgradlat, lines(lat, grad*1000, col=cols[2], lwd=lw))

	legend('topleft', legend=c('terrestrial', 'marine'), col=cols, lty=c(1,1), bty='n', cex=0.7)

	dev.off()
	
	
	# Seasonality vs. mean annual
		# option 1: two plots with longitude
	colramp <- colorRamp(brewer.pal(9, "OrRd"))
	
		# matrix of longitudes for plotting
	longrd <- matrix(as.numeric(colnames(lstclim)), nrow=1)
	longrd <- longrd[rep(1, nrow(lstclim)),]
	longrd.sc <- (longrd - min(longrd))/diff(range(longrd))

	quartz(width=10, height=5)
	# jpeg('figures/seasonality_vs_mean_annual_mar_and_terr_withlon.jpg', width=5, height=5, units='in', res=300)

	par(mfrow=c(1,2), mai=c(0.7,0.7,0.5, 0.1), las=1, mgp=c(2,0.7,0), cex.axis=0.8)

	plot(sstclim, sstseason, xlab='Mean annual temperature (°C)', ylab='Seasonality (°C)', main='Sea surface', col=rgb(colramp(longrd.sc), maxColorValue=255), pch=16, cex=0.5)

	legend('topleft', pch=16, col=rgb(colramp(c(0,0.5,1)), maxColorValue=255), legend=c('-180°', '0°', '180°'), title='Longitude')

	plot(lstclim, lstseason, xlab='Mean annual temperature (°C)', ylab='Seasonality (°C)', main='Land surface', col=rgb(colramp(longrd.sc), maxColorValue=255), pch=16, cex=0.5)

	dev.off()

		# option 2: two plots with latgitude
	colramp <- colorRamp(brewer.pal(9, "OrRd"))
	
		# matrix of latitudes for plotting
	latgrd <- matrix(as.numeric(rownames(lstclim)), ncol=1)
	latgrd <- latgrd[,rep(1, ncol(lstclim))]
	latgrd.sc <- (latgrd - min(latgrd))/diff(range(latgrd))

	quartz(width=10, height=5)
	# jpeg('figures/seasonality_vs_mean_annual_mar_and_terr_withlat.jpg', width=10, height=5, units='in', res=300)

	par(mfrow=c(1,2), mai=c(0.7,0.7,0.5, 0.1), las=1, mgp=c(2,0.7,0), cex.axis=0.8)

	plot(sstclim, sstseason, xlab='Mean annual temperature (°C)', ylab='Seasonality (°C)', main='Sea surface', col=rgb(colramp(latgrd.sc), maxColorValue=255), pch=16, cex=0.5)

	legend('topleft', pch=16, col=rgb(colramp(c(0,0.5,1)), maxColorValue=255), legend=c('-90°', '0°', '90°'), title='Latitude')

	plot(lstclim, lstseason, xlab='Mean annual temperature (°C)', ylab='Seasonality (°C)', main='Land surface', col=rgb(colramp(latgrd.sc), maxColorValue=255), pch=16, cex=0.5)

	dev.off()

	
		# option 2: on one plot
	cols <- c('#b2df8a11', '#1f78b40b') # land, ocean

	quartz(width=5, height=5)
	# jpeg('figures/seasonality_vs_mean_annual_mar_and_terr.jpg', width=5, height=5, units='in', res=300)
	# pdf('figures/seasonality_vs_mean_annual_mar_and_terr.pdf', width=5, height=5)
	par(mai=c(0.7,0.7,0.5, 0.1), las=1, mgp=c(2,0.7,0), cex.axis=0.8, tcl=-0.2)

	plot(lstclim, lstseason, xlab='Mean annual temperature (°C)', ylab='Seasonality (°C)', main='', col=cols[1], pch=16, cex=0.5)
	
	points(sstclim, sstseason, col=cols[2], pch=16, cex=0.5)
	
	legend('topright', pch=16, col=c('#b2df8a', '#1f78b4'), legend=c('land surface', 'sea surface'))
	
	dev.off()