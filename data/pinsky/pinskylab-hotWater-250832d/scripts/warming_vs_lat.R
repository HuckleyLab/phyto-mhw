## Analyze warming predicted in CMIP5
## Now uses temp just above surface (tas) for land, sst for ocean

require(ncdf4)
require(RColorBrewer)
require(maps)

#######################
## Read in data
#######################

# read in climatologies for annual mean
load('temp/lstclimatology_newgrid.rdata') # lstclimn
load('temp/sstclimatology_newgrid.rdata') # sstclimn

# read in climatologies for summer mean
load('temp/lstclimatology_warm3_newgrid.rdata') # lstclimwarm3n
load('temp/sstclimatology_warm3_newgrid.rdata') # sstclimwarm3n

# read in warming rcp26
infile <- nc_open('data/cmip5/d20161206_1632_19653.nc') # deltas for each month, 2081-2100 minus 1986-2005
	lstwarm26 <- ncvar_get(infile, 'diff') # lst warming  x  x 12 (last is months)
	dim(lstwarm26)
	dimnames(lstwarm26) <- list(lon=1:144, lat=1:72, mo3=1:12)
	dimnames(lstwarm26)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(lstwarm26)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	lstwarm26 <- lstwarm26[,dim(lstwarm26)[2]:1,]

infile <- nc_open('data/cmip5/d20161206_1630_11885.nc') # deltas for each month, 2081-2100 minus 1986-2005
	sstwarm26 <- ncvar_get(infile, 'diff') # sst warming 288 x 144 x 12 (last is months)
	dim(sstwarm26)
	dimnames(sstwarm26) <- list(lon=1:288, lat=1:144, mo3=1:12) # dummy dimnames for lat and lon
	dimnames(sstwarm26)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(sstwarm26)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	sstwarm26 <- sstwarm26[,dim(sstwarm26)[2]:1,] # rearrange


# read in warming rcp85
infile <- nc_open('data/cmip5/d20161127_1456_12434.nc') # deltas for each month, 2081-2100 minus 1986-2005
	lstwarm85 <- ncvar_get(infile, 'diff') # lst warming  x  x 12 (last is months)
	dim(lstwarm85)
	dimnames(lstwarm85) <- list(lon=1:144, lat=1:72, mo3=1:12)
	dimnames(lstwarm85)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(lstwarm85)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	lstwarm85 <- lstwarm85[,dim(lstwarm85)[2]:1,]

infile <- nc_open('data/cmip5/d20161127_1011_30742.nc') # deltas for each month, 2081-2100 minus 1986-2005
	sstwarm85 <- ncvar_get(infile, 'diff') # sst warming 288 x 144 x 12 (last is months)
	dim(sstwarm85)
	dimnames(sstwarm85) <- list(lon=1:288, lat=1:144, mo3=1:12) # dummy dimnames for lat and lon
	dimnames(sstwarm85)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(sstwarm85)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	sstwarm85 <- sstwarm85[,dim(sstwarm85)[2]:1,] # rearrange


#######################
# calculate warming 
#######################

# for annual mean
	lstwarm26ann <- t(apply(lstwarm26, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))
	lstwarm85ann <- t(apply(lstwarm85, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))
	sstwarm26ann <- t(apply(sstwarm26, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))
	sstwarm85ann <- t(apply(sstwarm85, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))


# for summer
	# lst
	lstwarm26sumN <- apply(lstwarm26[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	lstwarm26sumS <- apply(lstwarm26[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	lstwarm26sum <- t(cbind(lstwarm26sumN[,as.character(seq(88.75,1.25,by=-2.5))], lstwarm26sumS[,as.character(seq(-1.25,-88.75,by=-2.5))]))

	lstwarm85sumN <- apply(lstwarm85[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	lstwarm85sumS <- apply(lstwarm85[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	lstwarm85sum <- t(cbind(lstwarm85sumN[,as.character(seq(88.75,1.25,by=-2.5))], lstwarm85sumS[,as.character(seq(-1.25,-88.75,by=-2.5))]))


	# sst
	sstwarm26sumN <- apply(sstwarm26[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	sstwarm26sumS <- apply(sstwarm26[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	sstwarm26sum <- t(cbind(sstwarm26sumN[,as.character(seq(89.375,0.625,by=-1.25))], sstwarm26sumS[,as.character(seq(-0.625,-89.375,by=-1.25))]))

	sstwarm85sumN <- apply(sstwarm85[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	sstwarm85sumS <- apply(sstwarm85[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	sstwarm85sum <- t(cbind(sstwarm85sumN[,as.character(seq(89.375,0.625,by=-1.25))], sstwarm85sumS[,as.character(seq(-0.625,-89.375,by=-1.25))]))

	# clean up
	rm(sstwarm26sumN, sstwarm26sumS, lstwarm26sumN, lstwarm26sumS, sstwarm85sumN, sstwarm85sumS, lstwarm85sumN, lstwarm85sumS)

	
# trim surface air temps to only land
	lstwarm26ann[is.na(lstclimn)] <- NA
	lstwarm85ann[is.na(lstclimn)] <- NA
	lstwarm26sum[is.na(lstclimn)] <- NA
	lstwarm85sum[is.na(lstclimn)] <- NA


#############
# Plot maps
#############

# plot the future temperature maps (rcp85 annual)
	# reshape to match maps (-180 to 180 longitudes)
	lons <- as.numeric(colnames(lstwarm85ann))
	lsttoplot <- cbind(lstwarm85ann[,lons>180], lstwarm85ann[,lons<180])
	lons2 <- as.numeric(colnames(lsttoplot)); lons2[lons2>180] <- lons2[lons2>180] - 360
	colnames(lsttoplot) <- lons2

	lons <- as.numeric(colnames(sstwarm85ann))
	ssttoplot <- cbind(sstwarm85ann[,lons>180], sstwarm85ann[,lons<180])
	lons2 <- as.numeric(colnames(ssttoplot)); lons2[lons2>180] <- lons2[lons2>180] - 360
	colnames(ssttoplot) <- lons2

	# plot
	quartz(width=5, height=7)
	# pdf(width=5, height=7, file='figures/warmingmap_land_vs_ocean.pdf')
	par(mfrow=c(2,1))
	image(t(lsttoplot)[,nrow(lsttoplot):1], col=rev(heat.colors(20)), x=as.numeric(colnames(lsttoplot)), y=as.numeric(rev(rownames(lsttoplot))), xlab='Longitude', ylab='Latitude', main='Surface air temperature warming')
		map(add=TRUE, col='grey', interior=FALSE)
	image(t(ssttoplot)[,nrow(ssttoplot):1], col=rev(heat.colors(20)), x=as.numeric(colnames(ssttoplot)), y=as.numeric(rev(rownames(ssttoplot))), xlab='Longitude', ylab='Latitude', main='Surface sea temperature warming')
		map(add=TRUE, col='grey', interior=FALSE)
		
	dev.off()


# plot the future temperature maps (rcp85 summer)
	# reshape to match maps (-180 to 180 longitudes)
	lons <- as.numeric(colnames(lstwarm85sum))
	lsttoplot <- cbind(lstwarm85sum[,lons>180], lstwarm85sum[,lons<180])
	lons2 <- as.numeric(colnames(lsttoplot)); lons2[lons2>180] <- lons2[lons2>180] - 360
	colnames(lsttoplot) <- lons2

	lons <- as.numeric(colnames(sstwarm85sum))
	ssttoplot <- cbind(sstwarm85sum[,lons>180], sstwarm85sum[,lons<180])
	lons2 <- as.numeric(colnames(ssttoplot)); lons2[lons2>180] <- lons2[lons2>180] - 360
	colnames(ssttoplot) <- lons2

	# plot
	quartz(width=5, height=7)
	# pdf(width=5, height=7, file='figures/warmingmap_land_vs_ocean.pdf')
	par(mfrow=c(2,1))
	image(t(lsttoplot)[,nrow(lsttoplot):1], col=rev(heat.colors(20)), x=as.numeric(colnames(lsttoplot)), y=as.numeric(rev(rownames(lsttoplot))), xlab='Longitude', ylab='Latitude', main='Surface air temperature warming')
		map(add=TRUE, col='grey', interior=FALSE)
	image(t(ssttoplot)[,nrow(ssttoplot):1], col=rev(heat.colors(20)), x=as.numeric(colnames(ssttoplot)), y=as.numeric(rev(rownames(ssttoplot))), xlab='Longitude', ylab='Latitude', main='Surface sea temperature warming')
		map(add=TRUE, col='grey', interior=FALSE)
		
	dev.off()

##################
# lat averages
##################

	# means and SDs
	lstwarm26annbylat <- apply(lstwarm26ann, MARGIN=1, FUN=mean, na.rm=TRUE) # mean warming
	sstwarm26annbylat <- apply(sstwarm26ann, MARGIN=1, FUN=mean, na.rm=TRUE)
	lstwarm26annbylatsd <- apply(lstwarm26ann, MARGIN=1, FUN=sd, na.rm=TRUE) # SD warming
	sstwarm26annbylatsd <- apply(sstwarm26ann, MARGIN=1, FUN=sd, na.rm=TRUE)

	lstwarm85annbylat <- apply(lstwarm85ann, MARGIN=1, FUN=mean, na.rm=TRUE) # mean warming
	sstwarm85annbylat <- apply(sstwarm85ann, MARGIN=1, FUN=mean, na.rm=TRUE)
	lstwarm85annbylatsd <- apply(lstwarm85ann, MARGIN=1, FUN=sd, na.rm=TRUE) # SD warming
	sstwarm85annbylatsd <- apply(sstwarm85ann, MARGIN=1, FUN=sd, na.rm=TRUE)

	lstwarm26sumbylat <- apply(lstwarm26sum, MARGIN=1, FUN=mean, na.rm=TRUE) # mean warming
	sstwarm26sumbylat <- apply(sstwarm26sum, MARGIN=1, FUN=mean, na.rm=TRUE)
	lstwarm26sumbylatsd <- apply(lstwarm26sum, MARGIN=1, FUN=sd, na.rm=TRUE) # SD warming
	sstwarm26sumbylatsd <- apply(sstwarm26sum, MARGIN=1, FUN=sd, na.rm=TRUE)

	lstwarm85sumbylat <- apply(lstwarm85sum, MARGIN=1, FUN=mean, na.rm=TRUE) # mean warming
	sstwarm85sumbylat <- apply(sstwarm85sum, MARGIN=1, FUN=mean, na.rm=TRUE)
	lstwarm85sumbylatsd <- apply(lstwarm85sum, MARGIN=1, FUN=sd, na.rm=TRUE) # SD warming
	sstwarm85sumbylatsd <- apply(sstwarm85sum, MARGIN=1, FUN=sd, na.rm=TRUE)
	
	# trim out NAs
#	keep <- !is.na(lstwarm26annbylat) & !is.na(lstwarm26annbylatsd)
#	lstwarm26annbylat <- lstwarm26annbylat[keep]
#	lstwarm26annbylatsd <- lstwarm26annbylatsd[keep]	
#
#	keep <- !is.na(sstwarm26annbylat) & !is.na(sstwarm26annbylatsd) # trim out NAs
#	sstwarm26annbylat <- sstwarm26annbylat[keep]
#	sstwarm26annbylatsd <- sstwarm26annbylatsd[keep]
#
#	keep <- !is.na(lstwarm85annbylat) & !is.na(lstwarm85annbylatsd)
#	lstwarm85annbylat <- lstwarm85annbylat[keep]
#	lstwarm85annbylatsd <- lstwarm85annbylatsd[keep]	
#
#	keep <- !is.na(sstwarm85annbylat) & !is.na(sstwarm85annbylatsd) # trim out NAs
#	sstwarm85annbylat <- sstwarm85annbylat[keep]
#	sstwarm85annbylatsd <- sstwarm85annbylatsd[keep]
#
#	keep <- !is.na(lstwarm26sumbylat) & !is.na(lstwarm26sumbylatsd)
#	lstwarm26sumbylat <- lstwarm26sumbylat[keep]
#	lstwarm26sumbylatsd <- lstwarm26sumbylatsd[keep]	
#
#	keep <- !is.na(sstwarm26sumbylat) & !is.na(sstwarm26sumbylatsd) # trim out NAs
#	sstwarm26sumbylat <- sstwarm26sumbylat[keep]
#	sstwarm26sumbylatsd <- sstwarm26sumbylatsd[keep]
#
#	keep <- !is.na(lstwarm85sumbylat) & !is.na(lstwarm85sumbylatsd)
#	lstwarm85sumbylat <- lstwarm85sumbylat[keep]
#	lstwarm85sumbylatsd <- lstwarm85sumbylatsd[keep]	
#
#	keep <- !is.na(sstwarm85sumbylat) & !is.na(sstwarm85sumbylatsd) # trim out NAs
#	sstwarm85sumbylat <- sstwarm85sumbylat[keep]
#	sstwarm85sumbylatsd <- sstwarm85sumbylatsd[keep]

	# latitudes in the projection
	latsl <- as.numeric(names(lstwarm26annbylat))
	latss <- as.numeric(names(sstwarm26annbylat))


#############
# write out
#############

lout <- cbind(lat=latsl, lstwarm26annbylat, lstwarm26annbylatsd, lstwarm85annbylat, lstwarm85annbylatsd, lstwarm26sumbylat, lstwarm26sumbylatsd, lstwarm85sumbylat, lstwarm85sumbylatsd) # ocean

oout <- cbind(lat=latss, sstwarm26annbylat, sstwarm26annbylatsd, sstwarm85annbylat, sstwarm85annbylatsd, sstwarm26sumbylat, sstwarm26sumbylatsd, sstwarm85sumbylat, sstwarm85sumbylatsd)

write.csv(lout, file='temp/warming_bylat_land.csv')
write.csv(oout, file='temp/warming_bylat_ocean.csv')

######################
# plot lat patterns
######################

cols <- brewer.pal(4, 'RdBu')[c(1,4)]
pcols <- brewer.pal(4, 'RdBu')[c(2,3)]
quartz(width=4, height=3)
# pdf('figures/warming_land_vs_ocean.pdf', width=4, height=3)
par(mai=c(0.7, 0.85, 0.1, 0.1), mgp=c(2, 0.7, 0))
plot(0, 0, col='white', ylim=c(0,12), xlim=c(-90,90), xlab='', ylab='Delta °C for\n2081-2100 vs. 1986-2005', lwd=2, las=1)
mtext(side=1, 'Latitude', line=2)
polygon(c(latsl, rev(latsl)), c(lstwarm85annbylat+lstwarm85annbylatsd, rev(lstwarm85annbylat-lstwarm85annbylatsd)), col=pcols[1], border=NA)
polygon(c(latss, rev(latss)), c(sstwarm85annbylat+sstwarm85annbylatsd, rev(sstwarm85annbylat-sstwarm85annbylatsd)), col=pcols[2], border=NA)

lines(latsl, lstwarm85annbylat, col=cols[1], lwd=2)
lines(latss, sstwarm85annbylat, col=cols[2], lwd=2)

legend('topleft', legend=c('Land', 'Ocean'), col=cols, lty=1, cex=0.7, bty='n')
	
dev.off()


##################
## examples
##################

# air temp at Whitmire, SC, 34.6333°, -81.6°
ind.lat <- which.min(abs(as.numeric(dimnames(lstwarm26ann)$lat) - 34.6333))
ind.lon <- which.min(abs(as.numeric(dimnames(lstwarm26ann)$lon) - (360-81.6)))

lstwarm26ann[ind.lat, ind.lon]
lstwarm85ann[ind.lat, ind.lon]


# SST at Buoy 41004, 32.501 N 79.099 W
ind.lat <- which.min(abs(as.numeric(dimnames(sstwarm26ann)$lat) - 32.501))
ind.lon <- which.min(abs(as.numeric(dimnames(sstwarm26ann)$lon) - (360-79.099)))

sstwarm26ann[ind.lat, ind.lon]
sstwarm85ann[ind.lat, ind.lon]
mean(sstwarm85ann[(ind.lat-1):(ind.lat+1), (ind.lon-1):(ind.lon+1)], na.rm=TRUE)

# air temp at East Point, MA, 42.420 °N, -70.902 °E
ind.lat <- which.min(abs(as.numeric(dimnames(lstwarm26ann)$lat) - 42.42))
ind.lon <- which.min(abs(as.numeric(dimnames(lstwarm26ann)$lon) - (360-70.902)))

lstwarm26ann[ind.lat, ind.lon]
lstwarm85ann[ind.lat, ind.lon]

