require(raster)
require(reshape2)
require(maps)
require(mapdata)
require(RColorBrewer)

##############################
# Read in UDel ascii files
##############################

library(reshape2)

# find files
files <- list.files('data_dl/udel/air_temp_2014')

# extract years
m <- regexpr('[[:digit:]]{4}', files)
years <- as.numeric(regmatches(files, m))

# extract the data
tempsarray = array(data=NA, dim=c(360, 720, length(files)*12)) # giant array stack of all temperature data (lat, lon, year-month)
	dimnames(tempsarray)[[1]] = seq(-89.75, 89.75, by=0.5)
	dimnames(tempsarray)[[2]] = seq(-179.75, 179.75, by=0.5)
	dimnames(tempsarray)[[3]] = paste(rep(years, rep(12, length(years))), rep(1:12, length(years)), sep='_')

tmpl <- expand.grid(lat=seq(-89.75, 89.75, by=0.5), lon=seq(-179.75, 179.75, by=0.5)) # template with all lats and lons

for(i in 1:length(files)){ # for each file
	print(files[i])
	x <- read.table(paste('data/udel/air_temp_2014/', files[i], sep=''), header=FALSE)
	names(x) <- c('lon', 'lat', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
	x2 <- merge(tmpl, x, all.x=TRUE) # add missing lat/lons

	x3 <- melt(x2, id.vars=c('lat', 'lon'), measure.vars=c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')) # easy with reshape2!
	x4 <- acast(x3, lat ~ lon ~ variable) # easy with reshape2!

	moinds <- (12*i-11):(12*i)
	tempsarray[,,moinds] = x4
}

	# quick look
	dim(tempsarray)
	image(t(tempsarray[,,120])) # looks great!
	tempsarray['0.25','-61.25',120] # tropics
	tempsarray['-89.75','-98.25',120] # antarctic
	tempsarray['89.75','-98.25',120] # arctic: missing
	tempsarray['65.25','-120.25',120] # northern canada
		dimnames(tempsarray)[[3]][120] # December 1995

	range(tempsarray['-89.75','-98.25',]) # antarctic: always cold
	range(tempsarray['65.25','-120.25',]) # northern canada
	range(tempsarray['0.25','-61.25',]) # tropics: warm

# Write out
save(tempsarray, file=paste('temp/udel', years[1], '_', max(years), '.rdata', sep=''))

###########################
# Interpolate to 0.25
###########################
# Read in
load('temp/udel1986_2005.rdata') # tempsarray lat (360) x lon (720) x time (240 mo)

# make a raster brick
udelb <- brick(tempsarray, xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+proj=longlat +datum=WGS84')

	plot(udelb[[1]])

# create new grid
narray <- array(NA, dim=c(720, 1440, 240))
ngrd <- brick(narray, xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+proj=longlat +datum=WGS84')
	
# interpolate: raster resample approach
udel025rast <- resample(udelb, ngrd, method='bilinear')

# examine
udel025rast
plot(udel025rast[[1]]) # it's upside down
plot(udel025rast[[240]])

# make array
udel025 <- as.array(udel025rast)
	dim(udel025)
dimnames(udel025) <- list(seq(-89.875, 89.875, by=0.25), lon=seq(-179.875, 179.875, by=0.25), time=1:240)

	image(x=as.numeric(dimnames(udel025)[[1]]), y=as.numeric(dimnames(udel025)[[2]]), z=udel025[,,1])

# rotate so columns 0 -> 360 lon (centered at Greenwich), rows 90 -> -90 lat (like cmip5)
lons <- as.numeric(colnames(udel025))
newlonord <- c(lons[lons>0], lons[lons<0])
newcolnms <- newlonord
newcolnms[newcolnms<0] <- newcolnms[newcolnms<0] + 360
newlonord <- as.character(newlonord)
newcolnms <- as.character(newcolnms)

lats <- as.numeric(rownames(udel025))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

udel025_2 <- udel025[newlatord, newlonord,]
colnames(udel025_2) <- newcolnms

	image(udel025_2[,,1])

# rename
udel025 <- udel025_2

# save out
save(udel025, file='temp/udel1986_2005_0.25.rdata') #265MB

##################################
# Calculate annual climatology on 0.25x0.25
##################################
# Read in
load('temp/udel1986_2005_0.25.rdata') # udel025 lat (720) x lon (1400) x time (240 mo)


# Average
lstclim <- apply(udel025, MARGIN=c(1,2), FUN=mean)
	dim(lstclim) # 720 x 1440


# save
save(lstclim, file='temp/lstclimatology.rdata')


# Plot climatologies
	image(lstclim)
	
# Nice plot of lstclim
	lonrng <- round(range(as.numeric(colnames(lstclim))))
	latrng <- round(range(as.numeric(rownames(lstclim))))

	quartz(width=5, height=3)
	# png(width=5, height=3, filename='figures/climatology_map_UDel.png', units='in', res=300)
	par(mai=c(0.7, 0.7, 0.1, 0.2), mgp=c(2, 0.5, 0), las=1, tcl=-0.3)
	image(t(lstclim), xlab='Longitude (°E)', ylab='Latitude (°N)', xaxt='n', yaxt='n', col=rev(heat.colors(20)))
	axis(1, at=seq(0,1, length.out=9), labels=seq(lonrng[1], lonrng[2], length.out=9), cex=0.8)
	axis(2, at=seq(0,1, length.out=7), labels=seq(latrng[1], latrng[2], length.out=7), cex=0.8)

	dev.off()


# Nice plot of lstclim and sstclim (if latter loaded)
	lons <- as.numeric(colnames(lstclim))
	lats <- as.numeric(rownames(lstclim))
	lonrng <- round(range(lons))
	latrng <- round(range(lats))
#	cols1 <- rev(heat.colors(20))
	colfun1 <- colorRampPalette(brewer.pal(9, 'PuRd'))
	cols1 <- colfun1(20)

	lons2 <- as.numeric(colnames(sstclim))
	lats2 <- as.numeric(rownames(sstclim))
#	colfun2 <- colorRampPalette(brewer.pal(9, 'OrRd'))
	colfun2 <- colorRampPalette(brewer.pal(9, 'YlGnBu'))
	cols2 <- colfun2(20)

	quartz(width=5, height=3)
	# png(width=5, height=3, filename='figures/climatology_map_UDel&OISST.png', units='in', res=300)
	par(mai=c(0.7, 0.7, 0.1, 0.2), mgp=c(2, 0.5, 0), las=1, tcl=-0.3)
	image(t(lstclim), x=lons, y=lats, xlab='Longitude (°E)', ylab='Latitude (°N)', col=cols1)

	image(t(sstclim), add=TRUE, x=lons2, y=lats2, col=cols2)
	image(t(sstclim), add=TRUE, x=lons2-360, y=lats2, col=cols2)

	dev.off()

#################################################	
# Calculate annual climatology on CMIP5 grid
#################################################	
# need lstclim from previous section

# create new grid
	ngrd <- expand.grid(lat=seq(-88.75,88.75,by=2.5), lon=seq(-178.75,178.75,by=2.5)) # the grid to interpolate for land (2.5)
	
# interpolate
	# raster query approach with points
	ngrd.sp1 <- ngrd[,c('lon', 'lat')]
	coordinates(ngrd.sp1) <- ~lon + lat
	proj4string(ngrd.sp1) <- '+proj=longlat +datum=WGS84'

	lstclimr <- raster(lstclim[nrow(lstclim):1,], xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+proj=longlat +datum=WGS84') # make a raster object from lstclim data
	newvalsl <- extract(lstclimr, ngrd.sp1, method='bilinear')
	lstngrdr <- rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalsl))

# examine
	par(mfrow=c(1,2))
	plot(lstclimr)
	plot(lstngrdr)

# convert to matrix: n for new grid
lstclimn <- as.matrix(lstngrdr)
	rownames(lstclimn) <- yFromRow(lstngrdr)
	colnames(lstclimn) <- xFromCol(lstngrdr)
	
# rotate lst so 0-360
lons <- as.numeric(colnames(lstclimn))
lons[lons<0] <- lons[lons<0]+360
colnames(lstclimn) <- lons
lstclimn <- lstclimn[,order(lons)]

# write out
save(lstclimn, file='temp/lstclimatology_cmip5grid.rdata')


########################################
# Calculate summer climatology on 0.25
########################################

# Read in
load('temp/udel1986_2005_0.25.rdata') # udel025

nms <- dimnames(udel025)
dm <- dim(udel025)
tempsarraybymo <- array(udel025, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=1986:2005)) # reshape so it has a month dimension (3rd dimension)

lstclimbymo <- apply(tempsarraybymo, MARGIN=c(1,2,3), FUN=mean) # climatology for each month
	# image(lstclimbymo[,,1]) # january
	# image(lstclimbymo[,,6]) # june


# Using "summer" months
	lstclimwarm3N <- apply(lstclimbymo[,,6:8], MARGIN=c(1,2), FUN=mean) # temperature of summer 3-month period in each cell for northern hemisphere
	lstclimwarm3S <- apply(lstclimbymo[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # temperature of summer 3-month period in each cell for northern hemisphere
	lstclimwarm3 <- rbind(lstclimwarm3N[as.character(seq(89.875,0.125,by=-0.25)),], lstclimwarm3S[as.character(seq(-0.125,-89.875,by=-0.25)),])

		# plot
		par(mfrow=c(3,1))
		image(lstclimwarm3N, main='northern summer') 
		image(lstclimwarm3S, main='southern summer') 
		image(lstclimwarm3, main='combined summer') 



# save
save(lstclimwarm3, file='temp/lstclimatology_warm3.rdata')



#################################################	
# Calculate summer climatology on CMIP5 grid
# using "summer"
#################################################	
# need sstclimwarm3 from 2 sections previous

require(raster)
require(reshape2)
require(maps)
require(mapdata)

# create new grid
	ngrd <- expand.grid(lat=seq(-88.75,88.75,by=2.5), lon=seq(-178.75,178.75,by=2.5)) # the grid to interpolate for land (2.5)
	
# interpolate
	# raster query approach with points
	ngrd.sp <- ngrd[,c('lon', 'lat')]
	coordinates(ngrd.sp) <- ~lon + lat
	proj4string(ngrd.sp) <- '+proj=longlat +datum=WGS84'

	lstclimr <- raster(lstclimwarm3, xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+proj=longlat +datum=WGS84') # make a raster from sstclim data
	newvalls <- extract(lstclimr, ngrd.sp, method='bilinear') # interpolated sst from summer
	lstngrdr <- rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalls))

# examine
	par(mfrow=c(2,1))
	plot(lstclimr)
	plot(lstngrdr)

# convert to array: n for new grid
lstclimwarm3n <- as.matrix(lstngrdr)
	rownames(lstclimwarm3n) <- rev(yFromRow(lstngrdr))
	colnames(lstclimwarm3n) <- xFromCol(lstngrdr)

	image(lstclimwarm3n, xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(lstclimwarm3n)), labels=rownames(lstclimwarm3n))
		axis(side=2, at=seq(0,1,length=ncol(lstclimwarm3n)), labels=colnames(lstclimwarm3n))

# rotate lst so 0-360 like CMIP5 model grids
lons <- as.numeric(colnames(lstclimwarm3n))
lons[lons<0] <- lons[lons<0]+360
colnames(lstclimwarm3n) <- lons
lstclimwarm3n <- lstclimwarm3n[,order(lons)]

# flip to match cmip5
lstclimwarm3n <- lstclimwarm3n[nrow(lstclimwarm3n):1,]

	image(lstclimwarm3n, xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(lstclimwarm3n)), labels=rownames(lstclimwarm3n))
		axis(side=2, at=seq(0,1,length=ncol(lstclimwarm3n)), labels=colnames(lstclimwarm3n))


# write out
save(lstclimwarm3n, file='temp/lstclimatology_warm3_cmip5grid.rdata')


#####################################	
# Calculate monthly climatology on 0.25
#####################################	
# Read in
load('temp/udel1986_2005_0.25.rdata') # udel025

# reshape so month is a dimension
nms <- dimnames(udel025)
dm <- dim(udel025)
tempsarraybymo <- array(udel025, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=1986:2005)) # reshape so it has a month dimension (3rd dimension)

# average
lstclimbymo <- apply(tempsarraybymo, MARGIN=c(1,2,3), FUN=mean) # climatology for each month

# visualize
	image(lstclimbymo[,,1], xaxt='n', yaxt='n')
		axis(side=1, at=seq(0,1,length=nrow(lstclimbymo)), labels=rownames(lstclimbymo), cex.axis=0.4)
		axis(side=2, at=seq(0,1,length=ncol(lstclimbymo)), labels=colnames(lstclimbymo), cex.axis=0.4)
	

# save
save(lstclimbymo, file='temp/lstclimatology_bymonth.rdata')


#################################################	
# Calculate monthly climatology on CMIP5 grid
#################################################	
# need lstclimbymo from previous section
load('temp/lstclimatology_bymonth.rdata')

# create new grid
	ngrd <- expand.grid(lat=seq(-88.75,88.75,by=2.5), lon=seq(1.25,358.75,by=2.5)) # the grid to interpolate for land (2.5)
	
# interpolate
	# raster query approach with points
	ngrd.sp <- ngrd[,c('lon', 'lat')]
	coordinates(ngrd.sp) <- ~lon + lat
	proj4string(ngrd.sp) <- '+proj=longlat +datum=WGS84'

	lstclimr <- brick(lstclimbymo, xmn=0, xmx=360, ymn=-90, ymx=90, crs='+proj=longlat +datum=WGS84') # make a raster brick from sstclim data
		# plot(lstclimr, y=1) # check orientation

	newvalss <- extract(lstclimr, ngrd.sp, method='bilinear') # interpolated sst by month
	lstngrdr <- rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss))

# examine
	par(mfrow=c(2,1))
	plot(subset(lstclimr,1))
	plot(subset(lstngrdr,1))

# convert to array: n for new grid
lstclimbymon <- as.array(lstngrdr)
	dimnames(lstclimbymon)[[1]] <- yFromRow(lstngrdr)
	dimnames(lstclimbymon)[[2]] <- xFromCol(lstngrdr)
	dimnames(lstclimbymon)[[3]] <- 1:12

	image(lstclimbymon[,,1], xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(lstclimbymon)), labels=rownames(lstclimbymon))
		axis(side=2, at=seq(0,1,length=ncol(lstclimbymon)), labels=colnames(lstclimbymon))

# write out
save(lstclimbymon, file='temp/lstclimatology_bymonth_cmip5grid.rdata')


#############################################
# Calculate warmest month on 0.25x0.25
#############################################
# need lstclimbymon from previous section
load('temp/lstclimatology_bymonth.rdata')

lstclimwarmestmo <- apply(lstclimbymo, MARGIN=c(1,2), FUN=max)

	# check orientation: want 0-360 starting at Greenwich
	image(lstclimwarmestmo, xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(lstclimwarmestmo)), labels=rownames(lstclimwarmestmo))
		axis(side=2, at=seq(0,1,length=ncol(lstclimwarmestmo)), labels=colnames(lstclimwarmestmo))

	rownames(lstclimwarmestmo) # should be 90 to -90
	colnames(lstclimwarmestmo) # should be 0 to 360

# write out
save(lstclimwarmestmo, file='temp/lstclimatology_warmestmonth.rdata')

#############################################
# Calculate warmest month on the CMIP5 grid
#############################################
# need lstclimbymon from previous section
load('temp/lstclimatology_bymonth_cmip5grid.rdata')

lstclimwarmestmon <- apply(lstclimbymon, MARGIN=c(1,2), FUN=max)

	image(lstclimwarmestmon, xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(lstclimwarmestmon)), labels=rownames(lstclimwarmestmon))
		axis(side=2, at=seq(0,1,length=ncol(lstclimwarmestmon)), labels=colnames(lstclimwarmestmon))

	rownames(lstclimwarmestmon) # should be 90 to -90
	colnames(lstclimwarmestmon) # should be 0 to 360


# write out
save(lstclimwarmestmon, file='temp/lstclimatology_warmestmonth_cmip5grid.rdata')
