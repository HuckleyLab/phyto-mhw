require(raster)
require(reshape2)
require(maps)
require(mapdata)
require(ncdf4) # for reading netCDF files
require(abind) # for combining arrays
source('scripts/image.scale.r') # for plotting color bar on image
require(RColorBrewer)

############
# Functions
############

# Returns maximum non-NA value if present, otherwise returns NA (instead of -Inf)
maxNA <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

######################
# Prep (do once)
######################
	###################################################
	# Average into 0.5x0.5° grid and combine all files
	###################################################

	# find files
	files <- list.files('temp', full.names=TRUE, pattern='oisst[[:digit:]]{4}_[[:digit:]]{4}\\.rdata')

	# read in first array and average into 0.5x0.5
	# approach is to overlap the 4 grid cells to be combined, and then average (in 3D array space)
	load(files[1])
	ds <- dim(tempsarray)
	t1 <- tempsarray[seq(1,ds[1]-1,by=2), seq(1,ds[2]-1,by=2),] # top left corner of each new grid cell (across all times)
	t2 <- tempsarray[seq(2,ds[1],by=2), seq(1,ds[2]-1,by=2),] # bottom left corner
	t3 <- tempsarray[seq(1,ds[1]-1,by=2), seq(2,ds[2],by=2),] # top right
	t4 <- tempsarray[seq(2,ds[1],by=2), seq(2,ds[2],by=2),] # bottom right
	toaverage <- abind(t1, t2, t3, t4, along=4) # bind them along a new dimension
	tempsarray0.5 <- apply(toaverage, MARGIN=1:3, FUN=mean) # average the 4 grid cells together. 5-10 min. slow!
		dim(tempsarray0.5)

	newlats <- colMeans(rbind(as.numeric(dimnames(t1)$lat), as.numeric(dimnames(t2)$lat)))
	newlons <- colMeans(rbind(as.numeric(dimnames(t1)$lon), as.numeric(dimnames(t3)$lon)))
	dimnames(tempsarray0.5) <- list(lat=newlats, lon=newlons, time=dimnames(t1)$time)

	# read in next arrays
	for(i in 2:length(files)){
		load(files[i])
		ds <- dim(tempsarray)
		t1 <- tempsarray[seq(1,ds[1]-1,by=2), seq(1,ds[2]-1,by=2),] # top left corner of each new grid cell
		t2 <- tempsarray[seq(2,ds[1],by=2), seq(1,ds[2]-1,by=2),] # bottom left corner
		t3 <- tempsarray[seq(1,ds[1]-1,by=2), seq(2,ds[2],by=2),] # top right
		t4 <- tempsarray[seq(2,ds[1],by=2), seq(2,ds[2],by=2),] # bottom right
		toaverage <- abind(t1, t2, t3, t4, along=4) # bind them along a new dimension

		temp <- apply(toaverage, MARGIN=1:3, FUN=mean) # 5-10 min. slow!
			dim(temp)
		dimnames(temp) <- list(lat=newlats, lon=newlons, time=dimnames(t1)$time)

		temp <- round(temp, digits=1) # average to 0.1 precision like Udel
		
		tempsarray0.5 <- abind(tempsarray0.5, temp, along=3)
	}

	# get years
	yrs <- as.numeric(gsub('_[[:digit:]]{1,2}$', '', dimnames(tempsarray0.5)[[3]]))

	# change name for saving
	tempsarray <- tempsarray0.5

	# Write out
	save(tempsarray, file=paste('temp/oisst0.5deg_', min(yrs), '_', max(yrs), '.rdata', sep=''))



#################################################	
# Calculate annual climatology on 0.25x0.25° grid
#################################################	

# Read in
load('temp/oisst1986_1990.rdata') # tempsarray at 0.25x0.25
	oisst <- tempsarray
load('temp/oisst1991_1995.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst1996_2000.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst2001_2005.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
dim(oisst) # 720 x 140 x 240

# Average
sstclim <- apply(oisst, MARGIN=c(1,2), FUN=mean)
	
	image(sstclim)

# rotate so columns 0 -> 360 lon, rows 90 -> -90 lat (like cmip5)
lons <- as.numeric(colnames(sstclim)) # good

lats <- as.numeric(rownames(sstclim))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

sstclim <- sstclim[newlatord, ]

	image(sstclim)

# save
save(sstclim, file='temp/sstclimatology.rdata')

# Plot climatologies
	image(sstclim)

# Nicer plot of sstclim
	lonrng <- c(0,360) # really 0 358
	latrng <- round(range(as.numeric(rownames(sstclim))))

	quartz(width=5, height=3)
	# png(width=5, height=3, filename='figures/climatology_map_OISST.png', units='in', res=300)
	par(mai=c(0.7, 0.7, 0.1, 0.2), mgp=c(2, 0.5, 0), las=1, tcl=-0.3)
	image(t(sstclim), xlab='Longitude (°E)', ylab='Latitude (°N)', xaxt='n', yaxt='n', col=rev(heat.colors(20)))
	axis(1, at=seq(0,1, length.out=9), labels=seq(lonrng[1], lonrng[2], length.out=9), cex=0.8)
	axis(2, at=seq(0,1, length.out=9), labels=seq(latrng[1], latrng[2], length.out=9), cex=0.8)

	dev.off()




#################################################	
# Calculate annual climatology on CMIP5 grid
#################################################	
# need sstclim from previous section

# create new grid
	ngrd2 <- expand.grid(lat=seq(-89.375,89.375,by=1.25), lon=seq(0.625,359.375,by=1.25)) # the grid to interpolate for ocean (1.25)
	
# interpolate
	# raster query approach with points
	ngrd.sp2 <- ngrd2[,c('lon', 'lat')]
	coordinates(ngrd.sp2) <- ~lon + lat
	proj4string(ngrd.sp2) <- '+proj=longlat +datum=WGS84'

	sstclimr <- raster(sstclim[nrow(sstclim):1,], xmn=0, xmx=360, ymn=-89, ymx=89, crs='+proj=longlat +datum=WGS84') # make a raster object from sstclim data
	newvalss <- extract(sstclimr, ngrd.sp2, method='bilinear')
	sstngrdr <- rasterFromXYZ(cbind(ngrd2[,c('lon', 'lat')], newvalss))

# examine
	par(mfrow=c(1,2))
	plot(sstclimr)
	plot(sstngrdr)

# convert to matrix: n for new grid
sstclimn <- as.matrix(sstngrdr)
	rownames(sstclimn) <- yFromRow(sstngrdr)
	colnames(sstclimn) <- xFromCol(sstngrdr)
	
# write out
save(sstclimn, file='temp/sstclimatology_cmip5grid.rdata')


#####################################	
# Calculate summer climatology on 0.25x0.25
#####################################	
# Read in
load('temp/oisst1986_1990.rdata') # tempsarray at 0.25x0.25
	oisst <- tempsarray
load('temp/oisst1991_1995.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst1996_2000.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst2001_2005.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
dim(oisst) # 720 x 140 x 240

# Reshape so month has a dimension
nms <- dimnames(oisst)
mos <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(2,length(nms[[3]])*2,by=2)]) # months
yrs <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(1,length(nms[[3]])*2,by=2)]) # years
dm <- dim(oisst)
tempsarraybymo <- array(oisst, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=sort(unique(yrs)))) # add a month dimension (3rd dimension)

# average within months
sstclimbymo <- apply(tempsarraybymo, MARGIN=c(1,2,3), FUN=mean) # climatology for each month

	image(sstclimbymo[,,1])

# Using "summer" months
	sstclimwarm3N <- apply(sstclimbymo[,,6:8], MARGIN=c(1,2), FUN=mean) # temperature of summer 3-month period in each cell for northern hemisphere
	sstclimwarm3S <- apply(sstclimbymo[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # temperature of summer 3-month period in each cell for northern hemisphere
	sstclimwarm3 <- rbind(sstclimwarm3S[as.character(seq(-89.875,-0.125,by=0.25)),], sstclimwarm3N[as.character(seq(0.125,89.875,by=0.25)),])

		# maps
		zlims <- range(c(sstclimwarm3N, sstclimwarm3S), na.rm=TRUE)
		par(mfrow=c(3,1))
		image(y=as.numeric(row.names(sstclimwarm3N)), x=as.numeric(colnames(sstclimwarm3N)), t(sstclimwarm3N), zlim=zlims)
		image(t(sstclimwarm3S), zlim=zlims)
		image(t(sstclimwarm3), zlim=zlims)

# rotate so columns 0 -> 360 lon, rows 90 -> -90 lat (like cmip5)
lons <- as.numeric(colnames(sstclimwarm3)) # good

lats <- as.numeric(rownames(sstclimwarm3))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

sstclimwarm3 <- sstclimwarm3[newlatord, ]

	image(sstclimwarm3)

# save
save(sstclimwarm3, file='temp/sstclimatology_warm3.rdata')


#################################################	
# Calculate summer climatology on CMIP5 grid
#################################################	
# need sstclimwarm3 from previous section

# create new grid
	ngrd2 <- expand.grid(lat=seq(-89.375,89.375,by=1.25), lon=seq(0.625,359.375,by=1.25)) # the grid to interpolate for ocean (1.25)
	
# interpolate
	# raster query approach with points
	ngrd.sp2 <- ngrd2[,c('lon', 'lat')]
	coordinates(ngrd.sp2) <- ~lon + lat
	proj4string(ngrd.sp2) <- '+proj=longlat +datum=WGS84'

	sstclimr <- raster(sstclimwarm3, xmn=0, xmx=360, ymn=-89, ymx=89, crs='+proj=longlat +datum=WGS84') # make a raster from sstclim data
	newvalss <- extract(sstclimr, ngrd.sp2, method='bilinear') # interpolated sst from summer
	sstngrdr <- rasterFromXYZ(cbind(ngrd2[,c('lon', 'lat')], newvalss))

# examine
	par(mfrow=c(2,1))
	plot(sstclimr)
	plot(sstngrdr)

# convert to array: n for new grid
sstclimwarm3n <- as.matrix(sstngrdr)
	rownames(sstclimwarm3n) <- rev(yFromRow(sstngrdr))
	colnames(sstclimwarm3n) <- xFromCol(sstngrdr)

	image(t(sstclimwarm3n), xaxt='n', yaxt='n')	
		axis(side=2, at=seq(0,1,length=nrow(sstclimwarm3n)), labels=rownames(sstclimwarm3n))
		axis(side=1, at=seq(0,1,length=ncol(sstclimwarm3n)), labels=colnames(sstclimwarm3n))

# flip to match cmip5
sstclimwarm3n <- sstclimwarm3n[nrow(sstclimwarm3n):1,]

	image(sstclimwarm3n, xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(sstclimwarm3n)), labels=rownames(sstclimwarm3n))
		axis(side=2, at=seq(0,1,length=ncol(sstclimwarm3n)), labels=colnames(sstclimwarm3n))


# write out
save(sstclimwarm3n, file='temp/sstclimatology_warm3_cmip5grid.rdata')



#####################################	
# Calculate monthly climatology on 0.25x0.25
#####################################	
# Read in
load('temp/oisst1986_1990.rdata') # tempsarray at 0.25x0.25
	oisst <- tempsarray
load('temp/oisst1991_1995.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst1996_2000.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst2001_2005.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
dim(oisst) # 720 x 140 x 240

nms <- dimnames(oisst)
mos <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(2,length(nms[[3]])*2,by=2)]) # months
yrs <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(1,length(nms[[3]])*2,by=2)]) # years
dm <- dim(oisst)
tempsarraybymo <- array(oisst, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=sort(unique(yrs)))) # add a month dimension (3rd dimension)

sstclimbymo <- apply(tempsarraybymo, MARGIN=c(1,2,3), FUN=mean) # climatology for each month

# rotate so columns 0 -> 360 lon (centered at Greenwich), rows 90 -> -90 lat (like cmip5)
lons <- as.numeric(dimnames(sstclimbymo)[[2]])
newlonord <- c(lons[lons>0], lons[lons<0])
newcolnms <- newlonord
newcolnms[newcolnms<0] <- newcolnms[newcolnms<0] + 360
newlonord <- as.character(newlonord)
newcolnms <- as.character(newcolnms)

lats <- as.numeric(dimnames(sstclimbymo)[[1]])
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

sstclimbymo <- sstclimbymo[newlatord, newlonord,]
dimnames(sstclimbymo)[[2]] <- newcolnms

	image(sstclimbymo[,,1], xaxt='n', yaxt='n')
		axis(side=1, at=seq(0,1,length=nrow(sstclimbymo)), labels=rownames(sstclimbymo), cex.axis=0.4)
		axis(side=2, at=seq(0,1,length=ncol(sstclimbymo)), labels=colnames(sstclimbymo), cex.axis=0.4)

# save
save(sstclimbymo, file='temp/sstclimatology_bymonth.rdata')


#################################################	
# Calculate monthly climatology on CMIP5 grid
#################################################	
# need sstclimbymo from previous section

# create new grid
	ngrd2 <- expand.grid(lat=seq(-89.375,89.375,by=1.25), lon=seq(0.625,359.375,by=1.25)) # the grid to interpolate for ocean (1.25)
	
# interpolate
	# raster query approach with points
	ngrd.sp2 <- ngrd2[,c('lon', 'lat')]
	coordinates(ngrd.sp2) <- ~lon + lat
	proj4string(ngrd.sp2) <- '+proj=longlat +datum=WGS84'

	sstclimr <- brick(sstclimbymo, xmn=0, xmx=360, ymn=-89, ymx=89, crs='+proj=longlat +datum=WGS84') # make a raster brick from sstclim data
	newvalss <- extract(sstclimr, ngrd.sp2, method='bilinear') # interpolated sst by month
	sstngrdr <- rasterFromXYZ(cbind(ngrd2[,c('lon', 'lat')], newvalss))

# examine
	par(mfrow=c(2,1))
	plot(subset(sstclimr,1))
	plot(subset(sstngrdr,1))

# convert to array: n for new grid
sstclimbymon <- as.array(sstngrdr)
	dimnames(sstclimbymon)[[1]] <- yFromRow(sstngrdr)
	dimnames(sstclimbymon)[[2]] <- xFromCol(sstngrdr)
	dimnames(sstclimbymon)[[3]] <- 1:12

	image(sstclimbymon[,,1], xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(sstclimbymon)), labels=rownames(sstclimbymon))
		axis(side=2, at=seq(0,1,length=ncol(sstclimbymon)), labels=colnames(sstclimbymon))


# write out
save(sstclimbymon, file='temp/sstclimatology_bymonth_cmip5grid.rdata')


#############################################
# Calculate warmest month on the 0.25x0.25 grid
#############################################
# need sstclimbymon from previous section
load('temp/sstclimatology_bymonth.rdata') # sstclimbymo

sstclimwarmestmo <- apply(sstclimbymo, MARGIN=c(1,2), FUN=max)

	image(sstclimwarmestmo, xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(sstclimwarmestmo)), labels=rownames(sstclimwarmestmo))
		axis(side=2, at=seq(0,1,length=ncol(sstclimwarmestmo)), labels=colnames(sstclimwarmestmo))


# write out
save(sstclimwarmestmo, file='temp/sstclimatology_warmestmonth.rdata')

#############################################
# Calculate warmest month on the CMIP5 grid
#############################################
# need sstclimbymon from previous section
load('temp/sstclimatology_bymonth_cmip5grid.rdata')  # sstclimbymon (note "n")

sstclimwarmestmon <- apply(sstclimbymon, MARGIN=c(1,2), FUN=max)

	image(sstclimwarmestmon, xaxt='n', yaxt='n')	
		axis(side=1, at=seq(0,1,length=nrow(sstclimwarmestmon)), labels=rownames(sstclimwarmestmon))
		axis(side=2, at=seq(0,1,length=ncol(sstclimwarmestmon)), labels=colnames(sstclimwarmestmon))


# write out
save(sstclimwarmestmon, file='temp/sstclimatology_warmestmonth_cmip5grid.rdata')



#####################################	
# Calculate each month's warmest average daily high from 1986-2005 on 0.25x0.25
# Add 0.5*HadDTR to warmest example of each month in 1986-2005
#####################################	
# Read in OISST
load('temp/oisst1986_1990.rdata') # tempsarray at 0.25x0.25
	oisst <- tempsarray
load('temp/oisst1991_1995.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst1996_2000.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
load('temp/oisst2001_2005.rdata') # tempsarray at 0.25x0.25
	oisst <- abind(oisst, tempsarray, along=3)
dim(oisst) # 720 x 140 x 240

# Read in DTR interpolated to 0.25 and filled in
load('temp/dtrclimatology_interp0.25.rdata') # dtr025 at 0.25x0.25, interpolated to match oisst

# Reshape oisst to lat x lon x month x year
nms <- dimnames(oisst)
mos <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(2,length(nms[[3]])*2,by=2)]) # months
yrs <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(1,length(nms[[3]])*2,by=2)]) # years
dm <- dim(oisst)
oisstbymo <- array(oisst, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=sort(unique(yrs)))) # add a month dimension (3rd dimension)

# rotate sst monthly temps 1986-2005 so columns 0 -> 360 lon (centered at Greenwich), rows 90 -> -90 lat (like cmip5)
lons <- as.numeric(nms[[2]])
newlonord <- c(lons[lons>0], lons[lons<0])
newcolnms <- newlonord
newcolnms[newcolnms<0] <- newcolnms[newcolnms<0] + 360
newlonord <- as.character(newlonord)
newcolnms <- as.character(newcolnms)

lats <- as.numeric(nms[[1]])
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

sstbymo <- oisstbymo[newlatord, newlonord,,] # lat x lon (0-360) x month x year
dimnames(sstbymo)[[2]] <- newcolnms

	image(sstbymo[,,1,1], xaxt='n', yaxt='n')
		axis(side=1, at=seq(0,1,length=nrow(sstbymo)), labels=rownames(sstbymo), cex.axis=0.4)
		axis(side=2, at=seq(0,1,length=ncol(sstbymo)), labels=colnames(sstbymo), cex.axis=0.4)


# expand dtr to 20 years
dtrlong <- abind(list(dtr025)[rep(1,20)], along=4)
	dim(dtrlong)
	dim(sstbymo)
	
# add daily temperature range to monthly temperatures to get average daily highs in every month
tosmaxbymobyyr <- sstbymo + dtrlong*0.5 # assume average daily high is 50% of DTR above monthly mean
	dim(tosmaxbymobyyr)

	image(tosmaxbymobyyr[,,1,1], xaxt='n', yaxt='n')
		axis(side=1, at=seq(0,1,length=nrow(sstbymo)), labels=rownames(sstbymo), cex.axis=0.4)
		axis(side=2, at=seq(0,1,length=ncol(sstbymo)), labels=colnames(sstbymo), cex.axis=0.4)
	
	

# find highest average daily high across years for each month
tosmaxbymo <- apply(tosmaxbymobyyr, MARGIN=c(1,2,3), FUN=max)
	dim(tosmaxbymo)
	range(tosmaxbymo, na.rm=TRUE)
	
# save
save(tosmaxbymo, file='temp/tosmax_bymonth.rdata')


#############################################
# Calculate warmest daily high on the 0.25x0.25 grid
# Add 0.5*HadDTR to warmest day in 1986-2005
#############################################
# Load warmest day
load('temp/tosmaxday.rdata') # tosmaxday from climatology_OISST_maxday.r. 1986-2005.

# Read in DTR interpolated to 0.25 and filled in
load('temp/dtrclimatology_interp0.25.rdata') # dtr025 at 0.25x0.25, interpolated to match oisst

# Find widest DTR
dtrmax <- apply(dtr025, MARGIN=c(1,2), FUN=maxNA)

# Check dimensions
	dim(dtrmax)
	dim(tosmaxday)
	
# add daily temperature range to warmest day
tosmax <- tosmaxday + dtrmax*0.5 # assume daily high is 50% of DTR above
	dim(tosmax)

	zlims <- range(tosmax, na.rm=TRUE)
	bks <- seq(zlims[1], zlims[2],length.out=30)
	cols <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks)-1)

	layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(10,1))
	image(tosmax, xaxt='n', yaxt='n', zlim=zlims, breaks=bks, col=cols)	
		axis(side=1, at=seq(0,1,length=nrow(tosmax)), labels=rownames(tosmax))
		axis(side=2, at=seq(0,1,length=ncol(tosmax)), labels=colnames(tosmax))
	image.scale(zlim=zlims, col=cols, breaks=bks, horiz=FALSE, ylab='°C')
	


# write out
save(tosmax, file='temp/tosmax.rdata')


#############################################
# Calculate 95% warmest daily high on the 0.25x0.25 grid
# Add 0.5*HadDTR to 95% warmest day in 1986-2005
#############################################
# Load warmest day
load('temp/tos95day.rdata') # tosmaxbymo

# Read in DTR interpolated to 0.25 and filled in
load('temp/dtrclimatology_interp0.25.rdata') # dtr025 at 0.25x0.25, interpolated to match oisst

# Find widest DTR across the months
dtrmax <- apply(dtr025, MARGIN=c(1,2), FUN=maxNA)

# Check dimensions
	dim(dtrmax)
	dim(tos95day)
	
# add daily temperature range to 95% warmest day
tos95max <- tos95day + dtrmax*0.5 # assume average daily high is 50% of DTR above daily mean
	dim(tos95max)

	zlims <- range(tos95max, na.rm=TRUE)
	bks <- seq(zlims[1], zlims[2],length.out=30)
	cols <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks)-1)

	layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(10,1))
	image(tos95max, xaxt='n', yaxt='n', zlim=zlims, breaks=bks, col=cols)	
		axis(side=1, at=seq(0,1,length=nrow(tos95max)), labels=rownames(tos95max))
		axis(side=2, at=seq(0,1,length=ncol(tos95max)), labels=colnames(tos95max))
	image.scale(zlim=zlims, col=cols, breaks=bks, horiz=FALSE, ylab='°C')
	


# write out
save(tos95max, file='temp/tos95max.rdata')

