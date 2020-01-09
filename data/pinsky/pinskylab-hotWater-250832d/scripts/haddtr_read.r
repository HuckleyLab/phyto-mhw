# Read HadDTR temperature data
# see https://stackoverflow.com/questions/24115110/importing-sea-surface-temperature-text-files-in-ascii-format-into-rrequire(raster)
require(raster)
require(abind)

# Read in sst climatology (guide for interpolation)
load('temp/oisst0.5deg_1986_2005.rdata') # tempsarray at 0.5x0.5 1986-2005
	sstclim <- tempsarray
load('temp/oisst1986_1990.rdata') # tempsarray at 0.25x0.25
	sstclim025 <- tempsarray

# Read in HadDTR
f <- 'data_dl/hatdtr/monthly_climatology.txt'
d <- readLines(f)
d <- split(d, rep(1:12, each=30)) # 12 months of 30 lines each
d <- lapply(d, function(x) read.fwf(textConnection(x), rep(8, 72), skip=1, stringsAsFactors=FALSE, na.strings=c(' -99.99 ', ' -99.99'))) # 72 fields of 8 chars each
d <- lapply(d, function(x) sapply(x, as.numeric))
da <- array(unlist(d), dim = c(nrow(d[[1]]), ncol(d[[1]]), length(d))) # to array

# Rotate so 0-360 columns and rows 90 to -90 (like cmip5)
da2 <- abind(da[,37:72,], da[,1:36,], along=2)

# Create raster brick
dtrb <- brick(da2)
names(dtrb) <- month.abb # a constant in R
extent(dtrb) <- c(0, 360, -90, 90)
proj4string(dtrb) <- '+proj=longlat +datum=WGS84'

# examine
dtrb
plot(dtrb[[1]])
plot(dtrb)


# interpolate to 0.5x0.5 ° (no longer used)
	# create new grid
	ngrd <- expand.grid(lat=as.numeric(rownames(sstclim)), lon=as.numeric(colnames(sstclim)))
	ngrd.sp <- ngrd[,c('lon', 'lat')]
	coordinates(ngrd.sp) <- ~lon + lat
	proj4string(ngrd.sp) <- '+proj=longlat +datum=WGS84'
		
	# interpolate: raster query approach with points
	newvalss <- extract(dtrb, ngrd.sp, method='bilinear')
	dtr05rast <- rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss))
	proj4string(dtr05rast) <- '+proj=longlat +datum=WGS84'

	# examine
	dtr05rast
	plot(dtr05rast)

	# make array
	dtr <- as.array(dtr05rast)	
	dimnames(dtr) <- list(lat=as.numeric(rownames(sstclim)), lon=as.numeric(colnames(sstclim)), mon=month.abb)


	# write out array
	save(dtr, file='temp/dtrclimatology.rdata')
	
# interpolate to 0.25x0.25° (OISST)
	# create new grid
	ngrd2 <- expand.grid(lat=as.numeric(rownames(sstclim025)), lon=as.numeric(colnames(sstclim025)))
	ngrd2.sp <- ngrd2[,c('lon', 'lat')]
	coordinates(ngrd2.sp) <- ~lon + lat
	proj4string(ngrd2.sp) <- '+proj=longlat +datum=WGS84'
		
	# interpolate: raster query approach with points
	newvalss2 <- extract(dtrb, ngrd2.sp, method='bilinear')
	dtr025rast <- rasterFromXYZ(cbind(ngrd2[,c('lon', 'lat')], newvalss2))
	proj4string(dtr025rast) <- '+proj=longlat +datum=WGS84'

	# examine
	dtr025rast
	plot(dtr025rast[[1]])
	plot(dtr025rast)

	# make array
	dtr025 <- as.array(dtr025rast)
	dimnames(dtr025) <- list(lat=as.numeric(rownames(sstclim025)), lon=as.numeric(colnames(sstclim025)), mon=month.abb)


	# write out array
	save(dtr025, file='temp/dtrclimatology0.25.rdata')
	
# interpolate to 1.25x1.25° (CMIP5 tos)
	# create new grid
	ngrd3 <- expand.grid(lat=seq(-89.375, 89.375, by=1.25), lon=seq(0.625, 359.375, by=1.25))
	ngrd3.sp <- ngrd3[,c('lon', 'lat')]
	coordinates(ngrd3.sp) <- ~lon + lat
	proj4string(ngrd3.sp) <- '+proj=longlat +datum=WGS84'
		
	# interpolate: raster query approach with points
	newvalss3 <- extract(dtrb, ngrd3.sp, method='bilinear')
	dtr125rast <- rasterFromXYZ(cbind(ngrd3[,c('lon', 'lat')], newvalss3))
	proj4string(dtr125rast) <- '+proj=longlat +datum=WGS84'

	# examine
	dtr125rast
	plot(dtr125rast[[1]])
	plot(dtr125rast)

	# make array
	dtr125 <- as.array(dtr125rast)
	dimnames(dtr125) <- list(lat=seq(-89.375, 89.375, by=1.25), lon=seq(0.625, 359.375, by=1.25), mon=month.abb)


	# write out array
	save(dtr125, file='temp/dtrclimatology1.25.rdata')
	
