# Read CRU TS3.21 tmx temperature data (daily max by month)
require(raster)
require(abind)
require(reshape2)
require(ncdf4)
require(raster)

# Read in CRU TS3.21 tmx data
	fs <- c('data/cru_ts3.21/cru_ts3.21.1981.1990.tmx.dat.nc.gz', 'data/cru_ts3.21/cru_ts3.21.1991.2000.tmx.dat.nc.gz', 'data/cru_ts3.21/cru_ts3.21.2001.2010.tmx.dat.nc.gz')

	for(i in 1:length(fs)){
		n = nc_open(fs[i]) # Open the netCDF file
		# print(n) # get information about the file format

		# get data
		inarray = ncvar_get(n, 'tmx') # dim order: 720 lon x 360 lat x 120 time (observations are month, time measured in days since Jan 1, 1900). 0 lon is at date line.
			# dim(inarray)
		dimnames(inarray) <- list(lon=ncvar_get(n, 'lon'), lat=ncvar_get(n, 'lat'), time=ncvar_get(n, 'time'))

		nc_close(n)
		
		if(i==1) tmx <- inarray
		if(i>1) tmx <- abind(tmx, inarray, along=3)
	}
	dim(tmx)
		
	
# Trim to 1986-2005
times <- as.numeric(dimnames(tmx)[[3]])
tmx <- tmx[,,times > 31425 & times < 38701]
dim(tmx) # 720 x 360 x 240

# Reshape so lat x lon x time since start
tmx <- aperm(tmx, c(2,1,3))
	
# Flip so lat is 90 to -90 (like cmip5)
tmx <- tmx[dim(tmx)[1]:1,,]

# Rotate so lon is 0-360 (like cmip5)
lons <- as.numeric(colnames(tmx))
newlonord <- c(lons[lons>0], lons[lons<0])
newcolnms <- newlonord
newcolnms[newcolnms<0] <- newcolnms[newcolnms<0] + 360
newlonord <- as.character(newlonord)
newcolnms <- as.character(newcolnms)

tmx <- tmx[, newlonord,]
colnames(tmx) <- newcolnms

	dimnames(tmx)
	image(tmx[,,1])

# Already at 0.5x0.5, so no need to interpolate!

# Nicer dimension names
dates <- format(as.Date(as.numeric(dimnames(tmx)[[3]]), origin=as.Date('1900-01-01')), format='%Y%m')
dimnames(tmx) <- list(lat=dimnames(tmx)[[1]], lon=dimnames(tmx)[[2]], time=dates)

# write out array
save(tmx, file='temp/cruts321_tmx.rdata')


#############
## examine
#############
load('temp/cruts321_tmx.rdata')

for(i in 1:dim(tmx)[3]){
	image(t(tmx[nrow(tmx):1,,i]), main=dimnames(tmx)[[3]][i])
	readline(prompt="Press [enter] to continue")
}


##############################################################
# warmest daily high in any month (looking across 1986-2005)
# at 0.25x0.25
##############################################################
load('temp/cruts321_tmx.rdata')
source('scripts/image.scale.r') # for plotting color bar on image
require(RColorBrewer)

# find the month with the warmest average daily high
tasmax <- apply(tmx, MARGIN=c(1,2), FUN=max)

	image(t(tasmax[nrow(tasmax):1,])) # quick image
	
	# nicer image
	zlims <- range(tasmax, na.rm=TRUE)
	bks <- seq(zlims[1], zlims[2],length.out=20)
	cols <- colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(length(bks)-1)

	quartz(height=6, width=5)
	layout(matrix(c(1,2),nrow=2), heights=c(3,1))
	par(mai=c(0.5, 0.5, 0.3, 0.1))
	image(x=as.numeric(dimnames(tasmax)[[2]]), y=rev(as.numeric(dimnames(tasmax)[[1]])), t(tasmax[nrow(tasmax):1,]), col=cols, breaks=bks, main='tasmax 1986-2005', xlab='', ylab='')
		par(mar=c(3,1,1,1), mai=c(0.8, 0.2, 0.1, 0.1), mgp=c(2,0.8, 0))
	image.scale(zlim=zlims, col=cols, breaks=bks, horiz=TRUE, xlab='tmxmx (°C)')

# resample to 0.25x0.25
tasmaxr <- raster(tasmax, xmn=0, xmx=360, ymn=-90, ymx=90) # convert to raster
nmat <- matrix(NA, nrow=720, ncol=1440)
ngrd <- raster(nmat, xmn=0, xmx=360, ymn=-90, ymx=90) # new raster grid
tasmax2 <- resample(tasmaxr, ngrd, method='bilinear')

	plot(tasmax2)
	
# rename
tasmax <- as.matrix(tasmax2)
rownames(tasmax) <- yFromRow(tasmax2)
colnames(tasmax) <- xFromCol(tasmax2)

# write out array
save(tasmax, file='temp/tasmax.rdata')


##############################################################
# warmest daily high in each month (looking across 1986-2005)
# NOTE: still 0.5x0.5°
##############################################################
load('temp/cruts321_tmx.rdata')

# reshape so month is a separate dimension
tmxmelt <- melt(tmx, value.name='tmx')
tmxmelt$yr <- substr(tmxmelt$time, 1,4)
tmxmelt$mo <- substr(tmxmelt$time, 5,6)
tmxbymo <- acast(tmxmelt, lat ~ lon ~ yr ~ mo, value.var='tmx')
	dim(tmxbymo) # 360 x 720 x 20 x 12

# calculate max in each month, looking across years
tasmaxbymo <- apply(tmxbymo, MARGIN=c(1,2,4), FUN=max)

# reshape so lat is 90 to -90
tasmaxbymo <- tasmaxbymo[dim(tasmaxbymo)[1]:1,,]

	image(t(tasmaxbymo[nrow(tasmaxbymo):1,,1])) # quick image
	image(t(tasmaxbymo[nrow(tasmaxbymo):1,,6])) # quick image
	
# write out array
save(tasmaxbymo, file='temp/tasmax_bymonth.rdata')
