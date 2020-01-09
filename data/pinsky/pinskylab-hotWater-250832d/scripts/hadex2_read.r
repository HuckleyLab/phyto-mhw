# Read HadEX2 TXx temperature data (max daily max by month)
require(raster)
require(abind)
require(reshape2)
require(ncdf4)

# Read in sst climatology (guide for interpolate)
#load('temp/lstclimatology.rdata') # lstclim

# Read in HadEX2 TXx
f <- 'data_dl/hadex2/H2_TXx_1901-2010_RegularGrid_global_3.75x2.5deg_LSmask.nc'

n = nc_open(f) # Open the netCDF file
# print(n) # get information about the file format

# get data
tempsarray = ncvar_get(n, 'Jan') # January. dim order: 96 lon x 73 lat x 110 time (years). 0 lon is at Greenwich Meridian.
	# dim(tempsarray)

for(mo in 2:12){
	thismo <- ncvar_get(n, month.abb[mo])
	tempsarray <- abind(tempsarray, thismo, along=4)
}
	dim(tempsarray)
dimnames(tempsarray) <- list(lon=ncvar_get(n, 'lon'), lat=ncvar_get(n, 'lat'), time=ncvar_get(n, 'time'), mo=month.abb)

nc_close(n)

# Trim to 1986-2005
tempsarray <- tempsarray[,,86:105,]

# Reshape so lat x lon x time since start
#da2 <- aperm(tempsarray, c(2,1,3,4))
tempsmelt <- melt(tempsarray, na.rm=FALSE, value.name='txx')
tempsmelt$yrmo <- paste(substr(tempsmelt$time, 1,4), formatC(match(tempsmelt$mo, month.abb), width=2, flag='0'), sep='')
da2 <- acast(tempsmelt, lat ~ lon ~ yrmo, value.var='txx')
	dim(da2)
	dimnames(da2)
	
	image(da2[,,240])
	
# Flip so lat is 90 to -90
da2 <- da2[dim(da2)[1]:1,,]

# function to find the max in a time-series, removing NA. Returns NA if no non-missing values
maxna <- function(x) { 
	if(any(!is.na(x))) return(max(x, na.rm=TRUE))
	else return(NA)
}

# find the max per grid cell
hadex2txx <- apply(da2, MARGIN=c(1,2), FUN=maxna)
	dim(hadex2txx)
	dimnames(hadex2txx)
	
# adjust lon grid to grid centers
dimnames(hadex2txx)[[2]] <- as.character(seq(3.75/2, 360-3.75/2, by=3.75))
	dimnames(hadex2txx)

save(hadex2txx, file='temp/hadex2_txx.rdata')


# Create raster brick
#txxb <- brick(da2)
#extent(txxb) <- c(0, 360, -90, 90)
#proj4string(txxb) <- '+proj=longlat +datum=WGS84'
#
## examine
#txxb
#plot(txxb[[240]])


# interpolate to 0.5x0.5 °
	# create new grid
#	ngrd <- expand.grid(lat=as.numeric(rownames(lstclim)), lon=as.numeric(colnames(lstclim)))
#	ngrd.sp <- ngrd[,c('lon', 'lat')]
#	coordinates(ngrd.sp) <- ~lon + lat
#	proj4string(ngrd.sp) <- '+proj=longlat +datum=WGS84'
#		
#	# interpolate: raster query approach with points
#	newvalss <- extract(txxb, ngrd.sp, method='bilinear')
#	txxb05rast <- rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss))
#	proj4string(txxb05rast) <- '+proj=longlat +datum=WGS84'

# examine
#txxb05rast
#plot(txxb05rast[[240]])
#
## make array at 0.5x0.5
#txx <- as.array(txxb05rast)	
#dimnames(txx) <- list(lat=as.numeric(rownames(lstclim)), lon=as.numeric(colnames(lstclim)), yrmo=dimnames(da2)[[3]])


# write out array
#save(txx, file='temp/hadex2_txx.rdata')


#############
## examine
#############
load('temp/hadex2_txx.rdata')

for(i in 1:240){
	image(t(txx[nrow(txx):1,,i]), main=dimnames(txx)[[3]][i])
	readline(prompt="Press [enter] to continue")
}
