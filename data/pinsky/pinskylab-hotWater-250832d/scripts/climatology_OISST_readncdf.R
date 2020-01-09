# Read in OISST netCDF files and save in .rdata format

require(raster)
require(reshape2)
require(maps)
require(mapdata)
require(ncdf4) # for reading netCDF files
require(abind) # for combining arrays
source('scripts/image.scale.r') # for plotting color bar on image
require(RColorBrewer)

###############################
# Read in OISST netCDF files
# monthly means at 0.25x0.25
###############################

# find files
files <- list.files('../oisst', full.names=TRUE, pattern='.nc')

for(i in 1:files){
	n = nc_open(files[i]) # Open the netCDF file
	# print(n) # get information about the file format

	# extract years from filename (could get from times)
	m1 <- regexpr('[[:digit:]]{4}', files[i])
	yearstart <- as.numeric(regmatches(files[i], m1))
	m2 <- regexpr('-[[:digit:]]{4}', files[i])
	yearend <- as.numeric(substr(regmatches(files[i], m2), 2, 5))

	# get data
	tempsarray = ncvar_get(n, 'tos') # dim order: lon, lat, time. 0 lon is at date line.
		# dim(tempsarray)
	lats = ncvar_get(n, 'lat')
	lons = ncvar_get(n, 'lon')
	times = ncvar_get(n, 'time') # days since 1978-01-01 00:00:00 (value every month: 5 years x 12 mo = 60 times)

	dimnames(tempsarray) <- list(lon=lons, lat=lats, time=paste(rep(yearstart:yearend, rep(12, yearend-yearstart+1)), rep(1:12, yearend-yearstart+1), sep='_'))

	# reshape to lat, lon, time
	tempsarray <- aperm(tempsarray, c(2,1,3))

	# convert K to C
	tempsarray <- tempsarray - 273.15 # as noted in file metadata

	# quick look
	# image(x=lons, y=lats, z=t(tempsarray[,,60]))
	tempsarray[360,800,60] # middle of the pacific
	tempsarray[1,800,60] # antarctic (NA)
	tempsarray[719,800,60] # arctic

	# Write out
	save(tempsarray, file=paste('temp/oisst', yearstart, '_', yearend, '.rdata', sep='')) # 140mb (better than 1GB original)

	# cloe file
	nc_close(n)
}
