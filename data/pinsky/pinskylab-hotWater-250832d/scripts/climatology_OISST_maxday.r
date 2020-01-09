# Find maximum daily temperature in OISST
# 1986-2005
# run on a workstation as it takes ~15 hours

require(ncdf4)
require(abind)

############
# Functions
############

# Returns maximum non-NA value if present, otherwise returns NA (instead of -Inf)
maxNA <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

###############################
# Read in OISST netCDF files
# daily means at 0.25x0.25
# save the warmest day in each grid cell
# Run this on Amphpirion: takes ~ 15 hours
###############################
# find files
files <- list.files('oisst_daily', full.names=TRUE, pattern='.nc')
print(length(files))

# read in first file
	n = nc_open(files[1]) # Open the netCDF file
	# print(n) # get information about the file format

	# get data
	tosmaxday = ncvar_get(n, 'sst') # dim order: lon, lat, zlev, time. 0 lon is at date line.
		# dim(tempsarray) # 1440 x 720
		# range(as.numeric(tosmaxday), na.rm=TRUE)
	dimnames(tosmaxday)[[1]] <- ncvar_get(n, 'lon')
	dimnames(tosmaxday)[[2]] <- ncvar_get(n, 'lat')
	nc_close(n)


# read in rest of files and keep the warmest day
for(i in 2:length(files)){
	print(i)
	n = nc_open(files[i]) # Open the netCDF file
	tempsarray = ncvar_get(n, 'sst') # dim order: lon, lat, zlev, time. 0 lon is at date line.

	# keep the warmest day
	both <- abind(tosmaxday, tempsarray, along=3)
	tosmaxday <- apply(both, MARGIN=c(1,2), FUN=maxNA)
	
	print(range(as.numeric(tosmaxday), na.rm=TRUE)) # track progress. should increase

	# close file
	nc_close(n)

	# write out each X loops to save progress
	if(i %% 100 == 0) save(tosmaxday, file='tosmaxday.rdata')
}

# save final version
save(tosmaxday, file='tosmaxday.rdata')


# then go run climatology_OISST_maxday_process.r