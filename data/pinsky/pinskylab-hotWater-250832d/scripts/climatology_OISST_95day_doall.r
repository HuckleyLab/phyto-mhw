# This script reads in all OISST files
# Find maximum or 95% temperature in OISST
# Run before climatology_OISST_95day_process.r

require(ncdf4)
require(abind)

outname <- 'tosday.rdata'

############
# Functions
############

# running quantile test function
# https://stackoverflow.com/questions/1058813/on-line-iterator-algorithms-for-estimating-statistical-median-mode-skewnes/2144754#2144754
# https://stackoverflow.com/questions/2837311/incremental-way-of-counting-quantiles-for-large-set-of-data
runq <- function(x, p){
	eta <- 0.2
	quant <- 0
	med <- 0
	cumadev <- 0
	for(i in 1:length(x)){
		eta <- 0.2 - 0.2 * (i/length(x))
		med <- med + eta * sign(x[i] - med)
		quant <- quant + eta * (sign(x[i] - quant) + 2 * p - 1)
#		cumadev <- cumadev + abs(x[i]-med)
#		eta <- 1.5*cumadev/(i^2)
	}
	return(quant)
}


###############################
# Read in OISST netCDF files
# daily means at 0.25x0.25
# save the warmest day in each grid cell
###############################
# find files
files <- list.files('oisst_daily', full.names=TRUE, pattern='.nc')
print(length(files))

# allocate tosday
tosday <- array(NA, dim = c(1440, 720, length(files)))

# read in first file to get dimnames
	n = nc_open(files[1]) # Open the netCDF file
	# print(n) # get information about the file format
	dimnames(tosday)[[1]] <- ncvar_get(n, 'lon')
	dimnames(tosday)[[2]] <- ncvar_get(n, 'lat')
	nc_close(n)


# read in rest of files
for(i in 1:length(files)){
	print(i)
	n = nc_open(files[i]) # Open the netCDF file
	tosday[,,i] = ncvar_get(n, 'sst') # dim order: lon, lat, zlev, time. 0 lon is at date line.
	
	# close file
	nc_close(n)

	# write out each X loops to save progress
#	if(i %% 2000 == 0) save(tosday, file=outname)
}

#save(tosday, file=outname)


# find 95% quantile
tos95day <- apply(tosday, MARGIN=c(1,2), FUN=quantile, probs=0.95, na.rm=TRUE)

# save final version
save(tos95day, file='tos95day.rdata')

# then go run climatology_OISST_95day_process.r