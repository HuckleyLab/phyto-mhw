# Read GHCNDEX TXx temperature data (max daily max by month)
require(data.table)
require(reshape2)

# Read in TXx 1986-2005
tempsarray <- fread('data_dl/ghcndex/RawData_GHCNDEX_TXx_1986-2005_ANN_from-90to90_from-180to180.txt')
setnames(tempsarray, 1:4, c('lat', 'lon', 'year', 'txx'))

# Set -99 to NA
tempsarray[txx< -99, txx := NA]

# Check
tempsarray[,hist(txx)]

# function to find the max in a time-series, removing NA. Returns NA if no non-missing values
maxna <- function(x) { 
	if(any(!is.na(x))) return(max(x, na.rm=TRUE))
	else return(as.numeric(NA))
}

# Calculate max by grid cell
txxdt <- tempsarray[,.(txx = maxna(txx)), by=.(lat, lon)]

# reorder lon so 0-360
txxdt[lon<0, lon := lon + 360]

# calculate grid centers (not sure what to do with lat, since goes -90 to 90)
txxdt[,longrd := lon + 1.25]

# Reshape so lat x lon
txxmelt <- melt(txxdt, na.rm=FALSE, value.name='txx', id.vars=c('lat', 'lon', 'longrd'))
ghcntxx <- acast(txxmelt, lat ~ longrd, value.var='txx')
	dim(ghcntxx)
	dimnames(ghcntxx)
	
	image(ghcntxx)
	
# Flip so lat is 90 to -90
ghcntxx <- ghcntxx[dim(ghcntxx)[1]:1,]

	image(ghcntxx)

# Write out
save(ghcntxx, file='temp/ghcndex_txx.rdata')



