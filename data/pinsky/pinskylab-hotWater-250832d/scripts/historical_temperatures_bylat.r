# calculate long-term observed temperature change by latitude
# using UDel, GISS, and ERSST 1900-2014
#  get UDel data at http://climate.geog.udel.edu/~climate/html_pages/Global2014/air_temp_2014.tar.gz (233MB file)
#  get ERSST data at https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v4/ascii/ (used a small bash loop)
#  get GISS https://data.giss.nasa.gov/gistemp/
# at 2x2° (aggregate UDel to 2x2, ERSST already there, GISS already there)


library(reshape2)

############################################################
# Read in UDel ascii files and average by 2x2° within years
############################################################

# find files
files <- list.files('data/udel/air_temp_2014')

# extract years
m <- regexpr('[[:digit:]]{4}', files)
years <- as.numeric(regmatches(files, m))

# extract the data and average to 2x2
tempsarray = array(data=NA, dim=c(90, 180, length(files))) # giant array stack of all temperature data (lat, lon, year)
	dimnames(tempsarray)[[1]] = seq(-89, 89, by=2)
	dimnames(tempsarray)[[2]] = seq(-179, 179, by=2)
	dimnames(tempsarray)[[3]] = years

tmpl <- expand.grid(lat=seq(-89, 89, by=2), lon=seq(-179, 179, by=2)) # template with all lats and lons

for(i in 1:length(files)){ # for each file
	print(files[i])
	x <- read.table(paste('data/udel/air_temp_2014/', files[i], sep=''), header=FALSE)
	names(x) <- c('lon', 'lat', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

	# add 2x2 grid indicators
	x$longrid <- floor(x$lon/2)*2+1
	x$latgrid <- floor(x$lat/2)*2+1

	# average within years
	x$ave <- rowMeans(x[,c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')])
	
	# average within 2x2 grids
	x2 <- aggregate(list(ave = x$ave), by=list(lat=x$latgrid, lon=x$longrid), FUN=mean)

	# add missing lat/lons
	x3 <- merge(tmpl, x2, all.x=TRUE)

	# reshape
	x4 <- melt(x3, id.vars=c('lat', 'lon'), measure.vars=c('ave'))
	x5 <- acast(x4, lat ~ lon) # easy with reshape2!

	# add to tempsarray
	tempsarray[,,i] = x5
}

	# quick look
	dim(tempsarray)
	summary(tempsarray)
	image(t(tempsarray[,,1])) # looks great!
	image(t(tempsarray[,,100])) # looks great!
	tempsarray['1','-61',110] # tropics
	tempsarray['-89','-99',110] # antarctic
	tempsarray['89','-99',110] # arctic: missing
	tempsarray['65','-121',110] # northern canada
		dimnames(tempsarray)[[3]][110] # 2009

	range(tempsarray['-89','-97',], na.rm=TRUE) # antarctic: always cold
	range(tempsarray['65','-121',], na.rm=TRUE) # northern canada
	range(tempsarray['1','-61',], na.rm=TRUE) # tropics: warm

# Write out
save(tempsarray, file=paste('data/udel/udel2x2ann_', years[1], '_', max(years), '.rdata', sep=''))



##############################################################
# Read in GISS netcdf files (already at 2x2) and calculate annual average
##############################################################
require(ncdf4)

# get data from 2x2 grid
	# read in
n = nc_open('data/gisstemp/gistemp250.nc') # Open the netCDF file
# print(n) # get information about the file format

inarray = ncvar_get(n, 'tempanomaly') # dim order: 180 lon x 90 lat x 1651 time (observations are monthly, time measured in days since Jan 1, 1800). 0 lon is at date line.
	# dim(inarray)
dimnames(inarray)[[1]] <- ncvar_get(n, 'lon')
dimnames(inarray)[[2]] <- ncvar_get(n, 'lat')
dimnames(inarray)[[3]] <- paste(rep(1880:2017, rep(12,138)), formatC(1:12, width=2, flag=0), sep='-')[1:dim(inarray)[3]]

nc_close(n)

	# annual average
	# reshape so month a separate dimension
temp <- melt(inarray, varnames=c('lon', 'lat', 'time'), value.name='lst')
temp$year <- as.numeric(substr(temp$time, 1, 4))
temp$mo <- as.numeric(substr(temp$time, 6, 7))
temp2 <- acast(temp[temp$year %in% 1900:2014,], lat ~ lon ~ year ~ mo, value.var='lst') # dim order: lat x lon x year x mo
years <- as.numeric(dimnames(temp2)[[3]])

	# average
tempsarray <- apply(temp2, MARGIN=c(1,2,3), FUN=mean)

	# Write out
save(tempsarray, file=paste('data/gisstemp/giss2x2ann_', years[1], '_', max(years), '.rdata', sep=''))



# get data from zonal average
n = nc_open('data/gisstemp/zonmap.nc')
print(n)
gisslat <- ncvar_get(n, 'temperature') # dim order: 1404 time (monthly 1900-2017) x 46 lat
colnames(gisslat) <- ncvar_get(n, 'lat')
rownames(gisslat) <- paste(rep(1900:2017, rep(12,118)), formatC(1:12, width=2, flag=0), sep='-')[1:nrow(gisslat)]

nc_close(n)

##############################################################
# Read in ERSST ascii files (already at 2x2)
##############################################################

# find files
files <- list.files('data/ersstv4/ersst_ascii/')

# extract years
m <- regexpr('[[:digit:]]{4}', files)
years <- as.numeric(regmatches(files, m))

# extract the data (lat/lons are grid centers)
tempsarray = array(data=NA, dim=c(89, 180, length(files))) # giant array stack of all temperature data (lat, lon, year). 
	dimnames(tempsarray)[[1]] = seq(-88, 88, by=2)
	dimnames(tempsarray)[[2]] = seq(0, 358, by=2) # so grid extent is -1 to 359 (not 0 to 360)
	dimnames(tempsarray)[[3]] = years

for(i in 1:length(files)){ # for each file
	print(files[i])
	thisfile = paste('data/ersstv4/ersst_ascii/', files[i], sep='')
	x <- read.fwf(thisfile, widths=rep(6,89)) # library(readr) has a faster function
	x[x == -9999] = NA # mask out land
	x = x/100 # temperatures stored at degC * 100

	thisarray = array(data=NA, dim=c(89, 180, 12)) # array stack of temperature data (lat, lon, month). 
	for(j in 1:12){ # for each month in this file
		theselines = (180*j-179):(180*j) # one month is 180 longitude lines
		thisarray[,,j] = t(as.matrix(x[theselines,]))
	}
	tempsarray[,,i] <- apply(thisarray, MARGIN=c(1,2), FUN=mean)
}

	# quick look
	dim(tempsarray)
	image(t(tempsarray[,,110]))
	tempsarray[45,90,110] # middle of the pacific
	tempsarray[1,90,110] # antarctic
	tempsarray[89,90,110] # arctic


# Write out
save(tempsarray, file=paste('data/ersstv4/ersst2x2ann_', years[1], '_', max(years), '.rdata', sep=''))

####################################
# Calculate warming global average
####################################
# read in data from earlier
load('data/udel/udel2x2ann_1900_2014.rdata') # tempsarray
	udel <- tempsarray
load('data/ersstv4/ersst2x2ann_1900_2014.rdata') 
	ersst <- tempsarray
	
# read in GISS zonal average for comparison
giss <- read.csv('data/gisstemp/GLB.Ts.csv', skip=1, na.strings='***') #J.D has annual mean

# average, weighted by grid size
# see https://gis.stackexchange.com/questions/29734/how-to-calculate-area-of-1-x-1-degree-cells-in-a-raster
	# lst
f0 <- (as.numeric(dimnames(udel)[[1]])-1)*pi/180
f1 <- (as.numeric(dimnames(udel)[[1]])+1)*pi/180
l0 <- as.numeric(dimnames(udel)[[2]])[1]*pi/180 # all the same, so just use the first
l1 <- as.numeric(dimnames(udel)[[2]])[2]*pi/180
R <- 6371 # earth radius in km
wgt <- (sin(f1) - sin(f0)) * (l1 - l0) * R^2 # grid sizes in km for each latitudinal band
wgt <- matrix(wgt, byrow=FALSE, nrow=length(wgt), ncol=dim(udel)[2]) # matrix of weights

lstave <- apply(udel, MARGIN=3, FUN=weighted.mean, w=wgt, na.rm=TRUE)

	# sst
f0 <- (as.numeric(dimnames(ersst)[[1]])-1)*pi/180 # grid cell boundaries in radians
f1 <- (as.numeric(dimnames(ersst)[[1]])+1)*pi/180
l0 <- as.numeric(dimnames(ersst)[[2]])[1]*pi/180 # all the same, so just use the first
l1 <- as.numeric(dimnames(ersst)[[2]])[2]*pi/180
R <- 6371 # earth radius in km
wgt <- (sin(f1) - sin(f0)) * (l1 - l0) * R^2 # grid sizes in km for each latitudinal band
wgt <- matrix(wgt, byrow=FALSE, nrow=length(wgt), ncol=dim(ersst)[2]) # matrix of weights

sstave <- apply(ersst, MARGIN=3, FUN=weighted.mean, w=wgt, na.rm=TRUE)

# rates of warming (linear regression)
lstmod <- lm(I(lstave-lstclim) ~ as.numeric(names(lstave)))
sstmod <- lm(I(sstave-sstclim) ~ as.numeric(names(sstave)))
gissmod <- lm(J.D ~ Year, data=giss[giss$Year %in% 1901:2012,])

	# quick plot
	lstclim <- mean(lstave[as.numeric(names(lstave)) %in% 1961:1990]) # 1961-1990 climatology for plotting
	sstclim <- mean(sstave[as.numeric(names(sstave)) %in% 1961:1990]) # 1961-1990 climatology for plotting

	plot(as.numeric(names(lstave)), lstave-lstclim, type='l', ylim=c(-1,1), col='green')
	lines(as.numeric(names(sstave)), sstave-sstclim, col='blue')
	with(giss[giss$Year %in% 1900:2014,], lines(Year, J.D, col='dark green'))
	abline(h=0, lty=3)

	abline(lstmod, col='green', lty=2)
	abline(sstmod, col='blue', lty=2)
	abline(gissmod, col='dark green', lty=2)
	
# rates of warming (difference)
mean(lstave[as.numeric(names(lstave)) %in% 2005:2014]) - mean(lstave[as.numeric(names(lstave)) %in% 1900:1919])
mean(sstave[as.numeric(names(sstave)) %in% 2005:2014]) - mean(sstave[as.numeric(names(sstave)) %in% 1900:1919])

####################################
# Calculate warming by latitude
####################################
# read in annual gridded data from earlier
load('data/udel/udel2x2ann_1900_2014.rdata') # tempsarray
	udel <- tempsarray
load('data/ersstv4/ersst2x2ann_1900_2014.rdata') 
	ersst <- tempsarray
load('data/gisstemp/giss2x2ann_1900_2014.rdata') 
	giss <- tempsarray

# read in GISS lat averages (monthly)
n = nc_open('data/gisstemp/zonmap.nc')
gisslat <- ncvar_get(n, 'temperature') # dim order: 1404 time (monthly 1900-2017) x 46 lat
colnames(gisslat) <- ncvar_get(n, 'lat')
rownames(gisslat) <- paste(rep(1900:2017, rep(12,118)), formatC(1:12, width=2, flag=0), sep='-')[1:nrow(gisslat)]


# latitudinal average warming (1995-2014 minus 1900-1919)
	# UDel lst
udelwarm <- apply(udel[,,96:115], MARGIN=c(1,2), FUN=mean) - apply(udel[,,1:20], MARGIN=c(1,2), FUN=mean) # warming by grid cell
udelwarmave <- rowMeans(udelwarm, na.rm=TRUE) # mean across grid cells in a lat band
udelwarmsd <- apply(udelwarm, MARGIN=1, FUN=sd, na.rm=TRUE) # SD across grid cells in a lat band
udlats <- as.numeric(names(udelwarmave)) # extract latitudes

	# ERSST sst
ersstwarm <- apply(ersst[,,96:115], MARGIN=c(1,2), FUN=mean) - apply(ersst[,,1:20], MARGIN=c(1,2), FUN=mean)
ersstwarmave <- rowMeans(ersstwarm, na.rm=TRUE)
ersstwarmsd <- apply(ersstwarm, MARGIN=1, FUN=sd, na.rm=TRUE)
erlats <- as.numeric(names(ersstwarmave))

	# GISS
gisswarm <- apply(giss[,,96:115], MARGIN=c(1,2), FUN=mean, na.rm=TRUE) - apply(giss[,,1:20], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
gisswarmave <- rowMeans(gisswarm, na.rm=TRUE)
gisswarmsd <- apply(gisswarm, MARGIN=1, FUN=sd, na.rm=TRUE)
gslats <- as.numeric(names(gisswarmave))

	# GISS zonal average lst (no SD possible from zonal average)
gisszonewarmave <- apply(gisslat[1141:1380,], MARGIN=2, FUN=mean) - apply(gisslat[1:240,], MARGIN=2, FUN=mean) # warming by lat band
gszonelats <- as.numeric(names(gisszonewarmave))


# write out
write.csv(data.frame(lat=udlats, warm=udelwarmave, sd=udelwarmsd), file='temp/lstwarmlat.csv', row.names=FALSE)
write.csv(data.frame(lat=gslats, warm=gisswarmave, sd=gisswarmsd), file='temp/lstwarmlat_giss.csv', row.names=FALSE)
write.csv(data.frame(lat=gszonelats, warm=gisszonewarmave), file='temp/lstwarmlat_gisszone.csv', row.names=FALSE)
write.csv(data.frame(lat=erlats, warm=ersstwarmave, sd=ersstwarmsd), file='temp/sstwarmlat.csv', row.names=FALSE)

###########################
# plots
###########################
lstwarmlat <- read.csv('temp/lstwarmlat.csv')
sstwarmlat <- read.csv('temp/sstwarmlat.csv')
lstwarmlatgiss <- read.csv('temp/lstwarmlat_giss.csv')
lstwarmlatgisszone <- read.csv('temp/lstwarmlat_gisszone.csv')
	
# warming (1995-2014 minus 1900-1919) by latitude	
	cols <- c('#b2df8a', '#1f78b4', '#222222') # land, ocean, GISS
	cols2 <- c('#b2df8a55', '#1f78b455', '#22222255') # land, ocean partially transparent
	lw <- 2 # line width

	i.1 <- complete.cases(lstwarmlat) & lstwarmlat$lat < -66.25 # for polygon plotting
	i.2 <- complete.cases(lstwarmlat) & lstwarmlat$lat > -52.75 # for polygon plotting
	i2.1 <- complete.cases(lstwarmlatgiss) & lstwarmlatgiss$lat < -66.25 # for polygon plotting
	i2.2 <- complete.cases(lstwarmlatgiss) & lstwarmlatgiss$lat > -52.75 # for polygon plotting
	i3 <- complete.cases(sstwarmlat)

	quartz(w=4,h=4)
	plot(0, 0, xlab='Latitude (°N)', ylab='Historical warming (°C)', ylim=c(-1.5,3.5), xlim=c(-90,90), type='n') # just to set it up
	with(lstwarmlat[i.1,], polygon(c(lat, rev(lat)), c(warm+sd, rev(warm-sd)), col=cols2[1], border=NA))
	with(lstwarmlat[i.2,], polygon(c(lat, rev(lat)), c(warm+sd, rev(warm-sd)), col=cols2[1], border=NA))
	with(lstwarmlatgiss[i2.1,], polygon(c(lat, rev(lat)), c(warm+sd, rev(warm-sd)), col=cols2[3], border=NA))
	with(lstwarmlatgiss[i2.2,], polygon(c(lat, rev(lat)), c(warm+sd, rev(warm-sd)), col=cols2[3], border=NA))
	with(sstwarmlat[i3,], polygon(c(lat, rev(lat)), c(warm+sd, rev(warm-sd)), col=cols2[2], border=NA))
	with(lstwarmlat, lines(lat, warm, col=cols[1], lwd=lw))
	with(lstwarmlatgiss, lines(lat, warm, col=cols[3], lwd=lw))
	with(sstwarmlat, lines(lat, warm, col=cols[2], lwd=lw))

	with(lstwarmlatgisszone, lines(lat, warm, col='black', lwd=1, lty=2))