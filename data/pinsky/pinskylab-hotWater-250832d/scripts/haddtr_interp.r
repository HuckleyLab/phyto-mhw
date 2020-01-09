require(ncdf4)
require(abind)

##########################################################	
# Interpolate HadDTR to all values in CMIP5 at 1.25x1.25
##########################################################
# Read in DTR
load('temp/dtrclimatology1.25.rdata') # dtr125 at 1.25x1.25
# load('dtrclimatology1.25.rdata') # dtr125 at 1.25x1.25 (Amphiprion)

# Read in CMIP5
infile <- nc_open('data_dl/cmip5/tos_Omon_modmean_rcp85_ave.nc') # tos for each month
# infile <- nc_open('tos_Omon_modmean_rcp85_ave.nc') # tos for each month (Amphiprion)
	# print(infile)
	tos <- ncvar_get(infile, 'tos', start=c(1,1,1,1), count=c(-1,-1,1,1)) # tos 288 x 144 x 2800 lon x lat x months 1861-2100 (level is 4th dimension, length 1)
	dim(tos)
	dimnames(tos) <- list(lon=1:288, lat=1:144)
	dimnames(tos)[[1]] <- infile$var[['tos']]$dim[[1]]$vals # add lon
	dimnames(tos)[[2]] <- infile$var[['tos']]$dim[[2]]$vals # add lat
	nc_close(infile)


# rotate cmip5 columns 0 -> 360 lon (centered at Greenwich), rows 90 -> -90 lat (like cmip5)
tos <- aperm(tos, c(2,1))

lats <- as.numeric(rownames(tos))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)
tos <- tos[newlatord, ]

#	image(tos[,])

# make tos 12 layers to match dtr125
tosl <- abind(list(tos)[rep(1,12)], along=3)
	dim(tosl)


# interpolate dtr to all values in tos
# PARALLEL version. Takes an hour maybe?
require(parallel)
require(iterators)
require(doMC)
require(doParallel)
require(doSNOW)

	# start cluster
cl <- makeCluster(rep('localhost', 3), type='SOCK') # make the cluster on the localhost
# cl <- makeCluster(rep('localhost', 20), type='SOCK') # make the cluster on the localhost (Amphiprion)
registerDoSNOW(cl) # register the cluster so accessible to foreach

	# set up parameters
rg <- 30 # how many lat/lon grids in each direction to average over
inds <- as.matrix(which(is.na(dtr125) & !is.na(tosl), arr.ind=TRUE)) # find missing values that shouldn't be missing
	nrow(inds) # 43462
whichinds <- 1:nrow(inds) # if I only want to run some rows of inds
#whichinds <- 1:10 # if I only want to run some rows of inds
	length(whichinds)
iterator <- iter(inds[whichinds,], by='row') # make an iterator object to step through
pb <- txtProgressBar(max = length(whichinds), style = 3) # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

	# run loop
res <- foreach(i=iterator, .combine=c, .options.snow=opts) %dopar% {
	averng <- expand.grid(lat=(i[,1]-rg):(i[,1]+rg), lon=(i[,2]-rg):(i[,2]+rg)) # define grid over which to average
	averng <- averng[averng$lat>0 & averng$lat<dim(dtr125)[1] & averng$lon>0 & averng$lon<dim(dtr125)[2],] # remove out-of-range indices into dtr
	averng$mo <- i[,3] # average within same mo and year
	averng$w <- sqrt((averng$lat-i[,1])^2 + (averng$lon-i[,2])^2) # calculate Euclidean distance to each grid cell
	averng <- averng[averng$w>0,] # trim out self-reference
	averng <- as.matrix(averng) # so that we can use it as an index
	av <- weighted.mean(x=dtr125[averng[,1:3]], w=1/averng[,4], na.rm=TRUE) # inverse distance weighting. use vectorized selection with first 4 columns of averng
	return(av) 
}

	# stop cluster
	close(pb)
	stopCluster(cl)
	
	# how many missing?
	sum(is.na(res))

	# add results back into array
	dtr125[inds[whichinds,,drop=FALSE]] <- res # drop=FALSE so that 1-row matrix would still be a matrix we can use for vectorized selection

	# set NAs in tosl to missing in DTR
	nas <- is.na(tosl)
	dtr125[nas] <- NA

	# check if any values still missing
	sum(is.na(dtr125))
	sum(is.na(tosl)) # should match: not quite
	sum(!is.na(tosl) & is.na(dtr125)) # dtr125 is missing 24
	
		image(dtr125[,,1])
		image(tos)

save(dtr125, file='temp/dtrclimatology_interp1.25.rdata')
# save(dtr125, file='dtrclimatology_interp1.25.rdata') # Amphiprion



##########################################################	
# Interpolate HadDTR to all values in OISST at 0.5x0.5
##########################################################
# Read in
load('temp/oisst0.5deg_1986_2005.rdata') # tempsarray at 0.5x0.5 1986-2005
load('temp/dtrclimatology.rdata') # dtr at 0.5x0.5

nms <- dimnames(tempsarray)
mos <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(2,length(nms[[3]])*2,by=2)]) # months
yrs <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(1,length(nms[[3]])*2,by=2)]) # years
dm <- dim(tempsarray)
tempsarraybymo <- array(tempsarray, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=sort(unique(yrs)))) # add a month dimension (3rd dimension)

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

sstbymo <- tempsarraybymo[newlatord, newlonord,,] # lat x lon (0-360) x month x year
dimnames(sstbymo)[[2]] <- newcolnms

	image(sstbymo[,,1,1], xaxt='n', yaxt='n')
		axis(side=1, at=seq(0,1,length=nrow(sstbymo)), labels=rownames(sstbymo), cex.axis=0.4)
		axis(side=2, at=seq(0,1,length=ncol(sstbymo)), labels=colnames(sstbymo), cex.axis=0.4)

# interpolate dtr to all values in sstbymo
# PARALLEL version. Takes an hour maybe?
require(parallel)
require(iterators)
require(doMC)
require(doParallel)
require(doSNOW)

	# start cluster
cl <- makeCluster(rep('localhost', 3), type='SOCK') # make the cluster on the localhost
registerDoSNOW(cl) # register the cluster so accessible to foreach

	# set up parameters
rg <- 75 # how many lat/lon grids in each direction to average over
inds <- as.matrix(which(is.na(dtr) & !is.na(sstbymo[,,,1]), arr.ind=TRUE)) # find missing values that shouldn't be missing
	nrow(inds) # 231096
whichinds <- 1:nrow(inds) # if I only want to run some rows of inds
#whichinds <- 1:10 # if I only want to run some rows of inds
	length(whichinds)
iterator <- iter(inds[whichinds,], by='row') # make an iterator object to step through
pb <- txtProgressBar(max = length(whichinds), style = 3) # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

	# run loop
res <- foreach(i=iterator, .combine=c, .options.snow=opts) %dopar% {
	averng <- expand.grid(lat=(i[,1]-rg):(i[,1]+rg), lon=(i[,2]-rg):(i[,2]+rg)) # define grid over which to average
	averng <- averng[averng$lat>0 & averng$lat<dim(dtr)[1] & averng$lon>0 & averng$lon<dim(dtr)[2],] # remove out-of-range indices into dtr
	averng$mo <- i[,3] # average within same mo and year
	averng$w <- sqrt((averng$lat-i[,1])^2 + (averng$lon-i[,2])^2) # calculate Euclidean distance to each grid cell
	averng <- averng[averng$w>0,] # trim out self-reference
	averng <- as.matrix(averng) # so that we can use it as an index
	av <- weighted.mean(x=dtr[averng[,1:3]], w=1/averng[,4], na.rm=TRUE) # inverse distance weighting. use vectorized selection with first 4 columns of averng
	return(av) 
}

	# stop cluster
	close(pb)
	stopCluster(cl)
	
	# how many missing?
	sum(is.na(res))

	# add results back into array
	dtr[inds[whichinds,,drop=FALSE]] <- res # drop=FALSE so that 1-row matrix would still be a matrix we can use for vectorized selection

	# set NAs in sstbymo to missing in DTR
	nas <- is.na(sstbymo[,,,1])
	dtr[nas] <- NA

	# check if any values still missing
	sum(is.na(dtr))
	sum(is.na(sstbymo[,,,1])) # should match: dtr is missing 2
	
		image(dtr[,,1])
		image(sstbymo[,,1,1])

save(dtr, file='temp/dtrclimatology_interp.rdata')



##########################################################	
# Interpolate HadDTR to all values in OISST at 0.25x0.25
##########################################################
# Read in (for MacBook)
load('temp/oisst1986_1990.rdata') # tempsarray at 0.25x0.25 1986-1990
load('temp/dtrclimatology0.25.rdata') # dtr025
ncores=3

# Read in (if on Amphiprion)
# load('oisst1986_1990.rdata') # tempsarray at 0.25x0.25 1986-1990
# load('dtrclimatology0.25.rdata') # dtr025
# ncores=20

nms <- dimnames(tempsarray)
mos <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(2,length(nms[[3]])*2,by=2)]) # months
yrs <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(1,length(nms[[3]])*2,by=2)]) # years
dm <- dim(tempsarray)
tempsarraybymo <- array(tempsarray, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=sort(unique(yrs)))) # add a month dimension (3rd dimension)

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

sstbymo <- tempsarraybymo[newlatord, newlonord,,] # lat x lon (0-360) x month x year
dimnames(sstbymo)[[2]] <- newcolnms

	image(sstbymo[,,1,1], xaxt='n', yaxt='n')
		axis(side=1, at=seq(0,1,length=nrow(sstbymo)), labels=rownames(sstbymo), cex.axis=0.4)
		axis(side=2, at=seq(0,1,length=ncol(sstbymo)), labels=colnames(sstbymo), cex.axis=0.4)

# interpolate dtr to all values in sstbymo
# PARALLEL version. Takes an hour maybe?
require(parallel)
require(iterators)
require(doMC)
require(doParallel)
require(doSNOW)

	# start cluster
cl <- makeCluster(rep('localhost', ncores), type='SOCK') # make the cluster on the localhost
registerDoSNOW(cl) # register the cluster so accessible to foreach

	# set up parameters
rg <- 150 # how many lat/lon grids in each direction to average over
inds <- as.matrix(which(is.na(dtr025) & !is.na(sstbymo[,,,1]), arr.ind=TRUE)) # find missing values that shouldn't be missing
	nrow(inds) # 972382
whichinds <- 1:nrow(inds) # if I only want to run some rows of inds
#whichinds <- 1:10 # if I only want to run some rows of inds
	length(whichinds)
iterator <- iter(inds[whichinds,], by='row') # make an iterator object to step through
pb <- txtProgressBar(max = length(whichinds), style = 3) # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

	# run loop
res <- foreach(i=iterator, .combine=c, .options.snow=opts) %dopar% {
	averng <- expand.grid(lat=(i[,1]-rg):(i[,1]+rg), lon=(i[,2]-rg):(i[,2]+rg)) # define grid over which to average
	averng <- averng[averng$lat>0 & averng$lat<dim(dtr025)[1] & averng$lon>0 & averng$lon<dim(dtr025)[2],] # remove out-of-range indices into dtr025
	averng$mo <- i[,3] # average within same mo and year
	averng$w <- sqrt((averng$lat-i[,1])^2 + (averng$lon-i[,2])^2) # calculate Euclidean distance to each grid cell
	averng <- averng[averng$w>0,] # trim out self-reference
	averng <- as.matrix(averng) # so that we can use it as an index
	av <- weighted.mean(x=dtr025[averng[,1:3]], w=1/averng[,4], na.rm=TRUE) # inverse distance weighting. use vectorized selection with first 4 columns of averng
	return(av) 
}

	# stop cluster
	close(pb) # the progress bar
	stopCluster(cl)
	
	# how many missing?
	sum(is.na(res)) # 590

	# add results back into array
	dtr025[inds[whichinds,,drop=FALSE]] <- res # drop=FALSE so that 1-row matrix would still be a matrix we can use for vectorized selection

	# set NAs in sstbymo to missing in dtr025
	nas <- is.na(sstbymo[,,,1])
	dtr025[nas] <- NA

	# check if any values still missing
	sum(is.na(dtr025))
	sum(is.na(sstbymo[,,,1])) # should match
	sum(!is.na(sstbymo[,,,1]) & is.na(dtr025)) # dtr025 is missing 590
	
		image(dtr025[,,1])
		image(sstbymo[,,1,1])

save(dtr025, file='temp/dtrclimatology_interp0.25.rdata')
# save(dtr025, file='dtrclimatology_interp0.25.rdata') # on Amphiprion
