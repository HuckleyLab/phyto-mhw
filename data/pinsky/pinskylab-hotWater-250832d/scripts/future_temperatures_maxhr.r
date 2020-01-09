# Calculate warming of the hottest hour
# Also calculate hottest future hours (add to climatology)


require(ncdf4)
require(abind)
require(raster)
require(data.table)

###################
## Read in data
###################

# read in TXx rcp26
infile <- nc_open('data_dl/cmip5/txx_yr_modmean_rcp26_ave.nc') # tasmax for each month
	# print(infile)
	tasmax26 <- ncvar_get(infile, 'txxETCCDI') # 144 x 72 x 240 lon x lat x years 1861-2100)
	dim(tasmax26)
	dimnames(tasmax26) <- list(lon=1:144, lat=1:72, time=1:240)
	dimnames(tasmax26)[[1]] <- infile$var[['txxETCCDI']]$dim[[1]]$vals # add lon
	dimnames(tasmax26)[[2]] <- infile$var[['txxETCCDI']]$dim[[2]]$vals # add lat
	dimnames(tasmax26)[[3]] <- 1861:2100 # year of the CMIP5 time dimension
	nc_close(infile)


# read in tos rcp26
infile <- nc_open('data_dl/cmip5/tos_Omon_modmean_rcp26_ave.nc') # tos for each month
	# print(infile)
	tos26 <- ncvar_get(infile, 'tos') # tos 288 x 144 x 2800 lon x lat x months 1861-2100
	dim(tos26)
	dimnames(tos26) <- list(lon=1:288, lat=1:144, time=1:2880)
	dimnames(tos26)[[1]] <- infile$var[['tos']]$dim[[1]]$vals # add lon
	dimnames(tos26)[[2]] <- infile$var[['tos']]$dim[[2]]$vals # add lat
	dimnames(tos26)[[3]] <- paste(rep(1861:2100, rep(12, length(1861:2100))), paste('_', 1:12, sep=''), sep='') # year and month of the CMIP5 time dimension
	nc_close(infile)

# read in TXx rcp85
infile <- nc_open('data_dl/cmip5/txx_yr_modmean_rcp85_ave.nc') # tasmax for each month
	# print(infile)
	tasmax85 <- ncvar_get(infile, 'txxETCCDI') # 144 x 72 x 240 lon x lat x years 1861-2100)
	dim(tasmax85)
	dimnames(tasmax85) <- list(lon=1:144, lat=1:72, time=1:240)
	dimnames(tasmax85)[[1]] <- infile$var[['txxETCCDI']]$dim[[1]]$vals # add lon
	dimnames(tasmax85)[[2]] <- infile$var[['txxETCCDI']]$dim[[2]]$vals # add lat
	dimnames(tasmax85)[[3]] <- 1861:2100 # year of the CMIP5 time dimension
	nc_close(infile)


# read in tos rcp85
infile <- nc_open('data_dl/cmip5/tos_Omon_modmean_rcp85_ave.nc') # tos for each month
	# print(infile)
	tos85 <- ncvar_get(infile, 'tos') # tos 288 x 144 x 2800 lon x lat x months 1861-2100
	dim(tos85)
	dimnames(tos85) <- list(lon=1:288, lat=1:144, time=1:2880)
	dimnames(tos85)[[1]] <- infile$var[['tos']]$dim[[1]]$vals # add lon
	dimnames(tos85)[[2]] <- infile$var[['tos']]$dim[[2]]$vals # add lat
	dimnames(tos85)[[3]] <- paste(rep(1861:2100, rep(12, length(1861:2100))), paste('_', 1:12, sep=''), sep='') # year and month of the CMIP5 time dimension
	nc_close(infile)

# read in DTR (daily temperature range)
load('temp/dtrclimatology_interp1.25.rdata') # dtr125


# read in max hr climatologies
load('temp/tosmax.rdata') # tosmax 0.25x0.25 from OISST daily + HadDTR
load('temp/hadex2_txx.rdata') # hadex2txx 2.5x3.75 from HadEX2
	tasmax <- hadex2txx

##################################################################################
# permute CMIP5 dimensions: lat x lon x time
# lat 90 to -90; lon 0 to 360
##################################################################################
# change dimension order
tasmax26 <- aperm(tasmax26, c(2,1,3))
tasmax85 <- aperm(tasmax85, c(2,1,3))
tos26 <- aperm(tos26, c(2,1,3))
tos85 <- aperm(tos85, c(2,1,3))

# rotate so columns 0 -> 360 lon, rows 90 -> -90 lat
	# tasmax26
lons <- as.numeric(colnames(tasmax26)) # good

lats <- as.numeric(rownames(tasmax26))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

tasmax26 <- tasmax26[newlatord, ,]

	#image(tasmax26[,,1]) # north pole should be at the left, America at the top

	# tasmax85
lons <- as.numeric(colnames(tasmax85)) # good

lats <- as.numeric(rownames(tasmax85))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

tasmax85 <- tasmax85[newlatord, ,]

	#image(tasmax85[,,1])

	# tos26
lons <- as.numeric(colnames(tos26)) # good

lats <- as.numeric(rownames(tos26))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

tos26 <- tos26[newlatord, ,]

	#image(tos26[,,1])

	# tos85
lons <- as.numeric(colnames(tos85)) # good

lats <- as.numeric(rownames(tos85))
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

tos85 <- tos85[newlatord, ,]

	#image(tos85[,,1])

############################
# Compute tosmax26 and 86 (CMIP5 temperatures)
# Irrelevant since we compute a delta?
############################
# extend dtr125 so that it matches the dimensions of tos85
dtr125l <- abind(list(dtr125)[rep(1,dim(tos85)[3]/dim(dtr125)[3])], along=3)
	dim(dtr125l)
	dim(tos26)
	dim(tos85)

# add to get daily maximum
tosmax26 <- tos26 + 0.5*dtr125l
tosmax85 <- tos85 + 0.5*dtr125l


#############################################
# Calculate CMIP5 anomalies from 1986-2005
#############################################
climtime <- as.character(1986:2005) # year of the climatology period
futtime <- as.character(2006:2100) # year the future period

# Reshape CMIP5 to lat x lon x month x year
	# not needed for tasmax26 and tasmax85 since already annual TXx
	
	# tosmax26
nms <- dimnames(tosmax26)
mos <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(2,length(nms[[3]])*2,by=2)]) # months
yrs <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(1,length(nms[[3]])*2,by=2)]) # years
dm <- dim(tosmax26)
tosmax26bymo <- array(tosmax26, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=sort(unique(yrs)))) # add a month dimension (3rd dimension)

	# tosmax85
nms <- dimnames(tosmax85)
mos <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(2,length(nms[[3]])*2,by=2)]) # months
yrs <- as.numeric(unlist(strsplit(nms[[3]], split='_'))[seq(1,length(nms[[3]])*2,by=2)]) # years
dm <- dim(tosmax85)
tosmax85bymo <- array(tosmax85, dim=c(dm[1], dm[2], 12, dm[3]/12), dimnames=list(lat=nms[[1]], lon=nms[[2]], mo=1:12, year=sort(unique(yrs)))) # add a month dimension (3rd dimension)


# find highest daily max within years (TXx)
tasmax26yr <- tasmax26 # already annual TXx on land (HadEX2)
tasmax85yr <- tasmax85
tosmax26yr <- apply(tosmax26bymo, MARGIN=c(1,2,4), FUN=max)
tosmax85yr <- apply(tosmax85bymo, MARGIN=c(1,2,4), FUN=max)

# calculate climatology as the highest TXx across 20-year chunk
tasmax26clim <- apply(tasmax26yr[,,climtime], MARGIN=c(1,2), FUN=max) # climatological average for CMIP5
tasmax26climlong <- abind(list(tasmax26clim)[rep(1,length(futtime))], along=3) # expand to match future projections
	dim(tasmax26climlong) 

tasmax85clim <- apply(tasmax85yr[,,climtime], MARGIN=c(1,2), FUN=max) # climatological average for CMIP5
tasmax85climlong <- abind(list(tasmax85clim)[rep(1,length(futtime))], along=3) # expand to match future projections
	dim(tasmax85climlong) 

tosmax26clim <- apply(tosmax26yr[,,climtime], MARGIN=c(1,2), FUN=max)
tosmax26climlong <- abind(list(tosmax26clim)[rep(1,length(futtime))], along=3)
	dim(tosmax26climlong)

tosmax85clim <- apply(tosmax85yr[,,climtime], MARGIN=c(1,2), FUN=max)
tosmax85climlong <- abind(list(tosmax85clim)[rep(1,length(futtime))], along=3)
	dim(tosmax85climlong)


# calculate deltas by years
tasmax26dyr <- tasmax26yr[,,futtime] - tasmax26climlong # CMIP5 deltas
tasmax85dyr <- tasmax85yr[,,futtime] - tasmax85climlong # CMIP5 deltas
tosmax26dyr <- tosmax26yr[,,futtime] - tosmax26climlong
tosmax85dyr <- tosmax85yr[,,futtime] - tosmax85climlong


# write out for use in future_tsm_byyear.r
save(tasmax26dyr, file='temp/tasmax26dyr.rdata')
save(tasmax85dyr, file='temp/tasmax85dyr.rdata')
save(tosmax26dyr, file='temp/tosmax26dyr.rdata')
save(tosmax85dyr, file='temp/tosmax85dyr.rdata')


#######################################################
# average warming by latitude for 2081-2100 and write out
# for use in figure
#######################################################
# Make a "not ocean" mask
searast <- raster(!is.na(tos26[,,1]), xmn=0, xmx=360, ymn=-90, ymx=90) # 1 for ocean, 0 for land
landrast <- raster(tasmax26dyr[,,1], xmn=0, xmx=360, ymn=-90, ymx=90)
seamask <- round(resample(searast, landrast, method='bilinear')) # includes areas >=50% land as land (floor): 0 is land, 1 is ocean
seamask <- as.matrix(seamask)
seamask[seamask==1] <- NA # turn ocean to NA
seamask <- abind(list(seamask)[rep(1,dim(tasmax26dyr)[3])], along=3) # expand to match future projections

# mask out ocean for terrestrial air temp
tasmax26dyrmask <- tasmax26dyr + seamask
tasmax85dyrmask <- tasmax85dyr + seamask

# data.frame to hold results
lstwarmingbylat <- data.table(lat=as.numeric(dimnames(tasmax26dyr)[[1]]))
sstwarmingbylat <- data.table(lat=as.numeric(dimnames(tosmax26dyr)[[1]]))

# average by lat
lstwarmingbylat$tasmax26 <- apply(tasmax26dyrmask[,,as.character(2081:2100)], MARGIN=1, FUN=mean, na.rm=TRUE)
lstwarmingbylat$tasmax85 <- apply(tasmax85dyrmask[,,as.character(2081:2100)], MARGIN=1, FUN=mean, na.rm=TRUE)
sstwarmingbylat$tosmax26 <- apply(tosmax26dyr[,,as.character(2081:2100)], MARGIN=1, FUN=mean, na.rm=TRUE)
sstwarmingbylat$tosmax85 <- apply(tosmax85dyr[,,as.character(2081:2100)], MARGIN=1, FUN=mean, na.rm=TRUE)
	
	# plot to check
	lstwarmingbylat[,plot(lat, tasmax26, type='l', ylim=c(0,6))]
	lstwarmingbylat[,lines(lat, tasmax85, lty=2)]
	sstwarmingbylat[,lines(lat, tosmax26, col='blue')]
	sstwarmingbylat[,lines(lat, tosmax85, col='blue', lty=2)]

# SD by lat
lstwarmingbylat$tasmax26sd <- apply(tasmax26dyrmask[,,as.character(2081:2100)], MARGIN=1, FUN=sd, na.rm=TRUE)
lstwarmingbylat$tasmax85sd <- apply(tasmax85dyrmask[,,as.character(2081:2100)], MARGIN=1, FUN=sd, na.rm=TRUE)
sstwarmingbylat$tosmax26sd <- apply(tosmax26dyr[,,as.character(2081:2100)], MARGIN=1, FUN=sd, na.rm=TRUE)
sstwarmingbylat$tosmax85sd <- apply(tosmax85dyr[,,as.character(2081:2100)], MARGIN=1, FUN=sd, na.rm=TRUE)

# Sample size by lat
lstwarmingbylat$tasmax26n <- apply(tasmax26dyrmask[,,as.character(2081)], MARGIN=1, FUN=function(x) sum(!is.na(x)))
lstwarmingbylat$tasmax85n <- apply(tasmax85dyrmask[,,as.character(2081)], MARGIN=1, FUN=function(x) sum(!is.na(x)))
sstwarmingbylat$tosmax26n <- apply(tosmax26dyr[,,as.character(2081)], MARGIN=1, FUN=function(x) sum(!is.na(x)))
sstwarmingbylat$tosmax85n <- apply(tosmax85dyr[,,as.character(2081)], MARGIN=1, FUN=function(x) sum(!is.na(x)))


# write out
write.csv(lstwarmingbylat, file='temp/warmingmaxhr_bylat_land.csv')
write.csv(sstwarmingbylat, file='temp/warmingmaxhr_bylat_ocean.csv')



##################################################################
# Add deltas to climatology to get future temperatures 2081-2100
##################################################################
# average deltas for 2081-2100
tasmax26d2100 <- apply(tasmax26dyr[,,dimnames(tasmax26dyr)[[3]] %in% 2081:2100], MARGIN=c(1,2), FUN=max)
tasmax85d2100 <- apply(tasmax85dyr[,,dimnames(tasmax85dyr)[[3]] %in% 2081:2100], MARGIN=c(1,2), FUN=max)
tosmax26d2100 <- apply(tosmax26dyr[,,dimnames(tosmax26dyr)[[3]] %in% 2081:2100], MARGIN=c(1,2), FUN=max)
tosmax85d2100 <- apply(tosmax85dyr[,,dimnames(tosmax85dyr)[[3]] %in% 2081:2100], MARGIN=c(1,2), FUN=max)

# create rasters for interpolation
tasmax26d2100r <- raster(tasmax26d2100, xmn=0, xmx=360, ymn=-90, ymx=90)
tasmax85d2100r <- raster(tasmax85d2100, xmn=0, xmx=360, ymn=-90, ymx=90)
tosmax26d2100r <- raster(tosmax26d2100, xmn=0, xmx=360, ymn=-90, ymx=90)
tosmax85d2100r <- raster(tosmax85d2100, xmn=0, xmx=360, ymn=-90, ymx=90)

tasmaxr <- raster(tasmax, xmn=0, xmx=360, ymn=-90, ymx=90)
tosmaxr <- raster(tosmax, xmn=0, xmx=360, ymn=-90, ymx=90)

# interpolate to resolution of the climatology (2.5x3.75 on land, 0.25x0.25 in ocean)
tasmax26d2100_025r <- resample(tasmax26d2100r, tasmaxr, method='bilinear')
tasmax85d2100_025r <- resample(tasmax85d2100r, tasmaxr, method='bilinear')
tosmax26d2100_025r <- resample(tosmax26d2100r, tosmaxr, method='bilinear')
tosmax85d2100_025r <- resample(tosmax85d2100r, tosmaxr, method='bilinear')

# add deltas to climatologies
tasmax26_2100r <- tasmaxr + tasmax26d2100_025r
tasmax85_2100r <- tasmaxr + tasmax85d2100_025r
tosmax26_2100r <- tosmaxr + tosmax26d2100_025r
tosmax85_2100r <- tosmaxr + tosmax85d2100_025r

# convert to matrices
tasmax26_2100 <- as.matrix(tasmax26_2100r)
tasmax85_2100 <- as.matrix(tasmax85_2100r)
tosmax26_2100 <- as.matrix(tosmax26_2100r)
tosmax85_2100 <- as.matrix(tosmax85_2100r)



#############################################
# Average future temperatures by latitude
#############################################

# data.frame to hold results
lstmaxhrlat_2100 <- data.table(lat=yFromRow(tasmax26_2100r, 1:nrow(tasmax26_2100r)))
sstmaxhrlat_2100 <- data.table(lat=yFromRow(tosmax26_2100r,1:nrow(tosmax26_2100r)))

# average by lat
lstmaxhrlat_2100$tasmax26 <- apply(tasmax26_2100, MARGIN=1, FUN=mean, na.rm=TRUE)
lstmaxhrlat_2100$tasmax85 <- apply(tasmax85_2100, MARGIN=1, FUN=mean, na.rm=TRUE)
sstmaxhrlat_2100$tosmax26 <- apply(tosmax26_2100, MARGIN=1, FUN=mean, na.rm=TRUE)
sstmaxhrlat_2100$tosmax85 <- apply(tosmax85_2100, MARGIN=1, FUN=mean, na.rm=TRUE)
	
	# plot to check
	lstmaxhrlat_2100[,plot(lat, tasmax26, type='l', ylim=c(0,50))]
	lstmaxhrlat_2100[,lines(lat, tasmax85, lty=2)]
	sstmaxhrlat_2100[,lines(lat, tosmax26, col='blue')]
	sstmaxhrlat_2100[,lines(lat, tosmax85, col='blue', lty=2)]

# SD by lat
lstmaxhrlat_2100$tasmax26sd <- apply(tasmax26_2100, MARGIN=1, FUN=sd, na.rm=TRUE)
lstmaxhrlat_2100$tasmax85sd <- apply(tasmax85_2100, MARGIN=1, FUN=sd, na.rm=TRUE)
sstmaxhrlat_2100$tosmax26sd <- apply(tosmax26_2100, MARGIN=1, FUN=sd, na.rm=TRUE)
sstmaxhrlat_2100$tosmax85sd <- apply(tosmax85_2100, MARGIN=1, FUN=sd, na.rm=TRUE)

# Sample size by lat
lstmaxhrlat_2100$tasmax26n <- apply(tasmax26_2100, MARGIN=1, FUN=function(x) sum(!is.na(x)))
lstmaxhrlat_2100$tasmax85n <- apply(tasmax85_2100, MARGIN=1, FUN=function(x) sum(!is.na(x)))
sstmaxhrlat_2100$tosmax26n <- apply(tosmax26_2100, MARGIN=1, FUN=function(x) sum(!is.na(x)))
sstmaxhrlat_2100$tosmax85n <- apply(tosmax85_2100, MARGIN=1, FUN=function(x) sum(!is.na(x)))

# write out
write.csv(lstmaxhrlat_2100, file='temp/lstmaxhrlat_2081-2100.csv')
write.csv(sstmaxhrlat_2100, file='temp/sstmaxhrlat_2081-2100.csv')
