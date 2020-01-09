require(ncdf4)

###########
## Read in warming (all calculations)
##########

# read in monthly warming rcp26
infile <- nc_open('data/cmip5/d20161206_1632_19653.nc') # deltas for each month, 2081-2100 minus 1986-2005
	lstwarm26 <- ncvar_get(infile, 'diff') # lst warming  x  x 12 (last is months)
	dim(lstwarm26)
	dimnames(lstwarm26) <- list(lon=1:144, lat=1:72, mo3=1:12)
	dimnames(lstwarm26)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(lstwarm26)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	lstwarm26 <- lstwarm26[,dim(lstwarm26)[2]:1,]

infile <- nc_open('data/cmip5/d20161206_1630_11885.nc') # deltas for each month, 2081-2100 minus 1986-2005
	sstwarm26 <- ncvar_get(infile, 'diff') # sst warming 288 x 144 x 12 (last is months)
	dim(sstwarm26)
	dimnames(sstwarm26) <- list(lon=1:288, lat=1:144, mo3=1:12) # dummy dimnames for lat and lon
	dimnames(sstwarm26)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(sstwarm26)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	sstwarm26 <- sstwarm26[,dim(sstwarm26)[2]:1,] # rearrange


# read in monthly warming rcp85
infile <- nc_open('data/cmip5/d20161127_1456_12434.nc') # deltas for each month, 2081-2100 minus 1986-2005
	lstwarm85 <- ncvar_get(infile, 'diff') # lst warming  x  x 12 (last is months)
	dim(lstwarm85)
	dimnames(lstwarm85) <- list(lon=1:144, lat=1:72, mo3=1:12)
	dimnames(lstwarm85)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(lstwarm85)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	lstwarm85 <- lstwarm85[,dim(lstwarm85)[2]:1,]

infile <- nc_open('data/cmip5/d20161127_1011_30742.nc') # deltas for each month, 2081-2100 minus 1986-2005
	sstwarm85 <- ncvar_get(infile, 'diff') # sst warming 288 x 144 x 12 (last is months)
	dim(sstwarm85)
	dimnames(sstwarm85) <- list(lon=1:288, lat=1:144, mo3=1:12) # dummy dimnames for lat and lon
	dimnames(sstwarm85)[[1]] <- infile$var[['diff']]$dim[[1]]$vals # add lon
	dimnames(sstwarm85)[[2]] <- infile$var[['diff']]$dim[[2]]$vals # add lat
	nc_close(infile)
	sstwarm85 <- sstwarm85[,dim(sstwarm85)[2]:1,] # rearrange


################
## For annual mean
################

# read in climatologies for annual mean
load('temp/lstclimatology.rdata') # lstclim
load('temp/sstclimatology.rdata') # sstclim

# calculate warming 
	lstwarm26ann <- t(apply(lstwarm26, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))
	lstwarm85ann <- t(apply(lstwarm85, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))
	sstwarm26ann <- t(apply(sstwarm26, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))
	sstwarm85ann <- t(apply(sstwarm85, MARGIN=c(1,2), FUN=mean, na.rm=TRUE))

# interpolate warming to 0.5x0.5
	# create new grid
	ngrd <- expand.grid(lat=as.numeric(rownames(lstclim)), lon=as.numeric(colnames(lstclim)))
	ngrd.sp <- ngrd[,c('lon', 'lat')]
	coordinates(ngrd.sp) <- ~lon + lat
	proj4string(ngrd.sp) <- '+proj=longlat +datum=WGS84'
		
	# interpolate: raster query approach with points
	rast <- raster(lstwarm26ann, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		lstwarm26ann05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

			# par(mfrow=c(1,2))
			# image(lstwarm26ann)
			# image(lstwarm26ann05)

	rast <- raster(lstwarm85ann, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		lstwarm85ann05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

	rast <- raster(sstwarm26ann, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		sstwarm26ann05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

	rast <- raster(sstwarm85ann, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		sstwarm85ann05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))


# calculate future temperatures
	dim(lstclim)
	dim(lstwarm26ann05) # match: good
	
	par(mfrow=c(1,2)) # make sure they are aligned
	image(lstclim)
	image(lstwarm26ann05)

	
	lstfut26ann <- lstclim + lstwarm26ann05
	sstfut26ann <- sstclim + sstwarm26ann05

	lstfut85ann <- lstclim + lstwarm85ann05
	sstfut85ann <- sstclim + sstwarm85ann05


# plot the present temperature maps
	par(mfrow=c(2,1))
	image(t(lstclimn)[,nrow(lstclimn):1])
	image(t(sstclimn)[,nrow(sstclimn):1])


# plot the future temperature maps
	par(mfrow=c(2,1))
	image(t(lstfut85ann)[,nrow(lstfut85ann):1])
	image(t(sstfut85ann)[,nrow(sstfut85ann):1])
	

# write out
save(lstfut26ann, file='temp/lstfut26ann.rdata')
save(sstfut26ann, file='temp/sstfut26ann.rdata')
save(lstfut85ann, file='temp/lstfut85ann.rdata')
save(sstfut85ann, file='temp/sstfut85ann.rdata')


#####################
## For mean summer
#####################

# read in climatologies for summer mean
load('temp/lstclimatology_warm3.rdata') # lstclimwarm3
load('temp/sstclimatology_warm3.rdata') # sstclimwarm3

# calculate warming 
	# lst
	lstwarm26sumN <- apply(lstwarm26[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	lstwarm26sumS <- apply(lstwarm26[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	lstwarm26sum <- t(cbind(lstwarm26sumN[,as.character(seq(88.75,1.25,by=-2.5))], lstwarm26sumS[,as.character(seq(-1.25,-88.75,by=-2.5))]))

	lstwarm85sumN <- apply(lstwarm85[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	lstwarm85sumS <- apply(lstwarm85[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	lstwarm85sum <- t(cbind(lstwarm85sumN[,as.character(seq(88.75,1.25,by=-2.5))], lstwarm85sumS[,as.character(seq(-1.25,-88.75,by=-2.5))]))


	# sst
	sstwarm26sumN <- apply(sstwarm26[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	sstwarm26sumS <- apply(sstwarm26[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	sstwarm26sum <- t(cbind(sstwarm26sumN[,as.character(seq(89.375,0.625,by=-1.25))], sstwarm26sumS[,as.character(seq(-0.625,-89.375,by=-1.25))]))

	sstwarm85sumN <- apply(sstwarm85[,,6:8], MARGIN=c(1,2), FUN=mean) # delta for northern hemisphere summer
	sstwarm85sumS <- apply(sstwarm85[,,c(1,2,12)], MARGIN=c(1,2), FUN=mean) # delta for southern hemisphere summer
	sstwarm85sum <- t(cbind(sstwarm85sumN[,as.character(seq(89.375,0.625,by=-1.25))], sstwarm85sumS[,as.character(seq(-0.625,-89.375,by=-1.25))]))

	# clean up
	rm(sstwarm26sumN, sstwarm26sumS, lstwarm26sumN, lstwarm26sumS, sstwarm85sumN, sstwarm85sumS, lstwarm85sumN, lstwarm85sumS)

# interpolate warming to 0.5x0.5
	# create new grid
	ngrd <- expand.grid(lat=as.numeric(rownames(lstclimwarm3)), lon=as.numeric(colnames(lstclimwarm3)))
	ngrd.sp <- ngrd[,c('lon', 'lat')]
	coordinates(ngrd.sp) <- ~lon + lat
	proj4string(ngrd.sp) <- '+proj=longlat +datum=WGS84'
		
	# interpolate: raster query approach with points
	rast <- raster(lstwarm26sum, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		lstwarm26sum05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

			# par(mfrow=c(1,2))
			# image(lstwarm26ann)
			# image(lstwarm26ann05)

	rast <- raster(lstwarm85sum, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		lstwarm85sum05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

	rast <- raster(sstwarm26sum, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		sstwarm26sum05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

	rast <- raster(sstwarm85sum, xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		sstwarm85sum05 <- as.matrix(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))


# calculate future temperatures
	dim(lstclimwarm3)
	dim(lstwarm26sum05)
	
	par(mfrow=c(1,2)) # make sure they are aligned
	image(lstclimwarm3)
	image(lstwarm26sum05)

	lstfut26sum <- lstclimwarm3 + lstwarm26sum05 # for summer mean
	sstfut26sum <- sstclimwarm3 + sstwarm26sum05

	lstfut85sum <- lstclimwarm3 + lstwarm85sum05 # for summer mean
	sstfut85sum <- sstclimwarm3 + sstwarm85sum05


# plot the present temperature maps
	par(mfrow=c(2,1))
	image(t(lstclimwarm3)[,nrow(lstclimwarm3):1])
	image(t(sstclimwarm3)[,nrow(sstclimwarm3):1])


# plot the future temperature maps
	par(mfrow=c(2,1))
	image(t(lstfut85sum)[,nrow(lstfut85sum):1], main='rcp85')
	image(t(sstfut85sum)[,nrow(sstfut85sum):1], main='rcp85')


# write out
save(lstfut26sum, file='temp/lstfut26sum.rdata')
save(sstfut26sum, file='temp/sstfut26sum.rdata')
save(lstfut85sum, file='temp/lstfut85sum.rdata')
save(sstfut85sum, file='temp/sstfut85sum.rdata')


#####################
## For warmest month
#####################

# read in climatologies for all months
load('temp/sstclimatology_bymonth.rdata') # sstclimbymo
load('temp/lstclimatology_bymonth.rdata') # lstclimbymo


# interpolate warming for each month to 0.5x0.5
	# create new grid
	ngrd <- expand.grid(lat=as.numeric(rownames(lstclim)), lon=as.numeric(colnames(lstclim)))
	ngrd.sp <- ngrd[,c('lon', 'lat')]
	coordinates(ngrd.sp) <- ~lon + lat
	proj4string(ngrd.sp) <- '+proj=longlat +datum=WGS84'
		
	# interpolate: raster query approach with points
	# have to exchange axes for lstwarm26
	rast <- brick(aperm(lstwarm26, c(2,1,3)), xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		rast2 <- rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss))
		lstwarm26.05 <- as.array(rast2)
		
			# par(mfrow=c(1,2))
			# image(lstwarm26[,,1])
			# image(lstwarm26.05[,,1]) # should be rotated

	rast <- brick(aperm(lstwarm85, c(2,1,3)), xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		lstwarm85.05 <- as.array(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

	rast <- brick(aperm(sstwarm26, c(2,1,3)), xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		sstwarm26.05 <- as.array(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))

	rast <- brick(aperm(sstwarm85, c(2,1,3)), xmn=1.25, xmx=358.75, ymn=-88.75, ymx=88.75, crs='+proj=longlat +datum=WGS84') # make a raster object
		newvalss <- extract(rast, ngrd.sp, method='bilinear')
		sstwarm85.05 <- as.array(rasterFromXYZ(cbind(ngrd[,c('lon', 'lat')], newvalss)))


# check alignment
	dim(lstclimbymo)
	dim(sstclimbymo)
	dim(lstwarm26.05)
	dim(lstwarm85.05)
	dim(sstwarm26.05)
	dim(sstwarm85.05)
	
	par(mfrow=c(1,2)) # check for alignment
	image(lstclimbymo[,,2])
	image(lstwarm26.05[,,2])

	par(mfrow=c(1,2)) # check for alignment
	image(lstclimbymo[,,2])
	image(lstwarm85.05[,,2])

	par(mfrow=c(1,2)) # check for alignment
	image(sstclimbymo[,,2])
	image(sstwarm26.05[,,2])

	par(mfrow=c(1,2)) # check for alignment
	image(sstclimbymo[,,2])
	image(sstwarm85.05[,,2])


# calculate future temperatures for each month
	lstfut26bymo <- lstclimbymo + lstwarm26.05
	sstfut26bymo <- sstclimbymo + sstwarm26.05

	lstfut85bymo <- lstclimbymo + lstwarm85.05
	sstfut85bymo <- sstclimbymo + sstwarm85.05

# plot the present and future temperature maps
	# January
	zliml = range(c(lstclimbymon[,,1], lstfut85bymo[,,1]), na.rm=TRUE)
	zlimo = range(c(sstclimbymon[,,1], sstfut85bymo[,,1]), na.rm=TRUE)
	par(mfrow=c(3,2))
	image(lstclimbymo[,,1], zlim=zliml)
	image(sstclimbymo[,,1], zlim=zlimo)
	image(lstwarm85.05[,,1])
	image(sstwarm85.05[,,1])
	image(lstfut85bymo[,,1], main='rcp85 jan', zlim=zliml)
	image(sstfut85bymo[,,1], main='rcp85 jan', zlim=zlimo)

	# July
	zliml = range(c(lstclimbymo[,,7], lstfut85bymo[,,7]), na.rm=TRUE)
	zlimo = range(c(sstclimbymo[,,7], sstfut85bymo[,,7]), na.rm=TRUE)
	par(mfrow=c(3,2))
	image(lstclimbymo[,,7], zlim=zliml)
	image(sstclimbymo[,,7], zlim=zlimo)
	image(lstwarm85.05[,,7])
	image(sstwarm85.05[,,7])
	image(lstfut85bymo[,,7], main='rcp85 jul', zlim=zliml)
	image(sstfut85bymo[,,7], main='rcp85 jul', zlim=zlimo)

# calculate the warmest month
lstfut26warmestmo <- apply(lstfut26bymo, MARGIN=c(1,2), FUN=max)
sstfut26warmestmo <- apply(sstfut26bymo, MARGIN=c(1,2), FUN=max)
lstfut85warmestmo <- apply(lstfut85bymo, MARGIN=c(1,2), FUN=max)
sstfut85warmestmo <- apply(sstfut85bymo, MARGIN=c(1,2), FUN=max)


# plot the future temperature maps
	par(mfrow=c(2,1))
	image(t(lstfut85warmestmo)[,nrow(lstfut85warmestmo):1], main='rcp85')
	image(t(sstfut85warmestmo)[,nrow(sstfut85warmestmo):1], main='rcp85')


# write out
save(lstfut26warmestmo, file='temp/lstfut26warmestmo.rdata')
save(sstfut26warmestmo, file='temp/sstfut26warmestmo.rdata')
save(lstfut85warmestmo, file='temp/lstfut85warmestmo.rdata')
save(sstfut85warmestmo, file='temp/sstfut85warmestmo.rdata')
