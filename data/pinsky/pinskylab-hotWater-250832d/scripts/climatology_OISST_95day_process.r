# To analyze and process the output tos95day.rdata
# Run after climatology_OISST_95day_doall.r

################
# Reshape
# To run on laptop
###############
load('temp/tos95day.rdata')

# reorder dimensions
tos95day <- aperm(tos95day, c(2,1))

# rotate sst monthly temps 1986-2005 so columns 0 -> 360 lon (centered at Greenwich), rows 90 -> -90 lat (like cmip5)
nms <- dimnames(tos95day)
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

tos95day <- tos95day[newlatord, newlonord] # lat x lon (0-360)
dimnames(tos95day)[[2]] <- newcolnms

	image(tos95day)

save(tos95day, file='temp/tos95day.rdata')


#############
# Plot
#############
load('temp/tos95day.rdata')

image(tos95day)


###########################
# Compare 95% warmest day to warmest month and warmest day
###########################
require(raster)
load('temp/tos95day.rdata')

# to warmest month
	load('temp/sstclimatology_warmestmonth.rdata')

	range(tos95day - sstclimwarmestmo, na.rm=TRUE)
	summary(as.numeric(tos95day - sstclimwarmestmo))
	sum(as.numeric(tos95day - sstclimwarmestmo) < 0, na.rm=TRUE)/sum(!is.na(as.numeric(tos95day - tosmax)))

	rast <- raster(tos95day - sstclimwarmestmo, xmn=0, xmx=360, ymn=-90, ymx=90)
	plot(rast)

	# show values < 0 
	rast2 <- raster((tos95day - sstclimwarmestmo)<0, xmn=0, xmx=360, ymn=-90, ymx=90)
	plot(rast2)

# to warmest day
	load('temp/tosmaxday.rdata')

	range(tos95day - tosmaxday, na.rm=TRUE)
	summary(as.numeric(tos95day - tosmaxday))
	sum(as.numeric(tos95day - tosmaxday) > 0, na.rm=TRUE)/sum(!is.na(as.numeric(tos95day - tosmax)))

	rast <- raster(tos95day - tosmaxday, xmn=0, xmx=360, ymn=-90, ymx=90)
	plot(rast)

