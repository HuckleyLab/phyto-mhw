# To analyze and process the output tosmaxday.rdata
# Run after climatology_OISST_maxday.r

################
# Reshape
# To run on laptop
###############
load('temp/tosmaxday.rdata')

# reorder dimensions
tosmaxday <- aperm(tosmaxday, c(2,1))

# rotate sst monthly temps 1986-2005 so columns 0 -> 360 lon (centered at Greenwich), rows 90 -> -90 lat (like cmip5)
nms <- dimnames(tosmaxday)
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

tosmaxday <- tosmaxday[newlatord, newlonord] # lat x lon (0-360)
dimnames(tosmaxday)[[2]] <- newcolnms

	image(tosmaxday)

save(tosmaxday, file='temp/tosmaxday.rdata')


#############
# Plot
#############
load('temp/tosmaxday.rdata')

image(tosmaxday)


###########################
# Compare warmest day to warmest month
###########################
require(raster)

load('temp/tosmax.rdata')
load('temp/tosmaxday.rdata')

range(tosmaxday - tosmax, na.rm=TRUE)
summary(as.numeric(tosmaxday - tosmax))
sum(as.numeric(tosmaxday - tosmax) < 0, na.rm=TRUE)/sum(!is.na(as.numeric(tosmaxday - tosmax)))

rast <- raster(tosmaxday - tosmax, xmn=0, xmx=360, ymn=-90, ymx=90)
plot(rast)

# show values < 0 
rast2 <- raster((tosmaxday - tosmax)<0, xmn=0, xmx=360, ymn=-90, ymx=90)
plot(rast2)