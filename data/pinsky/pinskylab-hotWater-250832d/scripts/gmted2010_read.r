# read nc file from gmted2010 @ 0.25x0.25°
# has digital elevation model

require(ncdf4)

# read in data
f <- 'data/gmted2010/GMTED2010_15n060_0250deg.nc'
n <- nc_open(f)
	print(n)
elev <- ncvar_get(n, 'elevation')
dimnames(elev) <- list(lon=ncvar_get(n, 'longitude'), lat=ncvar_get(n, 'latitude'))
	dim(elev)
	
# put latitude as first dimension
elev <- aperm(elev, c(2,1))

# rotate to 0 to 360 lon and 90 to -90 lat (top to bottom)
lons <- as.numeric(dimnames(elev)[[2]])
newlonord <- c(lons[lons>0], lons[lons<0])
newcolnms <- newlonord
newcolnms[newcolnms<0] <- newcolnms[newcolnms<0] + 360
newlonord <- as.character(newlonord)
newcolnms <- as.character(newcolnms)

lats <- as.numeric(dimnames(elev)[[1]])
newlatord <- sort(lats, decreasing=TRUE)
newrownms <- as.character(newlatord)
newlatord <- as.character(newlatord)

elev <- elev[newlatord, newlonord] # lat x lon (0-360) x month x year
colnames(elev) <- newcolnms
rownames(elev) <- newrownms

	image(elev)
	summary(as.numeric(elev)) # in m
	
# save out
save(elev, file='temp/elev.rdata')