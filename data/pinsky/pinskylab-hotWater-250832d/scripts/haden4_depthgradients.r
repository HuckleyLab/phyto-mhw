# examine temperature profiles for Had EN4 data

##################
# Functions
##################
require(ncdf4)
require(data.table)

# round a single value (x) to nearest value in a vector (y)
roundto <- function(x, y){
	a <- abs(x - y)
	i <- which(a==min(a)) # can return 1 or 2 matches
	return(y[i])
}




###############
# Read in data
###############

dat <- fread('temp/warmingtolerance_byspecies.csv')

# trim to marine species
	nrow(dat)
dat <- dat[Realm=='Marine' & demers_pelag=='demersal', .(Genus, Species, demers_pelag, mobility, big, lat, lon)]
	nrow(dat) # 82
	
# adjust lon to 0-360
dat[,lon360 := lon]
dat[lon<0, lon360 := lon + 360]
	
# read in temperature profile files
fls <- list.files(path='data_dl/haden4', pattern='.nc', recursive=TRUE, full.names=TRUE)
	length(fls)
	
nc <- nc_open(grep('198608', fls, value=TRUE)) # August

prof <- ncvar_get(nc, 'temperature') 
	dim(prof)
dimnames(prof) <- list(lon=ncvar_get(nc, 'lon'), lat=ncvar_get(nc, 'lat'), depth=ncvar_get(nc, 'depth'))

nc_close(nc)



##############
# Analysis
##############

# plot temperature profiles for locations where species were collected

plot(0, 0, xlab='temperature (°C)', ylab='depth (m)', ylim=c(100,0), xlim=c(-2,32), type='n')
for(i in 1:nrow(dat)){
	lines(prof[dat[i,as.character(round(lon360))], dat[i,as.character(round(lat))], ]-273.15, as.numeric(dimnames(prof)$depth))
}
