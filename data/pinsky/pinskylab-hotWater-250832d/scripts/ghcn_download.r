# Download GHCND data for each location

library(data.table)
library('plyr') # for mapvalues()
source('scripts/ghcn_get.r')

data<-read.csv(file='data/tmax_data/dataset_1_hotwater.csv', header=T)
load('temp/ghcnd_stations.rdata') # the ghcnd station data

# fix column names and factor levels
setnames(data, c('lat', 'lon', 'habitat', 'citation'), c('lat_max', 'long_max', 'Realm', 'REF_max'))
data$Realm <- mapvalues(data$Realm, from=c('marine', 'terrestrial'), to=c('Marine', 'Terrestrial'))

# trim out rows without Tmax	
sum(is.na(data$tmax)) # none

# tweak lat/lon so that NicheMapR has data (e.g., sites too close to the ocean or data entry error)
i <- data$Genus == 'Hyla' & data$Species == 'peroni' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -33.4; data$long_max[i] <- 151.3 # Gosford, NSW, Australia. Move on land.
i <- data$Genus == 'Suta' & data$Species == 'flagellum' & data$REF_max=='Spellerberg_1972'; data$long_max[i] <- 149.95 # Move on land.

# find rows that can't be run
nodatarows<-NA
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -45 & data$long_max > 35 & data$long_max < 40)) # remove Prince Edward Islands (sub-Antarctic)
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -70)) # remove Antarctic
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -52 & data$long_max > 70 & data$long_max < 75)) # remove Heard Island
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 18.3 & data$lat_max < 18.9 & data$long_max > -115 & data$long_max < -110)) # Revillagigedo Islands, Mexico
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 39 & data$lat_max < 40 & data$long_max > 0 & data$long_max < 1)) # Columbretes archipellago

# find rows to run
landrows <- which(data$Realm == "Terrestrial")
NicheMapablerows <- setdiff(landrows, nodatarows)
dat2 <- data[NicheMapablerows,]
	nrow(dat2)

# find duplicated lat/lon
dat3 <- dat2[!duplicated(dat2[,c('long_max', 'lat_max')]),]
	nrow(dat3)

# remove rows already run (if any)
files <- list.files('data_dl/ghcnd/')
files <- gsub('ghcnd_lat|.rds', '', files)
spl <- strsplit(files, '_lon')
latlons <- sapply(spl, paste, collapse=' ')
alreadyrun <- which(paste(dat3$lat_max, dat3$long_max) %in% latlons)
dat4 <- dat3[-alreadyrun,] # to remove rows already run
	nrow(dat4)

# how many to run?
nrow(dat4)
	sum(data$Realm=='Terrestrial')
	sum(data$Realm=='Terrestrial' & !duplicated(data[,c('long_max', 'lat_max')]))
	sum(data$Realm=='Terrestrial' & !duplicated(data[,c('long_max', 'lat_max')])) - length(nodatarows)

# Find ghcnd data for each lat/lon and write out
for(j in 1:nrow(dat4)){
	print(paste('row ', j, ' of ', nrow(dat4), sep=''))

	# set mincov
	mincov <- 0.8
	if(abs(dat4$lat_max[j]) < 3) mincov <- 0.7 # be a little less strict near the equator

	# get GHCND data
	ghcn <- ghcn_get(lat=dat4$lat_max[j], lon=dat4$long_max[j], ghcnd_stations=ghcnd_stations, mindate='1986-01-01', maxdate='2005-12-31', radius=1000, mincov=mincov)

	# save if a station found
	if(!is.null(ghcn)) saveRDS(ghcn, file=paste('data_dl/ghcnd/ghcnd_lat', dat4$lat_max[j], '_lon', dat4$long_max[j], '.rds', sep=''))

}

# NOW QUIT AND RESTART R