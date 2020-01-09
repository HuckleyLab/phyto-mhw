# Function to get GHCN Daily temperature data near a given lat/lon point
# Global Historical Climatology Network
# lat
# lon
# ghcnd_stations: output from ghcnd_stations()
# radius: maximum search radius in km
# mincov: minimum coverage proportion
# mindate: YYYY-MM-DD format string, earliest date
# maxdate:
#
# Needs this option set to get GHCN data:
# options(noaakey = "KEY_EMAILED_TO_YOU")
#
# Need to load rgdal BEFORE running this function. Not clear why it can't load dynamically, but it won't for me.
#
# easy setup of default values when debugging: mindate='1986-01-01'; maxdate='2005-12-31'; radius=200; mincov=0.8

ghcn_get<-function(lat=40.48, lon=-74.44, ghcnd_stations, mindate='1986-01-01', maxdate='2005-12-31', radius=1000, mincov=0.8){
	if (!suppressMessages(library("rgdal", logical.return=TRUE))) {
		stop("rgdal needed for working with GHCN data. Please install it.", 
		  call. = FALSE)
	}
	if (!suppressMessages(library("rnoaa", logical.return=TRUE))) {
		stop("rnoaa needed for working with GHCN data. Please install it.", 
		  call. = FALSE)
	}
	if (!suppressMessages(library("rgeos", logical.return=TRUE))) {
		stop("rgeos needed for working with GHCN data. Please install it.", 
		  call. = FALSE)
	}
	if (!suppressMessages(library("fields", logical.return=TRUE))) {
		stop("fields needed for working with GHCN data. Please install it.", 
		  call. = FALSE)
	} # for rdist.earth

	minrows <- as.numeric(mincov*(as.Date(maxdate)-as.Date(mindate))) # minimum # of rows of data that we want
	maxrows <- as.numeric(as.Date(maxdate) - as.Date(mindate)+1) # total number of rows expected
	
	stations <- meteo_nearby_stations(data.frame(id=1, latitude=lat, longitude=lon), station_data=ghcnd_stations, year_min=substr(mindate,1,4), year_max=substr(maxdate,1,4), var=c('TMIN', 'TMAX'), radius=radius, limit=1000) # only the 1000 closest (limit=1000)
		
	if(nrow(stations$`1`)>0){
		if(!is.na(stations$`1`$id[1])){
			message(paste('Found', nrow(stations$`1`), 'GHCND stations\n'))
			FOUNDSTATION=FALSE
			error=FALSE
			r <- 1
		
			# check if any stations span the year range we need and have reasonable coverage
			while(FOUNDSTATION==FALSE & error==FALSE){
				message(paste('trying station', r, '\n'))

				dat <- NULL
				dat <- tryCatch( # in rare cases, this throws an error
					meteo_tidy_ghcnd(stationid=stations$`1`$id[r], keep_flags=TRUE, var=c('TMAX', 'TMIN'), date_min=mindate, date_max=maxdate), # returns tmax and tmin in degC*10	
					error = function(e){
						message(paste('ERROR in meteo_tidy_ghcnd():', e))
						return(NULL)
					}
					)
		
				# simple checks if data returned
				if(!is.null(dat) & 'tmin' %in% names(dat) & 'tmax' %in% names(dat)){
					# trim out data that failed quality checks
					dat$tmin[dat$qflag_tmin != ' '] <- NA
					dat$tmax[dat$qflag_tmax != ' '] <- NA
		
					if(sum(!is.na(dat$tmax))>=minrows & sum(!is.na(dat$tmin))>=minrows){
						FOUNDSTATION=TRUE
					}
				}
				
				r <- r+1

				# failed if no station met the search criteria
				if(r > nrow(stations$`1`)){
					message(paste('Could not find appropriate weather station for lat', lat, 'lon', lon, 'with radius', radius))
					error <- TRUE
				}
			}
		} else {
			message(paste('Could not find appropriate weather station for lat', lat, 'lon', lon, 'with radius', radius))
			error <- TRUE
		}
	} else {
		error <- TRUE
	}
	
	if(!error){
		message(paste('found station', stations$`1`$id[r-1], 'distance', round(stations$`1`$distance[r-1],2), 'km. Coverage tmin:', round(sum(!is.na(dat$tmin))/maxrows, 5), 'tmax:', round(sum(!is.na(dat$tmax))/maxrows, 5)))

		# check that every day is represented
		if(nrow(dat) > maxrows) stop(paste('Too many rows returned for', stations$`1`$id[r-1]))
		if(nrow(dat) < maxrows){
			message(paste('Too few rows returned for', stations$`1`$id[r-1], '. Padding to full length'))
			alldates <- data.frame(date=seq(from=as.Date(mindate), to=as.Date(maxdate), by=1))
			dat <- merge(alldates, dat, all.x=TRUE)
		}
	
		# convert from degC*10 to degC
		dat$tmin <- dat$tmin/10
		dat$tmax <- dat$tmax/10
			
		return(dat)
	}
}