# Thermal safety calculations using species-specific ARRs
# from thermal_safety_vs_lat_bylatlon_calculations.r

require(mgcv)
require(data.table)
require(lme4)

##################
# Functions
##################

# round a single value (x) to nearest value in a vector (y)
roundto <- function(x, y){
	a <- abs(x - y)
	i <- which(a==min(a)) # can return 1 or 2 matches
	return(y[i])
}


##################
# Parameters
##################
# for adjusting max air temperature to account for elevation
lapse <- 0.0055 # degC/m elevation. See Sunday et al. 2014 PNAS supplement

#average Acclimation Response Ratio for terrestrial and marine organisms
# from Gunderson and Stillman model-averaged results (Gunderson pers. comm.)
ARRland <- 0.11
ARRoce <- 0.24

#################
# Read in data
#################

# Measurements of upper thermal tolerance and NicheMapR (terrestrial microclimates)
dat <- fread(file="output/dataset_1_hotwater_Nichemapped_GHCND.csv")
dat95 <- fread(file="output/dataset_1_hotwater_Nichemapped_95quant.csv") # 95% quantiles for terrestrial microclimates

	# merge dat and dat95
	setnames(dat95, c("NM_2m_airshade", "NM_exposed_bodytemp", "NM_exposed_body_wet", "NM_shade_body_wet"), c("NM_2m_airshade95", "NM_exposed_Te95", "NM_exposed_Te_wet95", "NM_shade_Te_wet95"))

	dat <- merge(dat, dat95[,.(Genus, Species, citation, NM_2m_airshade95, NM_exposed_Te95, NM_exposed_Te_wet95, NM_shade_Te_wet95)], all.x=TRUE)
		dim(dat)
		dim(dat95)

	# fix column names
	setnames(dat, c('tmax', 'tmax_acc', 'altitude', 'citation', 'NM_exposed_bodytemp', 'NM_exposed_body_wet', 'NM_shade_body_wet'), c('Tmax', 'max_pretreatment', 'elevation_max', 'REF_max', 'NM_exposed_Te', 'NM_exposed_Te_wet', 'NM_shade_Te_wet'))


# read in traits
	traits <- fread('data/tmax_data/dataset_1_traits.csv')

	# fix benthopelagic and pelagic
	traits[demers_pelag_fb == 'benthopelagic', demers_pelag := 'demersal']
	traits[demers_pelag_fb == 'pelagic-neritic', demers_pelag := 'pelagic-neritic']

# read in species-specific ARR
	arr <- fread('data/tmax_data/dataset_1_ARRs.tsv')

	# merge traits and arr
	traits2 <- merge(traits, arr[, .(Genus, Species, ctmax_ARR_GS)], by=c('Genus', 'Species'), all.x=TRUE)

	# merge	traits
	dat <- merge(dat[,.(Genus, Species, Tmax, tmax_metric, max_pretreatment, lat_max, long_max, elevation_max, REF_max, Phylum, Class, Order, Family, NMGHCND_2m_airshade95, NMGHCND_exposed_Te95, NMGHCND_exposed_Te_wet95, NMGHCND_shade_Te_wet95)], traits2[, .(Genus, Species, thermy, Realm, Realm_detail, demers_pelag, mobility, weight, length, ctmax_ARR_GS)], all.x=TRUE, by=c('Genus', 'Species'))
		dim(dat)
		dim(dat95)

# trim out freshwater and intertidal
	dat <- dat[Realm %in% c('Marine', 'Terrestrial') & Genus != 'Gillichthys' & !(Realm_detail %in% c('catadromous', 'anadromous', 'amphidromous', 'freshwater')),]
		dim(dat)
		
# trim to species with species-specific ARRs
	dat <- dat[!is.na(ctmax_ARR_GS),]

# re-order dat alphabetically
	dat <- dat[order(dat$Genus, dat$Species, dat$REF_max),]

# how much trait data?
	dat[,sort(unique(Realm))]
	dat[Realm=='Marine', summary(length)]
	dat[Realm=='Marine', sort(unique(demers_pelag))]
	dat[Realm=='Marine', sort(unique(mobility))]



# Elevation
	load('temp/elev.rdata') # elev data.frame 0.5x0.5
		
# Current temperatures
	# read in climatologies for annual mean
	load('temp/lstclimatology.rdata') # lstclim (0.25x0.25)
	load('temp/sstclimatology.rdata') # sstclim
#		lstclimn <- lstclim
#		sstclimn <- sstclim
#
#	# read in climatologies for summer mean
	load('temp/lstclimatology_warm3.rdata') # lstclimwarm3 (0.25x0.25 grid)
	load('temp/sstclimatology_warm3.rdata') # sstclimwarm3
		lstclimwarm3n <- lstclimwarm3
		sstclimwarm3n <- sstclimwarm3
#
#	# read in climatology for the warmest month
	load('temp/sstclimatology_warmestmonth.rdata') # sstclimwarmestmo (0.25x0.25 grid)
	load('temp/lstclimatology_warmestmonth.rdata')
		lstclimwarmestmon <- lstclimwarmestmo
		sstclimwarmestmon <- sstclimwarmestmo
		
	# read in data for the 95% warmest daily maximum
	load('temp/tos95max.rdata') # tos95max (0.25x0.25 grid): ocean (95% warmest day + 50% DTR)

	# read in data for the warmest daily maximum
	load('temp/tosmax.rdata') # tosmax (0.25x0.25 grid): ocean (warmest day + 50% DTR)
#	load('temp/tasmax.rdata') # tasmax: air 0.25x0.25 (warmest daily max value averaged across any month). Use NicheMapR instead




############################################################################################################
# match habitat temperature to lat/lon of thermal tolerance (mostly for ocean since NicheMapR used on land
############################################################################################################

# Initialize columns for current habitat temperatures (those not from NicheMapR) and elevation
dat$elev.grid <- dat$thab95hr <- dat$thabmaxhr <- dat$thabmaxmo <- dat$thabsum <- dat$thabann <-  NA

# Get lists of latitude and longitude values in the temperature data
llons <- as.numeric(colnames(lstclimwarmestmon))
llats <- as.numeric(rownames(lstclimwarmestmon))
slons <- as.numeric(colnames(tosmax))
slats <- as.numeric(rownames(tosmax))

llonstep <- abs(diff(llons)[1])
slonstep <- abs(diff(slons)[1])
llatstep <- abs(diff(llats)[1])
slatstep <- abs(diff(slats)[1])

# Match each data point to temperature and elevation data based on lat/lon and habitat
for(i in 1:nrow(dat)){
	if(!is.na(dat$lat_max[i]) & !is.na(dat$long_max[i])){

		# land
		if(dat$Realm[i]=='Terrestrial'){
			if(dat$long_max[i] < 0){ # may need to correct lon to 0-360
				lons <- roundto(dat$long_max[i] + 360, llons) # find the closest longitude (or lons)
			} else {
				lons <- roundto(dat$long_max[i], llons) 
			}
			lats <- roundto(dat$lat_max[i], llats) # find the closest latitudes (or latitudes)
			
			# look for grid cell with data
			ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # test

			if(is.na(ltemp)){ # if didn't get a value, try searching in a slightly wider area (may have moved off land in rounding)
				cat(paste('Going to wider search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
				origlats <- lats
				origlons <- lons
				
				if(all(lats > dat$lat_max[i])){ # move down one step if all rounded lats are greater
					lats <- c(lats, lats - llatstep)
				}
				if(all(lats < dat$lat_max[i])){
					lats <- c(lats, lats + llatstep)
				}
				if(all(lons > dat$long_max[i])){
					lons <- c(lons, lons - llonstep)
				}
				if(all(lons < dat$long_max[i])){
					lons <- c(lons, lons + llonstep)
				}

				ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				
				# take only the first element so that  addition and subtraction work in following steps
				origlats <- lats[1]
				origlons <- lons[1]

				if(is.na(ltemp)){ # if still don't get a value, try searching in 9-grid area
					cat(paste('\tGoing to 9-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep, origlats, origlats+llatstep)
					lons <- c(origlons-llonstep, origlons, origlons+llonstep)
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}

				if(is.na(ltemp)){ # if still don't get a value, try searching in 25-grid area
					cat(paste('\tGoing to 25-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep*(2:1), origlats, origlats+llatstep*(1:2))
					lons <- c(origlons-llonstep*(2:1), origlons, origlons+llonstep*(1:2))
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}

				if(is.na(ltemp)){ # if still don't get a value, try searching in 49-grid area
					cat(paste('\tGoing to 49-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep*(3:1), origlats, origlats+llatstep*(1:3))
					lons <- c(origlons-llonstep*(3:1), origlons, origlons+llonstep*(1:3))
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}

				if(is.na(ltemp)){ # if still don't get a value, try searching in 81-grid area
					cat(paste('\tGoing to 81-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep*(4:1), origlats, origlats+llatstep*(1:4))
					lons <- c(origlons-llonstep*(4:1), origlons, origlons+llonstep*(1:4))
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}

				if(is.na(ltemp)){
					cat(paste('\tGoing to 121-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep*(5:1), origlats, origlats+llatstep*(1:5))
					lons <- c(origlons-llonstep*(5:1), origlons, origlons+llonstep*(1:5))
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}

				if(is.na(ltemp)){
					cat(paste('\tGoing to 169-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep*(6:1), origlats, origlats+llatstep*(1:6))
					lons <- c(origlons-llonstep*(6:1), origlons, origlons+llonstep*(1:6))
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}

				if(is.na(ltemp)){
					cat(paste('\tGoing to 225-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep*(7:1), origlats, origlats+llatstep*(1:7))
					lons <- c(origlons-llonstep*(7:1), origlons, origlons+llonstep*(1:7))
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}

				if(is.na(ltemp)){
					cat(paste('\tGoing to 289-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
					lats <- c(origlats-llatstep*(9:1), origlats, origlats+llatstep*(1:9))
					lons <- c(origlons-llonstep*(9:1), origlons, origlons+llonstep*(1:9))
					ltemp <- mean(lstclimwarmestmon[as.character(lats), as.character(lons)], na.rm=TRUE) # land temp
				}
				
				cat(paste('\tltemp now', ltemp, '\n'))

			}

			# for annual mean
			dat$thabann[i] <- mean(lstclim[as.character(lats), as.character(lons)], na.rm=TRUE)

			# for summer mean
			dat$thabsum[i] <- mean(lstclimwarm3n[as.character(lats), as.character(lons)], na.rm=TRUE)

			# for max month
			dat$thabmaxmo[i] <- mean(lstclimwarmestmo[as.character(lats), as.character(lons)], na.rm=TRUE)

			# for max hour
			# use NicheMapR instead
#			dat$thabmaxhr[i] <- mean(tasmax[as.character(lats), as.character(lons)], na.rm=TRUE)
			
			# for elevation
			dat$elev.grid[i] <- mean(elev[as.character(lats), as.character(lons)], na.rm=TRUE)

		}

		# ocean
		if(dat$Realm[i]=='Marine'){
			if(dat$long_max[i] < 0){
				lons <- roundto(dat$long_max[i] + 360, slons)
			} else {
				lons <- roundto(dat$long_max[i], slons)
			}
			lats <- roundto(dat$lat_max[i], slats)


			# look for grid cell with data
			stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE) # ocean temp

			if(is.na(stemp)){ # if didn't get a value, try searching in a slightly wider area (may have moved off ocean in rounding)
				cat(paste('Going to wider search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
				origlats <- lats
				origlons <- lons

				if(all(lats > dat$lat_max[i])){ # move down one step if all rounded lats are greater
					lats <- c(lats, lats - slatstep)
				}
				if(all(lats < dat$lat_max[i])){
					lats <- c(lats, lats + slatstep)
				}
				if(all(lons > dat$long_max[i])){
					lons <- c(lons, lons - slonstep)
				}
				if(all(lons < dat$long_max[i])){
					lons <- c(lons, lons + slonstep)
				}

				stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE) # ocean temp

				# take only the first element so that  addition and subtraction work in following steps
				origlats <- lats[1]
				origlons <- lons[1]

				if(is.na(stemp)){ # if still don't get a value, try searching in 9-grid area
					cat(paste('\tGoing to 9-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
					lats <- c(origlats-slatstep, origlats, origlats+slatstep)
					lons <- c(origlons-slonstep, origlons, origlons+slonstep)
					stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE) # ocean temp
				}

				if(is.na(stemp)){ # if still don't get a value, try searching in 25-grid area
					cat(paste('\tGoing to 25-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
					lats <- c(origlats-slatstep*(2:1), origlats, origlats+slatstep*(1:2))
					lons <- c(origlons-slonstep*(2:1), origlons, origlons+slonstep*(1:2))
					stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE)
				}

				if(is.na(stemp)){ # if still don't get a value, try searching in 49-grid area
					cat(paste('\tGoing to 49-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
					lats <- c(origlats-slatstep*(3:1), origlats, origlats+slatstep*(1:3))
					lons <- c(origlons-slonstep*(3:1), origlons, origlons+slonstep*(1:3))
					stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE)
				}

				if(is.na(stemp)){ # if still don't get a value, try searching in 81-grid area
					cat(paste('\tGoing to 81-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
					lats <- c(origlats-slatstep*(4:1), origlats, origlats+slatstep*(1:4))
					lons <- c(origlons-slonstep*(4:1), origlons, origlons+slonstep*(1:4))
					stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE)
				}

				if(is.na(stemp)){ 
					cat(paste('\tGoing to 121-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
					lats <- c(origlats-slatstep*(5:1), origlats, origlats+slatstep*(1:5))
					lons <- c(origlons-slonstep*(5:1), origlons, origlons+slonstep*(1:5))
					stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE)
				}

				if(is.na(stemp)){ 
					cat(paste('\tGoing to 169-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
					lats <- c(origlats-slatstep*(6:1), origlats, origlats+slatstep*(1:6))
					lons <- c(origlons-slonstep*(6:1), origlons, origlons+slonstep*(1:6))
					stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE)
				}

				if(is.na(stemp)){ 
					cat(paste('\tGoing to 225-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
					lats <- c(origlats-slatstep*(7:1), origlats, origlats+slatstep*(1:7))
					lons <- c(origlons-slonstep*(7:1), origlons, origlons+slonstep*(1:7))
					stemp <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE)
				}

				cat(paste('\tstemp now', stemp, '\n'))
			}

			# for annual mean
			dat$thabann[i] <- mean(sstclim[as.character(lats), as.character(lons)], na.rm=TRUE)

			# for summer mean
			dat$thabsum[i] <- mean(sstclimwarm3n[as.character(lats), as.character(lons)], na.rm=TRUE)

			# for max month
			dat$thabmaxmo[i] <- mean(sstclimwarmestmo[as.character(lats), as.character(lons)], na.rm=TRUE)

			# for 95% max hour
			dat$thab95hr[i] <- mean(tos95max[as.character(lats), as.character(lons)], na.rm=TRUE)

			# for max hour
			dat$thabmaxhr[i] <- mean(tosmax[as.character(lats), as.character(lons)], na.rm=TRUE)

		}

	}
}


	# check
	dat[,summary(thabann)]
	dat[,summary(thabsum)]
	dat[,summary(thabmaxmo)]
	dat[Realm=='Marine',summary(thabann)]
	dat[Realm=='Marine',summary(thab95hr)]
	dat[Realm=='Marine',summary(thabmaxhr)]

	dat[Realm=='Marine' & is.na(thabann),]


##############################################################
# adjust terrestrial habitat temperatures based on elevation differences 
##############################################################
dat[,thabann.adj := thabann]
dat[,thabsum.adj := thabsum]
dat[,thabmaxmo.adj := thabmaxmo]
#dat[,thab95hr.adj := thabmaxhr]
#dat[,thabmaxhr.adj := thabmaxhr]
i <- dat[,!is.na(elevation_max) & !is.na(elev.grid) & Realm=='Terrestrial']
dat[i,thabann.adj := thabann + lapse*(elev.grid - elevation_max)] # adjust for altitude offset from grid cell
dat[i,thabsum.adj := thabsum + lapse*(elev.grid - elevation_max)] # adjust for altitude offset from grid cell
dat[i,thabmaxmo.adj := thabmaxmo + lapse*(elev.grid - elevation_max)] # adjust for altitude offset from grid cell
# dat[i,thab95hr.adj := thab95hr + lapse*(elev.grid - elevation_max)] # adjust for altitude offset from grid cell. not needed since only in ocean
# dat[i,thabmaxhr.adj := thabmaxhr + lapse*(elev.grid - elevation_max)] # adjust for altitude offset from grid cell. not needed since only in ocean


##############################################################################
# adjust marine habitat temperatures based on behavioral thermoregulation
##############################################################################
# let marine species access cooler microhabitats through behavioral thermoregulation, down to -2degC (for warmest hours)

# initialize columns
dat[,thabmaxhr.marineBT := as.numeric(NA)]
dat[Realm=='Marine',thabmaxhr.marineBT := thabmaxhr]
dat[,thab95hr.marineBT := as.numeric(NA)]
dat[Realm=='Marine',thab95hr.marineBT := thab95hr]

# marine behavioral thermoregulation table
marBT <- data.table(mobility=c('swim', 'swim', 'swim', 'swim', 'swim', 'swim', 'crawl', 'crawl', 'sessile', 'sessile'), demers_pelag=c('pelagic-neritic', 'pelagic-neritic', 'demersal', 'pelagic', 'demersal', 'pelagic', 'demersal', 'demersal', 'demersal', 'demersal'), big=c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE), MBTrefuge=c(10, 3, 3, 10, 1, 10, 1, 0.5, 0, 0))
	marBT

# merge in the MBT table
dat[,big := length>50] # mark the big species (>50cm)
dat <- merge(dat, marBT, all.x=TRUE, by=c('demers_pelag', 'mobility', 'big'))
	nrow(dat)

# calculate refugium temperatures
dat[, thabmaxhr.marineBT := pmax(-2, thabmaxhr - MBTrefuge)] # for warmest hour
dat[, thab95hr.marineBT := pmax(-2, thab95hr - MBTrefuge)] # for 95% warmest hour


############################################
# adjust Tmax for acclimation temperature
# sensitivity analysis: also do with species-specific ARRs
############################################
dat[,tmax.accsum := as.numeric(NA)] # use summer temp for acclimation
dat[,tmax.accsum.elev := as.numeric(NA)]
dat[,tmax.accsumbyspp := as.numeric(NA)] # with species-specific ARRs
dat[,tmax.accsumbyspp.elev := as.numeric(NA)]

# if value is "F" (field?), set to missing
dat[max_pretreatment=='F', max_pretreatment := as.character(NA)]
if(class(dat$max_pretreatment)!='numeric') dat[,max_pretreatment := as.numeric(max_pretreatment)]

# adjust Tmax based on acclimation (average ARR)
lnd <- dat[,Realm=='Terrestrial' & !is.na(max_pretreatment)]
dat[lnd, tmax.accsum := Tmax + ARRland*(thabsum-max_pretreatment)] # adjust from acclimation temp to the summer temperature
dat[lnd, tmax.accsum.elev := Tmax + ARRland*(thabsum.adj-max_pretreatment)] # adjust from acclimation temp to the summer temperature. with elevation correction

oce <- dat[,Realm=='Marine' & !is.na(max_pretreatment)]
dat[oce, tmax.accsum := Tmax + ARRoce*(thabsum-max_pretreatment)]
dat[oce, tmax.accsum.elev := Tmax + ARRoce*(thabsum.adj-max_pretreatment)]

# adjust Tmax based on species-specific ARRs
lnd <- dat[,Realm=='Terrestrial' & !is.na(max_pretreatment) & !is.na(ctmax_ARR_GS)]
dat[lnd, tmax.accsumbyspp := Tmax + ctmax_ARR_GS*(thabsum-max_pretreatment)]
dat[lnd, tmax.accsumbyspp.elev := Tmax + ctmax_ARR_GS*(thabsum.adj-max_pretreatment)]

oce <- dat[,Realm=='Marine' & !is.na(max_pretreatment) & !is.na(ctmax_ARR_GS)]
dat[oce, tmax.accsumbyspp := Tmax + ctmax_ARR_GS*(thabsum-max_pretreatment)]
dat[oce, tmax.accsumbyspp.elev := Tmax + ctmax_ARR_GS*(thabsum.adj-max_pretreatment)]


# no adjustment to Tmax where acclimation temp unknown
dat[Realm=='Terrestrial' & is.na(tmax.accsum), tmax.accsum := Tmax]
dat[Realm=='Terrestrial' & is.na(tmax.accsum.elev), tmax.accsum.elev := Tmax]

dat[Realm=='Marine' & is.na(tmax.accsum), tmax.accsum := Tmax]
dat[Realm=='Marine' & is.na(tmax.accsum.elev), tmax.accsum.elev := Tmax]

	dat[,summary(Tmax)]
	dat[,summary(tmax.accsum)]
	dat[,summary(tmax.accsum.elev)]
	dat[,summary(tmax.accsumbyspp)]
	dat[,summary(tmax.accsumbyspp.elev)]

	# examine missing values
	dat[is.na(tmax.accsumbyspp),]

	# difference from average ARR
	dat[!is.na(tmax.accsumbyspp),.(mean=mean(tmax.accsum - tmax.accsumbyspp), se=sd(tmax.accsum - tmax.accsumbyspp)/sqrt(.N))]
	dat[!is.na(tmax.accsumbyspp.elev),.(mean=mean(tmax.accsum.elev - tmax.accsumbyspp.elev), se=sd(tmax.accsum.elev - tmax.accsumbyspp.elev)/sqrt(.N))]
	dat[!is.na(tmax.accsumbyspp),.(N=.N)]
	dat[!is.na(tmax.accsumbyspp.elev),.(N=.N)]

#	dat[,plot(Tmax, tmax.accsum)]; abline(0,1)
#	dat[,plot(Tmax, tmax.accsum.elev)]; abline(0,1)
#	dat[,plot(tmax.accsum.elev, tmax.accsumbyspp.elev)]; abline(0,1)

#####################################################################################
# calculate thermal safety margin for each measurement of upper thermal tolerance
#####################################################################################

# thermal safety margin 95% max hour (elevation adjusted air temp/not/NM, tmax acclimation corrected to summer/max month/none)
	# body temp with marine behavioral thermoregulation (no sense to include elevation corrections since all ocean)
dat[, tsm_95hr_marineBT_accsumbyspp := tmax.accsumbyspp - thab95hr.marineBT]

# thermal safety margin 95% GHCND max hour (only on land)
	# body temp from NM 2m shade (on land)
dat[ ,tsm_NMGHCND_2m_airshade95_accsumbyspp.elev := tmax.accsumbyspp.elev - NMGHCND_2m_airshade95]

# body temp from 100% wet skin sun in shade (on land)
dat[, tsm_NMGHCND_shade_Te_wet95_accsumbyspp.elev := tmax.accsumbyspp.elev - NMGHCND_shade_Te_wet95]



###########################
# Add species types
###########################

# add species types
dat[, animal.type := as.character(rep(NA, .N))]
dat[Class=='Amphibia' & Realm=='Terrestrial', animal.type := 'amphibian']
dat[Phylum=='Arthropoda' & Class %in% c('Insecta', 'Entognatha') & Realm=='Terrestrial', animal.type := 'insect']
dat[Class %in% c('Malacostraca', 'Collembola', 'Gastropoda') & Realm=='Terrestrial', animal.type := 'other terrestrial invert']
dat[Phylum=='Chordata' & Class %in% c('Reptilia', 'Archelosauria', 'Lepidosauria'), animal.type := 'reptile']
dat[Phylum=='Arthropoda' & Class == 'Arachnida' & Realm=='Terrestrial', animal.type := 'spider']

dat[Phylum=='Arthropoda' & Realm=='Marine', animal.type := 'crustacean']
dat[Phylum=='Chordata' & Realm=='Marine', animal.type := 'fish']
dat[Phylum=='Mollusca' & Realm=='Marine', animal.type := 'mollusc']
dat[Phylum %in% c('Bryozoa', 'Echinodermata', 'Brachiopoda') & Realm=='Marine', animal.type := 'other marine invert']

	# any missing?
	dat[is.na(animal.type), sort(unique(paste(Realm, Phylum, Class)))]
	dat[is.na(animal.type), sort(unique(paste(Realm, Phylum, Class, Order)))]

########################################
# Make aggregate vectors of TSM 
# (no need for Te since Te not affected by ARR)
########################################

## TSM
	# use 95% warmest hour GHCND for TSMs. favorable microclimates (shade or wet skin + behavioral thermoregulation)
	dat[, tsm_favorableGHCND95 := as.numeric(rep(NA, .N))]
	dat[Realm=='Terrestrial' & animal.type!='amphibian', tsm_favorableGHCND95 := tsm_NMGHCND_2m_airshade95_accsumbyspp.elev]
	dat[Realm=='Terrestrial' & animal.type=='amphibian', tsm_favorableGHCND95 := tsm_NMGHCND_shade_Te_wet95_accsumbyspp.elev]
	dat[Realm=='Marine', tsm_favorableGHCND95 := tsm_95hr_marineBT_accsumbyspp]


#######################
# write out TSMS by species
#######################
setnames(dat, 'lat_max', 'lat')
setnames(dat, 'long_max', 'lon')
write.csv(dat, 'temp/warmingtolerance_byspecies_ARRbyspp.csv', row.names=FALSE)



