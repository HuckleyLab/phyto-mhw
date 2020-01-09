# Calculate future TSM (thermal safety margins) from warmest hourly temperature

require(ncdf4)
require(abind)
require(raster)
require(data.table)

##################
# Functions
##################

# round a single value (x) to nearest value in a vector (y)
roundto <- function(x, y){
	a <- abs(x - y)
	i <- which(a==min(a)) # can return 1 or 2 matches
	return(y[i])
}


####################################################
# Subtract anomalies from TSM to get time-series
# Calculate year TSM crosses zero
# Calculate fractional change by 2100
####################################################

## Parameters
#set Acclimation Response Ratio for terrestrial and marine organisms and for time
# from Gunderson and Stillman model-averaged results (Gunderson pers. comm.)
ARRland <- 0.11
ARRoce <- 0.24


## Read in data
# load the deltas for the warmest hour
load('temp/tasmax26dyr.rdata')
load('temp/tasmax85dyr.rdata')
load('temp/tosmax26dyr.rdata')
load('temp/tosmax85dyr.rdata')

# read in TSMs
dat <- fread('temp/warmingtolerance_byspecies.csv')
dat <- dat[,c('Genus', 'Species', 'Phylum', 'Class', 'Order', 'Family', 'animal.type', 'Realm', 'lat', 'lon', 'tmax_metric',  'tsm_favorableGHCND95')] # trim to relevant columns

# read in species-specific ARR
arr <- fread('data/tmax_data/dataset_1_ARRs.tsv')

# merge TSMs and ARRs
nrow(dat)
dat <- merge(dat, arr, all.x=TRUE, by=c('Genus', 'Species'))
nrow(dat) # didn't lose any rows

# Initialize variables
dat$tsm_2081_2100_rcp26 <- NA # TSM in 2081-2100, RCP2.6
dat$tsm_2081_2100_rcp85 <- NA
dat$tsm_2081_2100_acc_rcp26 <- NA # TSM in 2081-2100 with acclimation
dat$tsm_2081_2100_acc_rcp85 <- NA 
dat$tsm_2081_2100_accbyspp_rcp26 <- NA # TSM in 2081-2100 with species-specific acclimation
dat$tsm_2081_2100_accbyspp_rcp85 <- NA 

# Get lists of latitude and longitude values in the temperature data
llons <- as.numeric(colnames(tasmax85dyr))
llats <- as.numeric(rownames(tasmax85dyr))
slons <- as.numeric(colnames(tosmax85dyr))
slats <- as.numeric(rownames(tosmax85dyr))

llonstep <- abs(diff(llons)[1])
slonstep <- abs(diff(slons)[1])
llatstep <- abs(diff(llats)[1])
slatstep <- abs(diff(slats)[1])


# loop through and calculate future TSM metrics
for(i in 1:nrow(dat)){

	if(dat$Realm[i]=='Terrestrial' & !is.na(dat$tsm_favorableGHCND95[i])){
		if(dat$lon[i] < 0){ # may need to correct lon to 0-360
			lons <- roundto(dat$lon[i] + 360, llons) # find the closest longitude (or lons)
		} else {
			lons <- roundto(dat$lon[i], llons) 
		}
		lats <- roundto(dat$lat[i], llats) # find the closest latitudes (or latitudes)
		
		# look for grid cell with data
		temp <- mean(tasmax85dyr[as.character(lats), as.character(lons),1], na.rm=TRUE) # test

		if(is.na(temp)){ # if didn't get a value, try searching in a slightly wider area (may have moved off land in rounding)
			cat(paste('Going to wider search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
			origlats <- lats
			origlons <- lons
			
			if(all(lats > dat$lat[i])){ # move down one step if all rounded lats are greater
				lats <- c(lats, lats - llatstep)
			}
			if(all(lats < dat$lat[i])){
				lats <- c(lats, lats + llatstep)
			}
			if(all(lons > dat$lon[i])){
				lons <- c(lons, lons - llonstep)
			}
			if(all(lons < dat$lon[i])){
				lons <- c(lons, lons + llonstep)
			}

			temp <- mean(tasmax85dyr[as.character(lats), as.character(lons),1], na.rm=TRUE) # land temp
			cat(paste('temp now', temp, '\n'))
			
			if(is.na(temp)){ # if still don't get a value, try searching in 9-grid area
				cat(paste('\tGoing to 9-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
				lats <- c(origlats-llatstep, origlats, origlats+llatstep)
				lons <- c(origlons-llonstep, origlons, origlons+llonstep)
				temp <- mean(tasmax85dyr[as.character(lats), as.character(lons),1], na.rm=TRUE) # land temp
				cat(paste('\ttemp now', temp, '\n'))
			}
		}		

		# calculate warming at this location and subtract from TSM to get future TSMs
		deltas26 <- apply(tasmax26dyr[as.character(lats), as.character(lons),,drop=FALSE], MARGIN=3, FUN=mean, na.rm=TRUE) # drop=FALSE so that array keeps 3 dimensions even if some are 1
		deltas85 <- apply(tasmax85dyr[as.character(lats), as.character(lons),,drop=FALSE], MARGIN=3, FUN=mean, na.rm=TRUE)
		tsms26 <- dat$tsm_favorableGHCND95[i] - deltas26
		tsms85 <- dat$tsm_favorableGHCND95[i] - deltas85
		
			# plot(as.numeric(names(tsms)), tsms)
			
		# account for acclimation
		deltas26_acc <- deltas26*(1-ARRland)
		deltas85_acc <- deltas85*(1-ARRland)
		tsms26_acc <- dat$tsm_favorableGHCND95[i] - deltas26_acc
		tsms85_acc <- dat$tsm_favorableGHCND95[i] - deltas85_acc
		
		# TSM in 2081-2100
		dat$tsm_2081_2100_rcp26[i] <- mean(tsms26[names(tsms26) %in% 2081:2100])
		dat$tsm_2081_2100_rcp85[i] <- mean(tsms85[names(tsms85) %in% 2081:2100])

		# TSM in 2081-2100 with acclimation
		dat$tsm_2081_2100_acc_rcp26[i] <- mean(tsms26_acc[names(tsms26_acc) %in% 2081:2100])
		dat$tsm_2081_2100_acc_rcp85[i] <- mean(tsms85_acc[names(tsms85_acc) %in% 2081:2100])

		# account for species-specific acclimation if available
		if(!is.na(dat$ctmax_ARR_GS[i])){
			deltas26_accbyspp <- deltas26*(1-dat$ctmax_ARR_GS[i])
			deltas85_accbyspp <- deltas85*(1-dat$ctmax_ARR_GS[i])
			tsms26_accbyspp <- dat$tsm_favorableGHCND95[i] - deltas26_accbyspp
			tsms85_accbyspp <- dat$tsm_favorableGHCND95[i] - deltas85_accbyspp
			dat$tsm_2081_2100_accbyspp_rcp26[i] <- mean(tsms26_accbyspp[names(tsms26_accbyspp) %in% 2081:2100])
			dat$tsm_2081_2100_accbyspp_rcp85[i] <- mean(tsms85_accbyspp[names(tsms85_accbyspp) %in% 2081:2100])
		}		
	
	}

	if(dat$Realm[i]=='Marine' & !is.na(dat$tsm_favorableGHCND95[i])){
		if(dat$lon[i] < 0){ # may need to correct lon to 0-360
			lons <- roundto(dat$lon[i] + 360, slons) # find the closest longitude (or lons)
		} else {
			lons <- roundto(dat$lon[i], slons) 
		}
		lats <- roundto(dat$lat[i], slats) # find the closest latitudes (or latitudes)
		
		# look for grid cell with data
		temp <- mean(tosmax85dyr[as.character(lats), as.character(lons),1], na.rm=TRUE) # test

		if(is.na(temp)){ # if didn't get a value, try searching in a slightly wider area (may have moved off land in rounding)
			cat(paste('Going to wider search area for i=', i, dat$Genus[i], dat$Species[i], 'at sea\n'))
			origlats <- lats
			origlons <- lons
			
			if(all(lats > dat$lat[i])){ # move down one step if all rounded lats are greater
				lats <- c(lats, lats - slatstep)
			}
			if(all(lats < dat$lat[i])){
				lats <- c(lats, lats + slatstep)
			}
			if(all(lons > dat$lon[i])){
				lons <- c(lons, lons - slonstep)
			}
			if(all(lons < dat$lon[i])){
				lons <- c(lons, lons + slonstep)
			}

			temp <- mean(tosmax85dyr[as.character(lats), as.character(lons),1], na.rm=TRUE) # ocean temp
			cat(paste('temp now', temp, '\n'))
			
			if(is.na(temp)){ # if still don't get a value, try searching in 9-grid area
				cat(paste('\tGoing to 9-grid search area for i=', i, dat$Genus[i], dat$Species[i], 'on land\n'))
				lats <- c(origlats-llatstep, origlats, origlats+llatstep)
				lons <- c(origlons-llonstep, origlons, origlons+llonstep)
				temp <- mean(tosmax85dyr[as.character(lats), as.character(lons),1], na.rm=TRUE) # land temp
				cat(paste('\ttemp now', temp, '\n'))
			}
		}		

		# calculate warming at this location and subtract from TSM
		deltas26 <- apply(tosmax26dyr[as.character(lats), as.character(lons),,drop=FALSE], MARGIN=3, FUN=mean, na.rm=TRUE) # drop=FALSE so that array keeps 3 dimensions even if some are 1
		deltas85 <- apply(tosmax85dyr[as.character(lats), as.character(lons),,drop=FALSE], MARGIN=3, FUN=mean, na.rm=TRUE)
		tsms26 <- dat$tsm_favorableGHCND95[i] - deltas26
		tsms85 <- dat$tsm_favorableGHCND95[i] - deltas85

			# plot(as.numeric(names(tsms)), tsms)
			
		# account for acclimation
		deltas26_acc <- deltas26*(1-ARRoce)
		deltas85_acc <- deltas85*(1-ARRoce)
		tsms26_acc <- dat$tsm_favorableGHCND95[i] - deltas26_acc
		tsms85_acc <- dat$tsm_favorableGHCND95[i] - deltas85_acc
		
		# TSM in 2081-2100
		dat$tsm_2081_2100_rcp26[i] <- mean(tsms26[names(tsms26) %in% 2081:2100])
		dat$tsm_2081_2100_rcp85[i] <- mean(tsms85[names(tsms85) %in% 2081:2100])
		
		# TSM in 2081-2100 with acclimation
		dat$tsm_2081_2100_acc_rcp26[i] <- mean(tsms26_acc[names(tsms26_acc) %in% 2081:2100])
		dat$tsm_2081_2100_acc_rcp85[i] <- mean(tsms85_acc[names(tsms85_acc) %in% 2081:2100])
		
		# account for species-specific acclimation if available
		if(!is.na(dat$ctmax_ARR_GS[i])){
			deltas26_accbyspp <- deltas26*(1-dat$ctmax_ARR_GS[i])
			deltas85_accbyspp <- deltas85*(1-dat$ctmax_ARR_GS[i])
			tsms26_accbyspp <- dat$tsm_favorableGHCND95[i] - deltas26_accbyspp
			tsms85_accbyspp <- dat$tsm_favorableGHCND95[i] - deltas85_accbyspp
			dat$tsm_2081_2100_accbyspp_rcp26[i] <- mean(tsms26_accbyspp[names(tsms26_accbyspp) %in% 2081:2100])
			dat$tsm_2081_2100_accbyspp_rcp85[i] <- mean(tsms85_accbyspp[names(tsms85_accbyspp) %in% 2081:2100])
		}		
		
	}

}


# write out
write.csv(dat, 'temp/warmingtolerance_byspecies_to2100.csv', row.names=FALSE)


###############
## calculations
###############
dat <- fread('temp/warmingtolerance_byspecies_to2100.csv')

# how much better is current than rcp85?
	# histogram by habitat
	par(mfrow=c(1,2))
	dat[Realm=='Terrestrial', hist(tsm_favorableGHCND95-tsm_2081_2100_rcp85, plot=TRUE)]
	dat[Realm=='Marine', hist(tsm_favorableGHCND95-tsm_2081_2100_rcp85, plot=TRUE)]

	# average
dat[!is.na(tsm_favorableGHCND95) & !is.na(tsm_2081_2100_rcp85), .(mean_diff_now_85=mean(tsm_favorableGHCND95-tsm_2081_2100_rcp85), se_diff=sd(tsm_favorableGHCND95-tsm_2081_2100_rcp85)/sqrt(.N), mean_ratio_now_85=mean(tsm_favorableGHCND95/tsm_2081_2100_rcp85), se_ratio=sd(tsm_favorableGHCND95/tsm_2081_2100_rcp85)/sqrt(.N))]

	# by habitat
dat[!is.na(tsm_favorableGHCND95) & !is.na(tsm_2081_2100_rcp85), .(mean_diff_now_85=mean(tsm_favorableGHCND95-tsm_2081_2100_rcp85), se_diff=sd(tsm_favorableGHCND95-tsm_2081_2100_rcp85)/sqrt(.N), mean_ratio_now_85=mean(tsm_favorableGHCND95/tsm_2081_2100_rcp85), se_ratio=sd(tsm_favorableGHCND95/tsm_2081_2100_rcp85)/sqrt(.N)), by=Realm]

# how much better is rcp26 than rcp85?
	# average
dat[!is.na(tsm_2081_2100_rcp26) & !is.na(tsm_2081_2100_rcp85), .(mean_diff_26_85=mean(tsm_2081_2100_rcp26-tsm_2081_2100_rcp85), se_diff=sd(tsm_2081_2100_rcp26-tsm_2081_2100_rcp85)/sqrt(.N), mean_ratio_26_85=mean(tsm_2081_2100_rcp26/tsm_2081_2100_rcp85), se_ratio=sd(tsm_2081_2100_rcp26/tsm_2081_2100_rcp85)/sqrt(.N))]

	# by habitat
dat[!is.na(tsm_2081_2100_rcp26) & !is.na(tsm_2081_2100_rcp85), .(mean_diff_26_85=mean(tsm_2081_2100_rcp26-tsm_2081_2100_rcp85), se_diff=sd(tsm_2081_2100_rcp26-tsm_2081_2100_rcp85)/sqrt(.N), mean_ratio_26_85=mean(tsm_2081_2100_rcp26/tsm_2081_2100_rcp85), se_ratio=sd(tsm_2081_2100_rcp26/tsm_2081_2100_rcp85)/sqrt(.N)), by=Realm]

	
# difference in TSM between average ARR and species-specific ARR
dat[!is.na(tsm_2081_2100_accbyspp_rcp85),.(mean=mean(tsm_2081_2100_acc_rcp85 - tsm_2081_2100_accbyspp_rcp85), se=sd(tsm_2081_2100_acc_rcp85 - tsm_2081_2100_accbyspp_rcp85)/sqrt(.N), N=.N), by=Realm]

dat[!is.na(tsm_2081_2100_accbyspp_rcp85), cor.test(tsm_2081_2100_acc_rcp85, tsm_2081_2100_accbyspp_rcp85)]

			
###################
# models
###################
dat <- fread('temp/warmingtolerance_byspecies_to2100.csv')
	dat$Realm <- as.factor(dat$Realm)

# data.frame for projection
	newdat <- data.frame(Realm=rep(c('Marine', 'Terrestrial'), c(161,161)), lat=rep(-80:80, 2)) # for projection onto a regular vector of latitudes
		newdat$abslat <- abs(newdat$lat)
		newdat$tmax_metric <- 'ctmax'

#### models (unfolded lat)

	# TSM 2081-2100 by lat rcp26
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tsm_2081_2100_rcp26', 'tmax_metric', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMfut26 <- uGamm(tsm_2081_2100_rcp26 ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,], control=glmerControl(optimizer='Nelder_Mead', optCtrl=list(maxfun=5000))) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		plot(modTSMfut26$gam, pages=1)
#		summary(modTSMfut26$gam)
#		summary(modTSMfut26$lme)

		## predictions from GAMM
		temp <- predict(modTSMfut26$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_2081_2100_rcp26 <- temp$fit
		newdat$tsm_2081_2100_rcp26se <- temp$se.fit
			
			# plot predictions
			plot(tsm_2081_2100_rcp26 ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
				lines(tsm_2081_2100_rcp26 + tsm_2081_2100_rcp26se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
				lines(tsm_2081_2100_rcp26 - tsm_2081_2100_rcp26se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
			lines(tsm_2081_2100_rcp26 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
				lines(tsm_2081_2100_rcp26 - tsm_2081_2100_rcp26se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
				lines(tsm_2081_2100_rcp26 + tsm_2081_2100_rcp26se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)

			points(tsm_2081_2100_rcp26 ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
			points(tsm_2081_2100_rcp26 ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)

		# fit all subsets
#		options(na.action = "na.fail")
#		modTSMfut26s <- dredge(modTSMfut26, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTSMfut26s)
#
#		modavgTSMfut26 <- model.avg(modTSMfut26s)
#		summary(modavgTSMfut26)

	# TSM 2081-2100 by lat rcp85
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tsm_2081_2100_rcp85', 'tmax_metric', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMfut85 <- uGamm(tsm_2081_2100_rcp85 ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,], control=glmerControl(optimizer='Nelder_Mead', optCtrl=list(maxfun=5000))) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		plot(modTSMfut85$gam, pages=1)
#		summary(modTSMfut85$gam)
#		summary(modTSMfut85$lme)

		## predictions from GAMM
		temp <- predict(modTSMfut85$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_2081_2100_rcp85 <- temp$fit
		newdat$tsm_2081_2100_rcp85se <- temp$se.fit
			
			# plot predictions
#			plot(tsm_2081_2100_rcp85 ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tsm_2081_2100_rcp85 + tsm_2081_2100_rcp85se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_2081_2100_rcp85 - tsm_2081_2100_rcp85se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_2081_2100_rcp85 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_2081_2100_rcp85 - tsm_2081_2100_rcp85se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_2081_2100_rcp85 + tsm_2081_2100_rcp85se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_2081_2100_rcp85 ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_2081_2100_rcp85 ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)

		# fit all subsets
#		options(na.action = "na.fail")
#		modTSMfut85s <- dredge(modTSMfut85, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTSMfut85s)
#
#		modavgTSMfut85 <- model.avg(modTSMfut85s)
#		summary(modavgTSMfut85)

	# TSM 2081-2100 with acclimation by lat RCP2.6
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tsm_2081_2100_acc_rcp26', 'tmax_metric', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMfut_acc26 <- uGamm(tsm_2081_2100_acc_rcp26 ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,])
#		plot(modTSMfut_acc26$gam, pages=1)
#		summary(modTSMfut_acc26$gam)
#		summary(modTSMfut_acc26$lme)

		## predictions from GAMM
		temp <- predict(modTSMfut_acc26$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_2081_2100_acc_rcp26 <- temp$fit
		newdat$tsm_2081_2100_acc_rcp26se <- temp$se.fit
			
			# plot predictions
#			plot(tsm_2081_2100_acc_rcp26 ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#			abline(h=0, lty=2, col='grey')
#
#				lines(tsm_2081_2100_acc_rcp26 + tsm_2081_2100_acc_rcp26se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_2081_2100_acc_rcp26 - tsm_2081_2100_acc_rcp26se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_2081_2100_acc_rcp26 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_2081_2100_acc_rcp26 - tsm_2081_2100_acc_rcp26se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_2081_2100_acc_rcp26 + tsm_2081_2100_acc_rcp26se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_2081_2100_acc_rcp26 ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_2081_2100_acc_rcp26 ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


		# fit all subsets
#		options(na.action = "na.fail")
#		modTSMfut_acc85s <- dredge(modTSMfut_acc85, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTSMfut_acc85s)
#
#		modavgTSMfut_acc85 <- model.avg(modTSMfut_acc85s)
#		summary(modavgTSMfut_acc85)

	# TSM 2081-2100 with acclimation by lat RCP8.5
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tsm_2081_2100_acc_rcp85', 'tmax_metric', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMfut_acc85 <- uGamm(tsm_2081_2100_acc_rcp85 ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,])
#		plot(modTSMfut_acc85$gam, pages=1)
#		summary(modTSMfut_acc85$gam)
#		summary(modTSMfut_acc85$lme)

		## predictions from GAMM
		temp <- predict(modTSMfut_acc85$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_2081_2100_acc_rcp85 <- temp$fit
		newdat$tsm_2081_2100_acc_rcp85se <- temp$se.fit
			
			# plot predictions
#			plot(tsm_2081_2100_acc_rcp85 ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#			abline(h=0, lty=2, col='grey')
#
#				lines(tsm_2081_2100_acc_rcp85 + tsm_2081_2100_acc_rcp85se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_2081_2100_acc_rcp85 - tsm_2081_2100_acc_rcp85se ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_2081_2100_acc_rcp85 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_2081_2100_acc_rcp85 - tsm_2081_2100_acc_rcp85se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_2081_2100_acc_rcp85 + tsm_2081_2100_acc_rcp85se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_2081_2100_acc_rcp85 ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_2081_2100_acc_rcp85 ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


		# fit all subsets
#		options(na.action = "na.fail")
#		modTSMfut_acc85s <- dredge(modTSMfut_acc85, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTSMfut_acc85s)
#
#		modavgTSMfut_acc85 <- model.avg(modTSMfut_acc85s)
#		summary(modavgTSMfut_acc85)

	# Write out predictions
	write.csv(newdat, file='temp/warmingtolerance_bylat_to2100.csv', row.names=FALSE)
