# Data exploration and models for TSM

require(mgcv)
require(data.table)
require(lme4)
require(MuMIn) # for dredge
source('scripts/findPeaksGAM.r') # for location and number of peaks


#############
## functions
#############


#############
# Read in dataset
#############
dat <- fread('temp/warmingtolerance_byspecies.csv', stringsAsFactors=TRUE)

# make critical the first factor
dat$tmax_metric <- relevel(dat$tmax_metric, 'crit')


#####################
## Examine dataset
#####################
# how much data by animal type
	nrow(dat) # 406

	# Tmax raw
	i <- complete.cases(dat[,.(Realm, lat, Tmax)])
	sum(i)
	dat[i, table(Realm)]
	dat[i, table(animal.type, Realm)]
	dat[i, length(unique(Class))]
	dat[i, range(lat), by=Realm]

	# Tmax acclimation temp
	i <- complete.cases(dat[, .(Realm, lat, tmax.accsum.elev, Phylum, Class, Order, Family, Genus)])
	sum(i)
	dat[i, table(Realm)]
	dat[i, table(animal.type, Realm)]

	# Operative temperature
	i <- complete.cases(dat[, .(Realm, lat, tb_favorableGHCND95, Phylum, Class, Order, Family, Genus)])
	sum(i)
	dat[i, table(Realm)]
	dat[i, table(animal.type, Realm)]

	# TSM present time, without acclimation GHCND
	i <- complete.cases(dat[, .(Realm, lat, tmax_metric, tsm_favorableGHCND95_noacc, Phylum, Class, Order, Family, Genus)]) 
	sum(i)
	dat[i, table(Realm)]
	dat[i, table(animal.type, Realm)]

	# TSM present time, with acclimation GHCND
	i <- complete.cases(dat[, .(Realm, lat, tmax_metric, tsm_favorableGHCND95, Phylum, Class, Order, Family, Genus)]) 
	sum(i)
	dat[i, table(Realm)]
	dat[i, table(animal.type, Realm)]


# how many phyla by habitat
aggregate(list(nphyla=dat$Phylum, nclass=dat$Class, norder=dat$Family, nfamily=dat$Family, ngenera=dat$Genus), by=list(Realm=dat$Realm), FUN=function(x) length(unique(x)))

# how many classes?
	# Tmax acclimation temp
	i <- complete.cases(dat[, .(Realm, lat, tmax.accsum.elev, Phylum, Class, Order, Family, Genus)])
	sum(i)
	t(t(table(dat$Class[i])))

	# TSM present time, with acclimation
	i <- complete.cases(dat[, .(Realm, lat, tmax_metric, tsm_favorable, Phylum, Class, Order, Family, Genus)]) 
	table(dat$Class[i])


# latitudinal range
	# Tmax
	i <- complete.cases(dat[,.(Realm, lat, Tmax, Phylum, Class, Order, Family, Genus)])
	sum(i)
	aggregate(list(range=dat$lat[i]), by=list(habitat=dat$Realm[i]), FUN=range)

	# TSM present time, with acclimation
	i <- complete.cases(dat[, .(Realm, lat, tsm_favorableGHCND95, Phylum, Class, Order, Family, Genus)]) 
	sum(i)
	aggregate(list(range=dat$lat[i]), by=list(habitat=dat$Realm[i]), FUN=range)


# acclimation effect size
	# acclimation temperature
dat[Realm=='Terrestrial', summary(Tmax - tmax.accsum.elev)]
dat[Realm=='Terrestrial', sd(Tmax - tmax.accsum.elev, na.rm=TRUE)]

dat[Realm=='Marine', summary(Tmax - tmax.accsum.elev)]
dat[Realm=='Marine', sd(Tmax - tmax.accsum.elev, na.rm=TRUE)]



# odd values
	# tsm < 0
	dat[,sum(tsm_maxhr<0, na.rm=TRUE)]
	dat[tsm_favorable<0 | tsm_favorable95<0 | tsm_favorableGHCND95<0, .(Species, Genus, animal.type, Realm, lat, long_max, elev.grid, tmax_metric, Tmax, tmax.accsum.elev, thabmaxhr, tsm_maxhr, tsm_maxhr_accsum, tsm_favorable, tsm_favorable95, tsm_favorableGHCND95)]


##################################
# Average in bands and write out
##################################

# averages near equator vs. farther
	dat[lat > -10 & lat < 10 & Realm=='Terrestrial', .(mean(tsm_favorableGHCND95), sd(tsm_favorableGHCND95)/.N, .N)]
	dat[lat > -3 & lat < 3 & Realm=='Terrestrial', .(mean(tsm_favorableGHCND95), sd(tsm_favorableGHCND95)/.N, .N)]
	dat[(lat > -35 & lat < -25) & Realm=='Terrestrial', .(mean(tsm_favorableGHCND95), sd(tsm_favorableGHCND95)/.N, .N)]
	dat[(lat > 20 & lat < 30) & Realm=='Terrestrial', .(mean(tsm_favorableGHCND95), sd(tsm_favorableGHCND95)/.N, .N)]
	dat[((lat > -35 & lat < -15) | (lat > 15 & lat < 35)) & Realm=='Terrestrial', .(mean(tsm_favorableGHCND95), sd(tsm_favorableGHCND95)/.N, .N)]
	
# average in bands
	TSMbylat <- dat[Realm=='Terrestrial' & !is.na(tsm_favorableGHCND95), .(mean=mean(tsm_favorableGHCND95), se=sd(tsm_favorableGHCND95)/.N, n=.N), by=.(latband=floor(lat/10)*10+5)]

		# small plot to check
		setkey(TSMbylat, latband)
		TSMbylat[,plot(latband, mean, type='o', pch=16, cex=0.5)]
		TSMbylat[,arrows(latband, mean-se, latband, mean+se, length=0, angle=90, code=3)]

# write out
write.csv(TSMbylat, file='temp/warmingtolerance_bylatband_land.csv', row.names=FALSE)

#############
# Models
#############

#### projections dataframe
	newdat <- data.frame(Realm=rep(c('Marine', 'Terrestrial'), c(161,161)), lat=rep(-80:80, 2)) # for projection onto a regular vector of latitudes
		newdat$tmax_metric <- 'crit'

#### models for body temperature (Tb)		
	# present-time body temperature in favorable microclimates, 95% max hr with GHCND
		i <- complete.cases(dat[,c('Realm', 'lat', 'tb_favorableGHCND95', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTbGHCND95dat <- dat[i,]
	modTbGHCND95 <- uGamm(tb_favorableGHCND95 ~ Realm + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modTbGHCND95dat) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTbGHCND95$gam)
	
		## predictions from GAMM
		temp <- predict(modTbGHCND95$gam, newdata=newdat, se.fit=TRUE)
		newdat$tb_favorableGHCND95 <- temp$fit
		newdat$tb_favorableGHCND95se <- temp$se.fit
		
			# plot predictions
#			plot(tb_favorableGHCND95 ~ lat, data=newdat[newdat$Realm=='Terrestrial',], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tb_favorableGHCND95 + tb_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#				lines(tb_favorableGHCND95 - tb_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#			lines(tb_favorableGHCND95 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tb_favorableGHCND95 - tb_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tb_favorableGHCND95 + tb_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tb_favorableGHCND95 ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tb_favorableGHCND95 ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


	# present-time body temperature in full sun, 95% max hr with GHCND
		i <- complete.cases(dat[,c('Realm', 'lat', 'tb_sunGHCND95', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTbsunGHCND95 <- uGamm(tb_sunGHCND95 ~ Realm + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,]) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTbsunGHCND95$gam)

		## predictions from GAMM
		temp <- predict(modTbsunGHCND95$gam, newdata=newdat, se.fit=TRUE)
		newdat$tb_sunGHCND95 <- temp$fit
		newdat$tb_sunGHCND95se <- temp$se.fit
		
			# plot predictions
#			plot(tb_sunGHCND95 ~ lat, data=newdat[newdat$Realm=='Terrestrial',], type='l', col='green', ylim=c(-10,60), xlim=c(-80,80))
#				lines(tb_sunGHCND95 + tb_sunGHCND95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#				lines(tb_sunGHCND95 - tb_sunGHCND95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#			lines(tb_sunGHCND95 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tb_sunGHCND95 - tb_sunGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tb_sunGHCND95 + tb_sunGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tb_sunGHCND95 ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tb_sunGHCND95 ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)
#
#			lines(tb_favorableGHCND95 ~ lat, data=newdat[newdat$Realm=='Marine',], col='black')
#				lines(tb_favorableGHCND95 - tb_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='black', lty=2)
#				lines(tb_favorableGHCND95 + tb_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='black', lty=2)



#### models for Tmax
	# Tmax (summer-acclimated)
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax.accsum.elev', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTmaxdat <- dat[i,]
	modTmax <- uGamm(tmax.accsum.elev ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modTmaxdat) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTmax$gam)

		## predictions from GAMM
		temp <- predict(modTmax$gam, newdata=newdat, se.fit=TRUE)
		newdat$tmax <- temp$fit
		newdat$tmaxse <- temp$se.fit
			
			# plot predictions
#		plot(tmax ~ lat, data=newdat[newdat$Realm=='Terrestrial',], type='l', col='green', ylim=c(10,50), xlim=c(-80,80))
#			lines(tmax + tmaxse ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#			lines(tmax - tmaxse ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#		lines(tmax ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#			lines(tmax - tmaxse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#			lines(tmax + tmaxse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#		points(tmax.accsum.elev ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#		points(tmax.accsum.elev ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


	# Tmax with species-specific ARR data
	i <- complete.cases(dat[,c('Realm', 'lat', 'tmax.accsumbyspp.elev', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTmaxARRbyspp <- uGamm(tmax.accsumbyspp.elev ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,]) 
#		summary(modTmaxARRbyspp$gam)

		## predictions from GAMM
		temp <- predict(modTmaxARRbyspp$gam, newdata=newdat, se.fit=TRUE)
		newdat$tmaxARRbyspp <- temp$fit
		newdat$tmaxARRbysppse <- temp$se.fit



#### models for TSM
		
	# present-time TSM (annual, no acclimation)
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_ann_elev', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMann <- uGamm(tsm_ann_elev ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,]) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTSMann$gam)

		## predictions from GAMM
		temp <- predict(modTSMann$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_ann <- temp$fit
		newdat$tsm_annse <- temp$se.fit

			# plot predictions
#			plot(tsm_ann ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tsm_ann + tsm_annse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_ann - tsm_annse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_ann ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_ann - tsm_annse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_ann + tsm_annse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_ann_elev ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_ann ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


	# present-time TSM (summer)
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_sum_elev', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMsum <- uGamm(tsm_sum_elev ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,]) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTSMsum$gam)

		## predictions from GAMM
		temp <- predict(modTSMsum$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_sum <- temp$fit
		newdat$tsm_sumse <- temp$se.fit

			# plot predictions
#			plot(tsm_sum ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tsm_sum + tsm_sumse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_sum - tsm_sumse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_sum ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_sum - tsm_sumse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_sum + tsm_sumse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_sum_elev ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_sum_elev ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


	# present-time TSM (hottest month)
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_maxmo_elev', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMmaxmo <- uGamm(tsm_maxmo_elev ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,]) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTSMmaxmo$gam)

		## predictions from GAMM
		temp <- predict(modTSMmaxmo$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_maxmo <- temp$fit
		newdat$tsm_maxmose <- temp$se.fit

			# plot predictions
#			plot(tsm_maxmo ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tsm_maxmo + tsm_maxmose ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_maxmo - tsm_maxmose ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_maxmo ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_maxmo - tsm_maxmose ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_maxmo + tsm_maxmose ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_maxmo_elev ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_maxmo_elev ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


	# present-time TSM in exposed microclimates (95% hottest hour GHCND Tb)
	# with summer acclimation corrections
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_exposedGHCND95', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMexp <- uGamm(tsm_exposedGHCND95 ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,]) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTSMexp$lme)
#		summary(modTSMexp$gam)

		## predictions from GAMM
		temp <- predict(modTSMexp$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_exposedGHCND95 <- temp$fit
		newdat$tsm_exposedGHCND95se <- temp$se.fit
			
			# plot predictions
#			plot(tsm_exposed ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-25,20), xlim=c(-80,80))
#				lines(tsm_exposed + tsm_exposedse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_exposed - tsm_exposedse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_exposed ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_exposed - tsm_exposedse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_exposed + tsm_exposedse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_exposed ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_exposed ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)



	# present-time TSM in favorable microclimates (95% hottest hour with GHCND Tb) (WITH acclimation, Marine BT)
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_favorableGHCND95', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modGHCND95dat <- dat[i,]
	modGHCND95 <- uGamm(tsm_favorableGHCND95 ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modGHCND95dat, control=lmeControl(niterEM=0, msMaxIter=100)) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modGHCND95$lme)
#		summary(modGHCND95$gam)
		
	
		## predictions from GAMM
		temp <- predict(modGHCND95$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_favorableGHCND95 <- temp$fit
		newdat$tsm_favorableGHCND95se <- temp$se.fit
			
			# plot predictions
#			plot(tsm_favorableGHCND95 ~ lat, data=newdat[newdat$Realm=='Terrestrial',], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tsm_favorableGHCND95 + tsm_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#				lines(tsm_favorableGHCND95 - tsm_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#			lines(tsm_favorableGHCND95 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_favorableGHCND95 - tsm_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_favorableGHCND95 + tsm_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_favorableGHCND95 ~ lat, data=dat[i & Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_favorableGHCND95 ~ lat, data=dat[i & Realm=='Marine',], col='blue', cex=0.5)
#
#				dat[i & Realm=='Marine' & demers_pelag=='pelagic' & big==TRUE & mobility=='swim', points(tsm_favorableGHCND95 ~ lat, col='red', cex=0.5)]
#			
#
#			lines(tsm_favorable95 ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='red')
#				lines(tsm_favorable95 - tsm_favorable95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='red', lty=2)
#				lines(tsm_favorable95 + tsm_favorable95se ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='red', lty=2)
#
#	
#			datorig <- fread('temp/warmingtolerance_byspecies_2017-12-19.csv')
#			# use 95% warmest hour for TSMs. favorable microclimates (shade or wet skin + behavioral thermoregulation of 10degC)
#			datorig[, tsm_favorable95_10 := as.numeric(rep(NA, .N))]
#			datorig[habitat=='terrestrial' & animal.type!='amphibian', tsm_favorable95_10 := tsm_NM_2m_airshade95_accsum.elev]
#			datorig[habitat=='terrestrial' & animal.type=='amphibian', tsm_favorable95_10 := tsm_NM_shade_body_wet95_accsum.elev]
#			datorig[habitat=='marine', tsm_favorable95_10 := tsm_95hr_marineBT10_accsum]
#
#			points(tsm_favorable95_10 ~ lat, data=datorig[habitat=='marine',], col='red', cex=0.3, pch=16)

		# minimum marine TSM vs. minimum terrestrial TSM
		min(newdat$tsm_favorableGHCND95[newdat$Realm=='Terrestrial']) - min(newdat$tsm_favorableGHCND95[newdat$Realm=='Marine'])
	

	# Species-specific ARR data: present-time TSM in favorable microclimates (95% hottest hour with GHCND Tb) (WITH acclimation, Marine BT)
	i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_favorableGHCND95byspp', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modGHCND95ARRbysppdat <- dat[i,]
	modGHCND95ARRbyspp <- uGamm(tsm_favorableGHCND95byspp ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modGHCND95ARRbysppdat, control=lmeControl(niterEM=50, msMaxIter=1000))
#		summary(modGHCND95ARRbyspp$gam)





	# present-time TSM in favorable microclimates (95% hottest hour GHCND Tb) (with acclimation, WITHOUT marine behavioral thermoregulation)
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_favorableGHCND95_nomarineBT', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modnoMBT <- uGamm(tsm_favorableGHCND95_nomarineBT ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=dat[i,], control=lmeControl(niterEM=0, msMaxIter=100))
#		summary(modnoMBT$gam)

		## predictions from GAMM
		temp <- predict(modnoMBT$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_favorableGHCND95_nomarineBT <- temp$fit
		newdat$tsm_favorableGHCND95_nomarineBTse <- temp$se.fit
			
			# plot predictions. TSMs with thermoregulation in marine animals in red.
#			plot(tsm_favorableGHCND95_nomarineBT ~ lat, data=newdat[newdat$Realm=='Terrestrial',], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tsm_favorableGHCND95_nomarineBT + tsm_favorableGHCND95_nomarineBTse ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#				lines(tsm_favorableGHCND95_nomarineBT - tsm_favorableGHCND95_nomarineBTse ~ lat, data=newdat[newdat$Realm=='Terrestrial',], col='green', lty=2)
#			lines(tsm_favorableGHCND95_nomarineBT ~ lat, data=newdat[newdat$Realm=='Marine',], col='red')
#				lines(tsm_favorableGHCND95_nomarineBT - tsm_favorableGHCND95_nomarineBTse ~ lat, data=newdat[newdat$Realm=='Marine',], col='red', lty=2) #w/out marine BT in red
#				lines(tsm_favorableGHCND95_nomarineBT + tsm_favorableGHCND95_nomarineBTse ~ lat, data=newdat[newdat$Realm=='Marine',], col='red', lty=2)
#
#			# overlay with marine BT
#			lines(tsm_favorableGHCND95 ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_favorableGHCND95 - tsm_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_favorableGHCND95 + tsm_favorableGHCND95se ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_favorableGHCND95_nomarineBT ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_favorableGHCND95_nomarineBT ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)


	# present-time TSM in favorable microclimates (95% hottest hour with GHCND Tb) (WITH acclimation, MOREs Marine BT)
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_favorableGHCND95_marBTmore', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modGHCND95_marBTmoredat <- dat[i,]
	modGHCND95_marBTmore <- uGamm(tsm_favorableGHCND95_marBTmore ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modGHCND95_marBTmoredat, control=lmeControl(niterEM=0, msMaxIter=100)) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modGHCND95_marBTmore$gam)
#			summary(modGHCND95$gam) # to compare

	# present-time TSM in favorable microclimates (95% hottest hour with GHCND Tb) (WITH acclimation, LESS Marine BT)
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_favorableGHCND95_marBTless', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modGHCND95_marBTlessdat <- dat[i,]
	modGHCND95_marBTless <- uGamm(tsm_favorableGHCND95_marBTless ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modGHCND95_marBTlessdat, control=lmeControl(niterEM=0, msMaxIter=100)) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modGHCND95_marBTless$gam)
#			summary(modGHCND95_marBTmore$gam) # to compare
#			summary(modGHCND95$gam) # to compare


	# present-time TSM in favorable microclimates (95% hottest hour, GHCND, WITH marine BT)
	# NO acclimation corrections
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_favorableGHCND95_noacc', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMfav_noaccdat <- dat[i,]
	modTSMfav_noacc <- uGamm(tsm_favorableGHCND95_noacc ~ Realm + tmax_metric + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modTSMfav_noaccdat) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTSMfav_noacc$gam)

		## predictions from GAMM
		temp <- predict(modTSMfav_noacc$gam, newdata=newdat, se.fit=TRUE)
		newdat$tsm_favorable_noacc <- temp$fit
		newdat$tsm_favorable_noaccse <- temp$se.fit
			
			# plot predictions
#			plot(tsm_favorable_noacc ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], type='l', col='green', ylim=c(-10,30), xlim=c(-80,80))
#				lines(tsm_favorable_noacc + tsm_favorable_noaccse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#				lines(tsm_favorable_noacc - tsm_favorable_noaccse ~ lat, data=newdat[newdat$Realm=='Terrestrial' & newdat$abslat<50,], col='green', lty=2)
#			lines(tsm_favorable_noacc ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue')
#				lines(tsm_favorable_noacc - tsm_favorable_noaccse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#				lines(tsm_favorable_noacc + tsm_favorable_noaccse ~ lat, data=newdat[newdat$Realm=='Marine',], col='blue', lty=2)
#
#			points(tsm_favorable_noacc ~ lat, data=dat[dat$Realm=='Terrestrial',], col='green', cex=0.5)
#			points(tsm_favorable_noacc ~ lat, data=dat[dat$Realm=='Marine',], col='blue', cex=0.5)



	# present-time TSM in favorable microclimates (95% hottest hour, GHCND, WITH marine BT)
	# NO acclimation corrections, but acclimation temperature included as a fixed effect
		# gamm
		i <- complete.cases(dat[,c('Realm', 'lat', 'tmax_metric', 'tsm_favorableGHCND95_noacc', 'tmax_acc', 'Phylum', 'Class', 'Order', 'Family', 'Genus')]); sum(i); nrow(dat)
	modTSMfav_acctermdat <- dat[i,]
	modTSMfav_accterm <- uGamm(tsm_favorableGHCND95_noacc ~ Realm + tmax_metric + tmax_acc + s(lat, by=Realm), random=list(Phylum=~1, Class=~1, Order=~1, Family=~1, Genus=~1), data=modTSMfav_acctermdat) # use uGamm instead of gamm, since former can be used with dredge. Just a wrapper function for gamm.
#		summary(modTSMfav_accterm$gam)



# Write out predictions
write.csv(newdat, file='temp/warmingtolerance_bylatlon.csv', row.names=FALSE)


#############################
## Calculate peak locations
## (need models from above)
#############################


	# present-time body temperature in favorable microclimates, 95% max hr with GHCND		
		# how many peaks?
		peaksmeanTbGHCND95_terr <- findPeaksGAM(modTbGHCND95$gam, newdat[newdat$Realm=='Terrestrial',], xvar='lat', xtol=4) # from the main prediction
			peaksmeanTbGHCND95_terr$npeaks
			peaksmeanTbGHCND95_terr$peaklocs			
		peaksTbGHCND95_terr <- findPeaksGAM_CI(mod=modTbGHCND95$gam, newdat=newdat[newdat$Realm=='Terrestrial',], n=1000, xvar='lat', xtol=4) # xtol sets lat window to 4° in each direction
		peaksmeanTbGHCND95_mar <- findPeaksGAM(modTbGHCND95$gam, newdat[newdat$Realm=='Marine',], xvar='lat', xtol=4) # from the main prediction			
		peaksTbGHCND95_mar <- findPeaksGAM_CI(mod=modTbGHCND95$gam, newdat=newdat[newdat$Realm=='Marine',], n=1000, xvar='lat', xtol=4)
		
		# write out
		saveRDS(peaksTbGHCND95_terr, file='temp/peaks_modTbGHCND95_terr.rds')
		saveRDS(peaksTbGHCND95_mar, file='temp/peaks_modTbGHCND95_mar.rds')



#### models for Tmax
	# Tmax (summer-acclimated)
		# how many peaks?
		peaksmeanTmax_terr <- findPeaksGAM(modTmax$gam, newdat[newdat$Realm=='Terrestrial',], xvar='lat', xtol=4) # from the main prediction
			peaksmeanTmax_terr$npeaks
			peaksmeanTmax_terr$peaklocs			
		peaksTmax_terr <- findPeaksGAM_CI(mod=modTmax$gam, newdat=newdat[newdat$Realm=='Terrestrial',], n=1000, xvar='lat', xtol=4) # xtol sets lat window to 4° in each direction
		peaksmeanTmax_mar <- findPeaksGAM(modTmax$gam, newdat[newdat$Realm=='Marine',], xvar='lat', xtol=4) # from the main prediction			
		peaksTmax_mar <- findPeaksGAM_CI(mod=modTmax$gam, newdat=newdat[newdat$Realm=='Marine',], n=1000, xvar='lat', xtol=4)
		
		# write out
		saveRDS(peaksTmax_terr, file='temp/peaks_modTmax_terr.rds')
		saveRDS(peaksTmax_mar, file='temp/peaks_modTmax_mar.rds')


#### models for TSM
	# present-time TSM in favorable microclimates (95% hottest hour with GHCND Tb) (WITH acclimation, Marine BT)
		# how many valleys?
		peaksmeanTSM_favorableGHCND95_terr <- findPeaksGAM(modGHCND95$gam, newdat[newdat$Realm=='Terrestrial',], xvar='lat', xtol=4, findmaxima=FALSE) # from the main prediction
			peaksmeanTSM_favorableGHCND95_terr$npeaks
			peaksmeanTSM_favorableGHCND95_terr$peaklocs			
		peaksTSM_favorableGHCND95_terr <- findPeaksGAM_CI(mod=modGHCND95$gam, newdat=newdat[newdat$Realm=='Terrestrial',], n=1000, xvar='lat', xtol=4, findmaxima=FALSE) # xtol sets lat window to 4° in each direction
		peaksmeanTSM_favorableGHCND95_mar <- findPeaksGAM(modGHCND95$gam, newdat[newdat$Realm=='Marine',], xvar='lat', xtol=4, findmaxima=FALSE)
		peaksTSM_favorableGHCND95_mar <- findPeaksGAM_CI(mod=modGHCND95$gam, newdat=newdat[newdat$Realm=='Marine',], n=1000, xvar='lat', xtol=4, findmaxima=FALSE)
		
		# write out
		saveRDS(peaksTSM_favorableGHCND95_terr, file='temp/peaks_modGHCND95_terr.rds')
		saveRDS(peaksTSM_favorableGHCND95_mar, file='temp/peaks_modGHCND95_mar.rds')



#############################
## Calculate RVI
## (need models from above)
#############################

#### models for operative temperature (Te)
	# present-time body temperature in favorable microclimates, 95% max hr with GHCND
		# fit all subsets
		options(na.action = "na.fail")
		modTbGHCND95s <- dredge(modTbGHCND95, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTbGHCND95s, delta < 6)

		modavgTbGHCND95 <- model.avg(modTbGHCND95s)
#		summary(modavgTEGHCND95)


#### models for Tmax
	# Tmax (summer-acclimated)
		# fit all subsets
		options(na.action = "na.fail")
		modTmaxs <- dredge(modTmax, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTmaxs)

		modavgTmax <- model.avg(modTmaxs)
		#summary(modavgTmax)


#### models for TSM
	# present-time TSM in favorable microclimates (95% hottest hour with GHCND) (WITH acclimation, Marine BT)
		# fit all subsets
		options(na.action = "na.fail")
		modGHCND95s <- dredge(modGHCND95, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modGHCND95s, delta < 6)

		modavgGHCND95 <- model.avg(modGHCND95s)
#		summary(modavgGHCND95)


	# Species-specific ARR data: present-time TSM in favorable microclimates (95% hottest hour with GHCND Tb) (WITH acclimation, Marine BT)
		# fit all subsets
		options(na.action = "na.fail")
		modGHCND95ARRbyspps <- dredge(modGHCND95ARRbyspp, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modGHCND95ARRbyspps, delta < 6)

		modavgGHCND95ARRbyspp <- model.avg(modGHCND95ARRbyspps)
#		summary(modavgGHCND95ARRbyspp)

	# present-time TSM in favorable microclimates (95% hottest hour with GHCND) (with acclimation, MORE Marine BT)
		# fit all subsets
		options(na.action = "na.fail")
		modGHCND95_marBTmores <- dredge(modGHCND95_marBTmore, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modGHCND95_marBTmores, delta < 6)

		modavgGHCND95_marBTmore <- model.avg(modGHCND95_marBTmores)
#		summary(modavgGHCND95_marBTmore)

	# present-time TSM in favorable microclimates (95% hottest hour with GHCND) (with acclimation, LESS Marine BT)
		# fit all subsets
		options(na.action = "na.fail")
		modGHCND95_marBTlesss <- dredge(modGHCND95_marBTless, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modGHCND95_marBTlesss, delta < 6)

		modavgGHCND95_marBTless <- model.avg(modGHCND95_marBTlesss)
#		summary(modavgGHCND95_marBTless)



	# present-time TSM in favorable microclimates (95% hottest hour, GHCND, WITH marine BT)
	# NO acclimation corrections
		# fit all subsets
		options(na.action = "na.fail")
		modTSMfav_noaccs <- dredge(modTSMfav_noacc, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTSMfav_noaccs, delta < 6)

		modavgTSMfav_noacc <- model.avg(modTSMfav_noaccs)
#		summary(modavgTSMfav_noacc)

	# present-time TSM in favorable microclimates (95% hottest hour, GHCND, WITH marine BT)
	# NO acclimation corrections, but acclimation temperature included as a fixed effect
		# fit all subsets
		options(na.action = "na.fail")
		modTSMfav_accterms <- dredge(modTSMfav_accterm, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#		subset(modTSMfav_accterms, delta < 6)

		modavgTSMfav_accterm <- model.avg(modTSMfav_accterms)
#		summary(modavgTSMfav_accterm)




####################################################
### Write out table of peaks and valleys in GAMMS
####################################################
out <- data.frame(Variable=c('T_b terrestrial', 'T_b marine', 'T_max terrestrial', 'T_max marine', 'TSM_favorable terrestrial', 'TSM_favorable marine'), Peak1=character(6), Peak2=character(6), Peak3=character(6), stringsAsFactors=FALSE)

# calculate quantiles
qTbGHCND95_terr1 <- quantile(sapply(peaksTbGHCND95_terr$peaklocs, FUN=function(x, min=-50, max=0){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTbGHCND95_terr2 <- quantile(sapply(peaksTbGHCND95_terr$peaklocs, FUN=function(x, min=0, max=50){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTbGHCND95_mar1 <- quantile(sapply(peaksTbGHCND95_mar$peaklocs, FUN=function(x, min=-50, max=50){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)

qTmax_terr1 <- quantile(sapply(peaksTmax_terr$peaklocs, FUN=function(x, min=-50, max=0){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTmax_terr2 <- quantile(sapply(peaksTmax_terr$peaklocs, FUN=function(x, min=0, max=30){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTmax_mar1 <- quantile(sapply(peaksTmax_mar$peaklocs, FUN=function(x, min=-50, max=-30){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTmax_mar2 <- quantile(sapply(peaksTmax_mar$peaklocs, FUN=function(x, min=-30, max=0){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTmax_mar3 <- quantile(sapply(peaksTmax_mar$peaklocs, FUN=function(x, min=0, max=30){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)

qTSM_favorableGHCND95_terr1 <- quantile(sapply(peaksTSM_favorableGHCND95_terr$peaklocs, FUN=function(x, min=-50, max=0){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTSM_favorableGHCND95_terr2 <- quantile(sapply(peaksTSM_favorableGHCND95_terr$peaklocs, FUN=function(x, min=0, max=50){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)
qTSM_favorableGHCND95_mar1 <- quantile(sapply(peaksTSM_favorableGHCND95_mar$peaklocs, FUN=function(x, min=-30, max=50){ret <- x[x>min & x<max]; ifelse(length(ret)==1, ret, NA)}), probs=c(0.025, 0.975), na.rm=TRUE)

# fill table cells
out[1,'Peak1'] <- paste(peaksmeanTbGHCND95_terr$peaklocs[[1]][1], ' [', paste(qTbGHCND95_terr1, collapse=','), ']', sep='')
out[1,'Peak2'] <- paste(peaksmeanTbGHCND95_terr$peaklocs[[1]][2], ' [', paste(qTbGHCND95_terr2, collapse=','), ']', sep='')
out[2,'Peak1'] <- paste(peaksmeanTbGHCND95_mar$peaklocs[[1]][1], ' [', paste(qTbGHCND95_mar1, collapse=','), ']', sep='')
out[3,'Peak1'] <- paste(peaksmeanTmax_terr$peaklocs[[1]][1], ' [', paste(qTmax_terr1, collapse=','), ']', sep='')
out[3,'Peak2'] <- paste(peaksmeanTmax_terr$peaklocs[[1]][2], ' [', paste(qTmax_terr2, collapse=','), ']', sep='')
out[4,'Peak1'] <- paste(peaksmeanTmax_mar$peaklocs[[1]][1], ' [', paste(qTmax_mar1, collapse=','), ']', sep='')
out[4,'Peak2'] <- paste(peaksmeanTmax_mar$peaklocs[[1]][2], ' [', paste(qTmax_mar2, collapse=','), ']', sep='')
out[4,'Peak3'] <- paste(peaksmeanTmax_mar$peaklocs[[1]][3], ' [', paste(qTmax_mar3, collapse=','), ']', sep='')
out[5,'Peak1'] <- paste(peaksmeanTSM_favorableGHCND95_terr$peaklocs[[1]][1], ' [', paste(qTSM_favorableGHCND95_terr1, collapse=','), ']', sep='')
out[5,'Peak2'] <- paste(peaksmeanTSM_favorableGHCND95_terr$peaklocs[[1]][2], ' [', paste(qTSM_favorableGHCND95_terr2, collapse=','), ']', sep='')
out[6,'Peak1'] <- paste(peaksmeanTSM_favorableGHCND95_mar$peaklocs[[1]][1], ' [', paste(qTSM_favorableGHCND95_mar1, collapse=','), ']', sep='')

# write out
out
write.csv(out, file='tables/peak_locations.csv', row.names=FALSE)


##############################################
## Write out table of GAMM results
## need models from above and model averages
##############################################
out2 <- data.frame(model=character(0), r2=character(0), n=character(0), variable=character(0), smooth=numeric(0), estimate=numeric(0), se=numeric(0), t=numeric(0), p=numeric(0), RVI=numeric(0), edf=numeric(0), F=numeric(0))

modsout <- list(Tmax=modTmax, Tb=modTbGHCND95, TSM=modGHCND95, TSMnoacc=modTSMfav_noacc, TSMaccterm=modTSMfav_accterm, TSMARRbyspp=modGHCND95ARRbyspp, TSM_marBTmore=modGHCND95_marBTmore, TSM_marBTless=modGHCND95_marBTless)
modavgsout <- list(Tmax=modavgTmax, Tb=modavgTbGHCND95, TSM=modavgGHCND95, TSMnoacc=modavgTSMfav_noacc, TSMaccterm=modavgTSMfav_accterm, TSMARRbyspp=modavgGHCND95ARRbyspp, TSM_marBTmore=modavgGHCND95_marBTmore, TSM_marBTless=modavgGHCND95_marBTless)

for(i in 1:length(modsout)){
	#terms <- unlist(strsplit(gsub('.*~', '', as.character(modsout[[i]]$gam$pred.formula)), split=' \\+ '))
	summ <- summary(modsout[[i]]$gam)
	
	# non-smooth terms
	for(j in 1:nrow(summ$p.table)){	
		var <- gsub('\\(|\\)|Terrestrial|LT50|2LT|leth', '', rownames(summ$p.table)[j])
		indxrvi <- grep(var, names(modavgsout[[i]]$importance))
		temp <- data.frame(
			model = names(modsout)[i],
			r2 =if(j==1) round(summ$r.sq,2) else '',
			n = if(j==1) summ$n else '',
			variable = var,
			smooth = 0,
			estimate = round(summ$p.table[j, 'Estimate'],2),
			se = round(summ$p.table[j, 'Std. Error'],2),
			t = round(summ$p.table[j, 't value'],2),
			p = signif(summ$p.table[j, 'Pr(>|t|)'],2),
			RVI = round(ifelse(length(indxrvi)>0, no=NA, yes=modavgsout[[i]]$importance[indxrvi]),2),
			edf = NA,
			F = NA)
		out2 <- rbind(out2, temp)
	}

	# smooth terms
	for(j in 1:nrow(summ$s.table)){
		var <- gsub('s\\(lat\\)\\:Realm', '', rownames(summ$s.table)[j])
		temp <- data.frame(
			model = names(modsout)[i],
			r2='',
			n='',
			variable = paste('Latitude:', var, sep=''),
			smooth = 1,
			estimate = NA,
			se = NA,
			t = NA,
			p = signif(summ$s.table[j, 'p-value'],2),
			RVI = round(ifelse(var=='Marine', no=NA, yes=modavgsout[[i]]$importance['s(lat, by = Realm)']),2),
			edf = round(summ$s.table[j, 'edf'],2),
			F = round(summ$s.table[j, 'F'],2))
		out2 <- rbind(out2, temp)
	}
}

# format p-values
orig <- options(scipen=1)
# rnd <- out2$p < 0.00001
out2$p <- as.character(out2$p)
# out2$p[rnd] <- '< 0.00001'

out2$p

options(scipen=orig)

# write out
out2
write.csv(out2, file='tables/TableS2_gamm_models.csv', row.names=FALSE, na='')
