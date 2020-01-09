#for testing the nichemapRscript_95int.R loop
# packaged as a function so that it can be run with lineprof()

nichemapRscript_ghcntest <- function(st, ed){
	print(paste('from', st, 'to', ed))
	#load packages

	#do once to get NicheMapR
	#install.packages("~/Downloads/NicheMapR_1.0.0.tar", repos = NULL,  type = .Platform$pkgType)
	#get.global.climate(folder="/Users/Jennifer_Sunday/library/R/3.3/")#download climate data
	#get.global.climate(folder="~/Library/R/NicheMapR/")#download climate data

	library("NicheMapR")
	library("dismo")
	library("geonames")
	library("raster")
	library("XML")
	library("readr")
	library("R.utils")
	library("ncdf4")
	library("lattice")
	library('rgdal') # needed by micro_ghcnd and ghcn_get
	source('scripts/micro_ghcnd.r') # for running with GHCN daily data. To run, this needs a NOAA key set, e.g., in .Rprofile: options(noaakey = "KEY_EMAILED_TO_YOU")

	##load data####
	data<-read.csv(file='data/tmax_data/dataset_1_traits.csv', header=T)
	load('temp/ghcnd_stations.rdata') # the ghcnd station data

	# trim out rows without Tmax	
	data <- data[!is.na(data$Tmax),]
		nrow(data)

	#make two new vectors with NA so that unfilled data are NA
	data$NM_2m_airshade<-NA #max shade air temperature (deg C) at reference height (specified by 'Refhyt', 1.2m default)
	data$NM_exposed_Te<-NA #max operative temperature (deg C) in full sun
	data$NM_exposed_Te_wet<-NA #max wet operative temperature (deg C) in full sun
	data$NM_2m_airshade95<-NA # 95th percentile	
	data$NM_exposed_Te95<-NA
	data$NM_exposed_Te_wet95<-NA

	data$NMGHCND_2m_airshade<-NA # using GHCN daily data
	data$NMGHCND_exposed_Te<-NA
	data$NMGHCND_exposed_Te_wet<-NA
	data$NMGHCND_2m_airshade95<-NA
	data$NMGHCND_exposed_Te95<-NA
	data$NMGHCND_exposed_Te_wet95<-NA


	#make a vector of rows for which NicheMapR has climate data
	# UPDATE IF CLIMATE DATA CHANGES
	landrows <- which(data$Realm == "Terrestrial")
	nodatarows<-NA
	#no_NicheMapR_data %in% c("Heard", "Marion", "Antarctica"))
	nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -45 & data$long_max > 35 & data$long_max < 40)) # remove Prince Edward Islands (sub-Antarctic)
	nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -70)) # remove Antarctic
	nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -52 & data$long_max > 70 & data$long_max < 75)) # remove Heard Island
	nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 18.3 & data$lat_max < 18.9 & data$long_max > -115 & data$long_max < -110)) # Revillagigedo Islands, Mexico
	nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 39 & data$lat_max < 40 & data$long_max > 0 & data$long_max < 1)) # Columbretes archipellago
	NicheMapablerows <- setdiff(landrows, nodatarows)

	#NicheMapablerows <- setdiff(NicheMapablerows, 1:165) # to remove rows already run
	#NicheMapablerows <- setdiff(NicheMapablerows, which(!is.na(data$NMGHCND_2m_airshade))) # to remove rows already run

	# tweak lat/lon so that NicheMapR has data (e.g., sites too close to the ocean or data entry error)
	i <- data$long_max==139.7 & data$lat_max==35.5; data$long_max[i] <- 139.7024; data$lat_max[i] <- 35.52982 # Kawasaki, Japan. Move slightly inland.
	i <- data$long_max== 146 & data$lat_max== -17; data$long_max[i] <- 145.8 # coast of Australia. Move on land.
	i <- data$long_max== 129.3 & data$lat_max== 26.5; data$lat_max[i] <- 28.3 # Amami-ohshima, Japan. Move slightly inland.
	i <- data$long_max== -79.5 & data$lat_max== 9; data$long_max[i] <- -79.53 # Panama City. Move slightly inland.
	i <- data$Genus == 'Cyclorana' & data$Species == 'brevipes' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -21 # Queensland (incorrectly entered as 21)
	i <- data$Genus == 'Hyla' & data$Species == 'lesueuri' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -37.4 # Murrindal, VIC, Australia. Move on land.
	i <- data$Genus == 'Hyla' & data$Species == 'peroni' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -33.4 # Gosford, NSW, Australia. Move on land.
	i <- data$Genus == 'Hyla' & data$Species == 'phyllochroa' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -37.4 # Murrindal, VIC, Australia. Move on land.
	i <- data$Genus == 'Limnodynastes' & data$Species == 'Salmini' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -33.4 # Gosford, NSW, Australia. Move on land.
	i <- data$Genus == 'Elgaria' & data$Species == 'multicarinata' & data$REF_max=='Cunningham_1966'; data$lat_max[i] <- 34.05 # Move on land.
	i <- data$Genus == 'Suta' & data$Species == 'flagellum' & data$REF_max=='Spellerberg_1972a'; data$long_max[i] <- 149.95 # Move on land.
	i <- data$lat_max == 21.97 & data$long_max == -159.72; data$long_max[i] <- -159.66 # Move slightly inland.
	i <- data$lat_max == 15.29 & data$long_max == -61.4; data$long_max[i] <- -61.33 # Move slightly inland.
	i <- data$lat_max == 30.72 & data$long_max == 122.45; data$long_max[i] <- 121 # Move on land.
	i <- data$lat_max == 22.05 & data$long_max == 121.53 & data$REF_max == 'Huang_et_al_2006'; data$lat_max[i] <- 22.576; data$long_max[i] <- 120.89 # Move on land. Samples were collected in both Orchid Island (no climate data) and in Taitung County, Taiwan. Choose the latter.
	i <- data$lat_max == 18.38 & data$long_max == -67.19; data$long_max[i] <- -67.16 # Move inland.
	i <- data$lat_max == 18.33 & data$long_max == -65.65; data$long_max[i] <- -65.67 # Move inland.
	i <- data$lat_max == 33.01 & data$long_max == -118.56; data$lat_max[i] <- 32.87; data$long_max[i] <- -118.46 # Move to center of San Clemente Island, CA



	#Run Micro_global and ectotherm models for each NicheMappable lat and long
	for(i in NicheMapablerows[st:ed]){
		print(paste(i, 'of', max(NicheMapablerows)))
		microglob <- micro_global(loc = c(data$long_max[i], data$lat_max[i]), timeinterval = 365, nyears = 1, soiltype = 4,
			REFL = 0.15, slope = 0, aspect = 0,
			DEP = c(0., 2.5,  5.,  10.,  15,  20,  30,  50,  100,  200), minshade = 0, maxshade = 90,
			Usrhyt = 0.01, Refhyt=2)
		micro <- microglob # ectotherm() expects micro to be present
#		shadmet <- as.data.frame(micro$shadmet) #make a dataframe of the above-ground micrometeorological conditions under the maximum specified shade	
#		data$NM_2m_airshade[i] <- max(shadmet$TAREF) # max shade air temperature (deg C) at reference height (specified by 'Refhyt', 1.2m default)
#		data$NM_2m_airshade95[i] <- quantile(shadmet$TAREF, probs=c(0.95)) # 95 quantile 

		message('starting ecto')
#		ecto <- ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=0)
#		environ <- as.data.frame(ecto$environ)
#		data$NM_exposed_Te[i] <- max(environ$TC) # max operative temperature (deg C) in full sun
#		data$NM_exposed_Te95[i] <- quantile(environ$TC, probs=c(0.95)) #95 quantile	

		message('starting ectowet')
#		ectowet<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=100)
#		environwet<-as.data.frame(ectowet$environ)
#		data$NM_exposed_Te_wet[i] <- max(environwet$TC)  # max wet operative temperature full sun
#		data$NM_exposed_Te_wet95[i] <- quantile(environwet$TC, probs=c(0.95))  #95 quantile

		# NEED shade wet
	

		# run using GHCND data
		message('starting GHCND')
		microghcnd <- NULL
		microghcnd <- micro_ghcnd(loc = c(data$long_max[i], data$lat_max[i]), timeinterval = 365, nyears = 20, soiltype = 4,
			REFL = 0.15, slope = 0, aspect = 0,
			DEP = c(0., 2.5,  5.,  10.,  15,  20,  30,  50,  100,  200), minshade = 0, maxshade = 90,
			Usrhyt = 0.01, Refhyt=2, 
			ghcnd_stations=ghcnd_stations, radius=1000, mincov=0.8, mindate='1986-01-01', maxdate='2005-12-31') # won't return anything if hits an error

		if(!is.null(microghcnd)){ 
			micro <- microghcnd # ectotherm() expects micro to be present
			shadmetghcnd <- as.data.frame(microghcnd$shadmet) #make a dataframe of the above-ground micrometeorological conditions under the maximum specified shade	
#			data$NMGHCND_2m_airshade[i] <- max(shadmetghcnd$TAREF) # max shade air temperature (deg C) at reference height (specified by 'Refhyt', 1.2m default)
#			data$NMGHCND_2m_airshade95[i] <- quantile(shadmetghcnd$TAREF, probs=c(0.95)) # 95 quantile 

#			ectoghcnd <- ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=0)
#			environghcnd <- as.data.frame(ectoghcnd$environ)
#			data$NMGHCND_exposed_Te[i] <- max(environghcnd$TC) # max operative temperature (deg C) in full sun
#			data$NMGHCND_exposed_Te95[i] <- quantile(environghcnd$TC, probs=c(0.95)) #95 quantile	

#			ectowetghcnd<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=100)
#			environwetghcnd<-as.data.frame(ectowetghcnd$environ)
#			data$NMGHCND_exposed_Te_wet[i] <- max(environwetghcnd$TC)  # max wet operative temperature full sun
#			data$NMGHCND_exposed_Te_wet95[i] <- quantile(environwetghcnd$TC, probs=c(0.95))  #95 quantile
		# NEED shade wet
		} else {
			message(paste('failed to get microghcnd back'))
		}

	}

}