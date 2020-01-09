#goal: read in the data file and add extreme max body temps in full sun and full shade to each row

#load packages

#do once to get NicheMapR
#install.packages("~/Downloads/NicheMapR_1.0.0.tar", repos = NULL,  type = .Platform$pkgType)
#get.global.climate(folder="/Users/Jennifer_Sunday/library/R/3.3/")#download climate data
#get.global.climate(folder="~/Library/R/NicheMapR/")#download climate data

library(data.table)
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
library('pryr') # for mem_used()
library('plyr') # for mapvalues()
source('scripts/micro_ghcnd.r') # for running with GHCN daily data. To run, this needs a NOAA key set, e.g., in .Rprofile: options(noaakey = "KEY_EMAILED_TO_YOU")

#########################################
## START HERE IF STARTING FROM SCRATCH
## (NO GHCND NICHEMAPPING DONE YET)
#########################################
##load data####
data<-read.csv(file='data/tmax_data/dataset_1_hotwater.csv', header=T)
load('temp/ghcnd_stations.rdata') # the ghcnd station data

# fix column names and factor levels
setnames(data, c('lat', 'lon', 'habitat', 'citation'), c('lat_max', 'long_max', 'Realm', 'REF_max'))
data$Realm <- mapvalues(data$Realm, from=c('marine', 'terrestrial'), to=c('Marine', 'Terrestrial'))

# trim out rows without Tmax	
sum(is.na(data$tmax)) # none

#make two new vectors with NA so that unfilled data are NA
#data$NM_2m_airshade<-NA #max shade air temperature (deg C) at reference height (specified by 'Refhyt', 1.2m default)
#data$NM_exposed_Te<-NA #max operative temperature (deg C) in full sun
#data$NM_exposed_Te_wet<-NA #max wet operative temperature (deg C) in full sun
#data$NM_2m_airshade95<-NA # 95th percentile	
#data$NM_exposed_Te95<-NA
#data$NM_exposed_Te_wet95<-NA
#data$NM_shade_Te_wet<-NA
#data$NM_shade_Te_wet95<-NA

data$NMGHCND_2m_airshade<-NA # using GHCN daily data
data$NMGHCND_exposed_Te<-NA
data$NMGHCND_exposed_Te_wet<-NA
data$NMGHCND_2m_airshade95<-NA
data$NMGHCND_exposed_Te95<-NA
data$NMGHCND_exposed_Te_wet95<-NA
data$NMGHCND_shade_Te_wet<-NA
data$NMGHCND_shade_Te_wet95<-NA

# tweak lat/lon so that NicheMapR has data (e.g., sites too close to the ocean or data entry error)
#i <- data$long_max==139.7 & data$lat_max==35.5; data$long_max[i] <- 139.7024; data$lat_max[i] <- 35.52982 # Kawasaki, Japan. Move slightly inland.
#i <- data$long_max== 146 & data$lat_max== -17; data$long_max[i] <- 145.8 # coast of Australia. Move on land.
#i <- data$long_max== 129.3 & data$lat_max== 26.5; data$lat_max[i] <- 28.3 # Amami-ohshima, Japan. Move slightly inland.
#i <- data$long_max== -79.5 & data$lat_max== 9; data$long_max[i] <- -79.53 # Panama City. Move slightly inland.
#i <- data$Genus == 'Cyclorana' & data$Species == 'brevipes' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -21 # Queensland (incorrectly entered as 21)
#i <- data$Genus == 'Hyla' & data$Species == 'lesueuri' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -37.4 # Murrindal, VIC, Australia. Move on land.
i <- data$Genus == 'Hyla' & data$Species == 'peroni' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -33.4; data$long_max[i] <- 151.3 # Gosford, NSW, Australia. Move on land.
#i <- data$Genus == 'Hyla' & data$Species == 'phyllochroa' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -37.4 # Murrindal, VIC, Australia. Move on land.
#i <- data$Genus == 'Limnodynastes' & data$Species == 'Salmini' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -33.4 # Gosford, NSW, Australia. Move on land.
#i <- data$Genus == 'Elgaria' & data$Species == 'multicarinata' & data$REF_max=='Cunningham_1966'; data$lat_max[i] <- 34.05 # Move on land.
i <- data$Genus == 'Suta' & data$Species == 'flagellum' & data$REF_max=='Spellerberg_1972'; data$long_max[i] <- 149.95 # Move on land.
#i <- data$lat_max == 21.97 & data$long_max == -159.72; data$long_max[i] <- -159.66 # Move slightly inland.
#i <- data$lat_max == 15.29 & data$long_max == -61.4; data$long_max[i] <- -61.33 # Move slightly inland.
#i <- data$lat_max == 30.72 & data$long_max == 122.45; data$long_max[i] <- 121 # Move on land.
#i <- data$lat_max == 22.05 & data$long_max == 121.53 & data$REF_max == 'Huang_et_al_2006'; data$lat_max[i] <- 22.576; data$long_max[i] <- 120.89 # Move on land. Samples were collected in both Orchid Island (no climate data) and in Taitung County, Taiwan. Choose the latter.
#i <- data$lat_max == 18.38 & data$long_max == -67.19; data$long_max[i] <- -67.16 # Move inland.
#i <- data$lat_max == 18.33 & data$long_max == -65.65; data$long_max[i] <- -65.67 # Move inland.
#i <- data$lat_max == 33.01 & data$long_max == -118.56; data$lat_max[i] <- 32.87; data$long_max[i] <- -118.46 # Move to center of San Clemente Island, CA


##############################################################
## START HERE IF STARTING FROM AN EXISTING NICHEMAPPED FILE
##############################################################
data <- read.csv("output/dataset_1_hotwater_Nichemapped_GHCND.csv") #read in partially-run dataset
load('temp/ghcnd_stations.rdata') # the ghcnd station data


######################
## RUN NICHEMAPPING
######################

#make a vector of rows for which NicheMapR has climate data, and that haven't already been run
landrows <- which(data$Realm == "Terrestrial")
nodatarows<-NA
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -45 & data$long_max > 35 & data$long_max < 40)) # remove Prince Edward Islands (sub-Antarctic)
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -70)) # remove Antarctic
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -52 & data$long_max > 70 & data$long_max < 75)) # remove Heard Island
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 18.3 & data$lat_max < 18.9 & data$long_max > -115 & data$long_max < -110)) # Revillagigedo Islands, Mexico
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 39 & data$lat_max < 40 & data$long_max > 0 & data$long_max < 1)) # Columbretes archipellago
NicheMapablerows <- setdiff(landrows, nodatarows)

NicheMapablerows <- setdiff(NicheMapablerows, which(!is.na(data$NMGHCND_2m_airshade))) # to remove rows already run
#NicheMapablerows <- setdiff(NicheMapablerows, which(!is.na(data$NM_shade_Te_wet))) # to remove rows already run
length(NicheMapablerows)
	sum(data$Realm=='Terrestrial')

# Create sets of 25 to run before writing out results
batches <- split(NicheMapablerows, ceiling(seq_along(NicheMapablerows)/25))

# How many batches to run?? To help solve the virtual memory issue with R. Quit after this and start again
nbatch <- min(6, length(batches))

# Run Micro_global and ectotherm models for each NicheMappable lat and long
# Write out after each batch
for(j in 1:nbatch){
	print(paste('batch ', j, ' of ', length(batches), ' (running first ', nbatch, ')', sep=''))
	print(mem_used()) # to try to diagnose memory problems
	
	for(i in batches[[j]]){
		print(paste('index', i, 'of', max(batches[[j]]), 'batch', j))
#		microglob <- micro_global(loc = c(data$long_max[i], data$lat_max[i]), timeinterval = 365, nyears = 1, soiltype = 4,
#			REFL = 0.15, slope = 0, aspect = 0,
#			DEP = c(0., 2.5,  5.,  10.,  15,  20,  30,  50,  100,  200), minshade = 0, maxshade = 90,
#			Usrhyt = 0.01, Refhyt=2)
#		micro <- microglob # ectotherm() expects micro to be present
#		data$NM_2m_airshade[i] <- max(micro$shadmet[,'TAREF']) # max shade air temperature (deg C) at reference height (specified by 'Refhyt')
#		data$NM_2m_airshade95[i] <- quantile(micro$shadmet[,'TAREF'], probs=c(0.95)) # 95 quantile 
#
#		ecto <- ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=0)
#		data$NM_exposed_Te[i] <- max(ecto$environ[,'TC']) # max operative temperature (deg C) in full sun
#		data$NM_exposed_Te95[i] <- quantile(ecto$environ[,'TC'], probs=c(0.95)) #95 quantile	
#
#		ectowet<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=100)
#		data$NM_exposed_Te_wet[i] <- max(ectowet$environ[,'TC'])  # max wet operative temperature full sun
#		data$NM_exposed_Te_wet95[i] <- quantile(ectowet$environ[,'TC'], probs=c(0.95))  #95 quantile
#
#		ectowetshade<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=1, crepus=0, nocturn=0, skinwet=100, minshade=90)
#		data$NM_shade_Te_wet[i] <- max(ectowetshade$environ[,'TC'])  # max wet operative temperature full sun
#		data$NM_shade_Te_wet95[i] <- quantile(ectowetshade$environ[,'TC'], probs=c(0.95))  #95 quantile
	
#		rm(micro, microglob, ecto, ectowet, ectowetshade)

		# run using GHCND data
		microghcnd <- NULL
		microghcnd <- micro_ghcnd(loc = c(data$long_max[i], data$lat_max[i]), timeinterval = 365, nyears = 20, soiltype = 4,
			REFL = 0.15, slope = 0, aspect = 0,
			DEP = c(0., 2.5,  5.,  10.,  15,  20,  30,  50,  100,  200), minshade = 0, maxshade = 90,
			Usrhyt = 0.01, Refhyt=2, 
			ghcnd_stations=ghcnd_stations, radius=1000, mincov=0.8, mindate='1986-01-01', maxdate='2005-12-31') # won't return anything if hits an error

		if(!is.null(microghcnd)){ 
			micro <- microghcnd # ectotherm() expects micro to be present
			data$NMGHCND_2m_airshade[i] <- max(microghcnd$shadmet[,'TAREF']) # max shade air temperature (deg C) at reference height (specified by 'Refhyt')
			data$NMGHCND_2m_airshade95[i] <- quantile(microghcnd$shadmet[,'TAREF'], probs=c(0.95)) # 95 quantile 

			ectoghcnd <- ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=0)
			data$NMGHCND_exposed_Te[i] <- max(ectoghcnd$environ[,'TC']) # max operative temperature (deg C) in full sun
			data$NMGHCND_exposed_Te95[i] <- quantile(ectoghcnd$environ[,'TC'], probs=c(0.95)) #95 quantile	

			ectowetghcnd<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=100)
			data$NMGHCND_exposed_Te_wet[i] <- max(ectowetghcnd$environ[,'TC'])  # max wet operative temperature full sun
			data$NMGHCND_exposed_Te_wet95[i] <- quantile(ectowetghcnd$environ[,'TC'], probs=c(0.95))  #95 quantile
		
			ectowetshadeghcnd<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=1, crepus=0, nocturn=0, skinwet=100, minshade=90)
			data$NMGHCND_shade_Te_wet[i] <- max(ectowetshadeghcnd$environ[,'TC'])  # max wet operative temperature full sun
			data$NMGHCND_shade_Te_wet95[i] <- quantile(ectowetshadeghcnd$environ[,'TC'], probs=c(0.95))  #95 quantile
			
			rm(micro, ectoghcnd, ectowetghcnd, ectowetshadeghcnd)
		}
		
		rm(microghcnd)

	}

	# write out
	write.csv(data, "output/dataset_1_hotwater_Nichemapped_GHCND.csv", row.names=FALSE) #write out data
}

# Now quit R to free up memory, then restart from the file that was just written


########################################
#compare to original max temp values
########################################
oldnichemapped<-read.csv("output/dataset_1_hotwater_Nichemapped.csv")#read in max temp values
oldnichemapped95<-read.csv("output/dataset_1_hotwater_Nichemapped_95quant.csv")#read in max temp values

	#plot max
	plot(NM_exposed_Te~lat_max, ylim=c(0, 100), data=data)
	points(NM_exposed_Te_wet~lat_max, col="blue", data=data)
	points(NM_2m_airshade~lat_max, col="green", data=data)

	points(NM_exposed_bodytemp~lat, pch=2, data=oldnichemapped)
	points(NM_exposed_body_wet~lat, col="blue", pch=2, data=oldnichemapped)

	#plot 95th percentile
	plot(NM_exposed_Te95~lat_max, ylim=c(0, 100), data=data)
	points(NM_exposed_Te_wet95~lat_max, col="green", data=data)

	points(NM_exposed_bodytemp~lat, pch=2, data=oldnichemapped95)
	points(NM_exposed_body_wet~lat, col="green", pch=2, data=oldnichemapped95)

# try merging on sci name and comparing apples to apples
comb <- merge(data, oldnichemapped95, by=c('Genus', 'Species'))
	nrow(comb)
		
	plot(NM_exposed_Te95~NM_exposed_bodytemp, data=comb)
	abline(0,1)
	
	plot(NM_exposed_Te_wet95~NM_exposed_body_wet, data=comb)
	abline(0,1)
	
	i <- comb$NM_exposed_Te95 < comb$NM_exposed_bodytemp - 15 & !is.na(comb$NM_exposed_Te95) & !is.na(comb$NM_exposed_bodytemp)
	comb[i,c('Genus', 'Species', 'lat_max', 'lat', 'long_max', 'lon', 'NM_exposed_Te95', 'NM_exposed_bodytemp')]
	
	
########################################
## compare NM global vs. NM with GHCND
########################################

with(data, plot(NM_2m_airshade, NMGHCND_2m_airshade)); abline(0,1); abline(5,1, col='purple'); abline(10,1, col='red')

with(data, plot(NM_2m_airshade95, NMGHCND_2m_airshade95)); abline(0,1); abline(1,1,col='purple')

with(data, plot(NM_2m_airshade95, NMGHCND_2m_airshade95)); abline(0,1); abline(1,1,col='purple')