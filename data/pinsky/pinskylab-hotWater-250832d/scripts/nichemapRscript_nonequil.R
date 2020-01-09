#goal: read in the data file and calculate operative temperatures and non-equilibrium body temperatures from previously-downloaded GHCND data in files (see ghcnd_download.r)

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
library('plyr') # for mapvalues()
source('scripts/micro_ghcnd_fromfiles.r') # for running with GHCN daily data that was previously downloaded

#########################################
## START HERE IF STARTING FROM SCRATCH
## (NO GHCND NICHEMAPPING DONE YET)
#########################################
##load data####
data <- fread(file='data/tmax_data/dataset_1_hotwater.csv', header=T)
traits <- fread('data/tmax_data/dataset_1_traits.csv')

# merge	traits
	dim(data)
data <- merge(data, traits[, .(Genus, Species, thermy, weight, weight_source, length, notes)], all.x=TRUE, by=c('Genus', 'Species'))
	dim(data)

	# any missing weights?
	data[habitat=='terrestrial',sum(is.na(weight))] # missing 47
	#data[habitat=='terrestrial' & is.na(weight), .(Genus, Species, weight, weight_source, notes)]


# fix column names and factor levels
setnames(data, c('lat', 'lon', 'habitat', 'citation'), c('lat_max', 'long_max', 'Realm', 'REF_max'))
data$Realm <- mapvalues(data$Realm, from=c('marine', 'terrestrial'), to=c('Marine', 'Terrestrial'))

# trim out rows without Tmax	
sum(is.na(data$tmax)) # none: good!

#make two new vectors with NA so that unfilled data are NA
data$NMGHCND_2m_airshade95 <- NA
data$NMGHCND_exposed_Te95 <- NA
data$NMGHCND_exposed_Te_wet95 <- NA
data$NMGHCND_shade_Te_wet95 <- NA

data$NMGHCND_2m_shade_Tb95 <- NA
data$NMGHCND_exposed_Tb95 <- NA
data$NMGHCND_exposed_Tb_wet95 <- NA
data$NMGHCND_shade_Tb_wet95 <- NA

# tweak lat/lon so that NicheMapR has data (e.g., sites too close to the ocean or data entry error)
i <- data$Genus == 'Hyla' & data$Species == 'peroni' & data$REF_max=='Brattstrom_1970'; data$lat_max[i] <- -33.4; data$long_max[i] <- 151.3 # Gosford, NSW, Australia. Move on land.
i <- data$Genus == 'Suta' & data$Species == 'flagellum' & data$REF_max=='Spellerberg_1972'; data$long_max[i] <- 149.95 # Move on land.


##############################################################
## START HERE IF STARTING FROM AN EXISTING NICHEMAPPED FILE
##############################################################
data <- fread("output/dataset_1_hotwater_Nichemapped_GHCNDnonequil.csv") #read in partially-run dataset


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
	length(NicheMapablerows)

NicheMapablerows <- setdiff(NicheMapablerows, which(!is.na(data$NMGHCND_2m_airshade95))) # to remove rows already run
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
	
	for(i in batches[[j]]){
		print(paste('index', i, 'of', max(batches[[j]]), 'batch', j))

		# run using GHCND data
		microghcnd <- NULL
		microghcnd <- micro_ghcnd_fromfiles(loc = c(data$long_max[i], data$lat_max[i]), timeinterval = 365, nyears = 20, soiltype = 4,
			REFL = 0.15, slope = 0, aspect = 0,
			DEP = c(0., 2.5,  5.,  10.,  15,  20,  30,  50,  100,  200), minshade = 0, maxshade = 90,
			Usrhyt = 0.01, Refhyt=2, 
			ghcnd_dir='data_dl/ghcnd/') # won't return anything if hits an error

		if(!is.null(microghcnd)){ 
			micro <- microghcnd # ectotherm() expects micro to be present
			data$NMGHCND_2m_airshade95[i] <- quantile(microghcnd$shadmet[,'TAREF'], probs=c(0.95)) # 95 quantile 

			ectoghcnd <- ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=0) # returns operative temperatures every hour
			data$NMGHCND_exposed_Te95[i] <- quantile(ectoghcnd$environ[,'TC'], probs=c(0.95)) #95 quantile	

			ectowetghcnd<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=0, crepus=0, nocturn=0, skinwet=100)
			data$NMGHCND_exposed_Te_wet95[i] <- quantile(ectowetghcnd$environ[,'TC'], probs=c(0.95))  #95 quantile
	
			ectowetshadeghcnd<-ectotherm(nyears = micro$nyears, ABSMAX=0.9, ABSMIN=0.9, amass=5, burrow=0, CkGrShad=1, crepus=0, nocturn=0, skinwet=100, minshade=90)
			data$NMGHCND_shade_Te_wet95[i] <- quantile(ectowetshadeghcnd$environ[,'TC'], probs=c(0.95))  #95 quantile
			
			# run non-equilibrium calculations for body temperature
			if(!is.na(data$weight[i])){
				# time-constants in minutes. From Grigg et al. 1979 J Therm Biol 4(1):95-103
				tau_up <- exp(0.72 + 0.36 * log(data$weight[i]))  # for warming on land
				tau_down <- exp(0.42 + 0.44 * log(data$weight[i])) # for cooling on land
				
				# intialize Tb with mean Te
				Tbshd <- rep(NA, length(microghcnd$shadmet[,'TAREF'])); Tbshd[1] <- mean(microghcnd$shadmet[,'TAREF']) # shade
				Tbexp <- rep(NA, length(ectoghcnd$environ[,'TC'])); Tbexp[1] <- mean(ectoghcnd$environ[,'TC']) # exposed
				Tbexpwet <- rep(NA, length(ectowetghcnd$environ[,'TC'])); Tbexpwet[1] <- mean(ectowetghcnd$environ[,'TC']) # exposed wet
				Tbshdwet <- rep(NA, length(ectowetshadeghcnd$environ[,'TC'])); Tbshdwet[1] <- mean(ectowetshadeghcnd$environ[,'TC']) # shade wet
				
				# calculate non-equilibrium Tb based on time-constant
				for(k in 2:length(microghcnd$shadmet[,'TAREF'])){
					if(microghcnd$shadmet[k,'TAREF'] >= Tbshd[k-1]){
						Tbshd[k] <- Tbshd[k-1] + (1-exp(-60/tau_up)) * (microghcnd$shadmet[k,'TAREF'] - Tbshd[k-1]) # t=60 since Te data are hourly
					} else {
						Tbshd[k] <- Tbshd[k-1] + (1-exp(-60/tau_down)) * (microghcnd$shadmet[k,'TAREF'] - Tbshd[k-1])
					}

					if(ectoghcnd$environ[k,'TC'] >= Tbexp[k-1]){
						Tbexp[k] <- Tbexp[k-1] + (1-exp(-60/tau_up)) * (ectoghcnd$environ[k,'TC'] - Tbexp[k-1])
					} else {
						Tbexp[k] <- Tbexp[k-1] + (1-exp(-60/tau_down)) * (ectoghcnd$environ[k,'TC'] - Tbexp[k-1])
					}

					if(ectowetghcnd$environ[k,'TC'] >= Tbexp[k-1]){
						Tbexpwet[k] <- Tbexpwet[k-1] + (1-exp(-60/tau_up)) * (ectowetghcnd$environ[k,'TC'] - Tbexpwet[k-1])
					} else {
						Tbexpwet[k] <- Tbexpwet[k-1] + (1-exp(-60/tau_down)) * (ectowetghcnd$environ[k,'TC'] - Tbexpwet[k-1])
					}

					if(ectowetshadeghcnd$environ[k,'TC'] >= Tbexp[k-1]){
						Tbshdwet[k] <- Tbshdwet[k-1] + (1-exp(-60/tau_up)) * (ectowetshadeghcnd$environ[k,'TC'] - Tbshdwet[k-1])
					} else {
						Tbshdwet[k] <- Tbshdwet[k-1] + (1-exp(-60/tau_down)) * (ectowetshadeghcnd$environ[k,'TC'] - Tbshdwet[k-1])
					}
				}

				# 95th quantile
				data$NMGHCND_2m_shade_Tb95[i] <- quantile(Tbshd, probs=c(0.95))
				data$NMGHCND_exposed_Tb95[i] <- quantile(Tbexp, probs=c(0.95))
				data$NMGHCND_exposed_Tb_wet95[i] <- quantile(Tbexpwet, probs=c(0.95))
				data$NMGHCND_shade_Tb_wet95[i] <- quantile(Tbshdwet, probs=c(0.95))
				
			}
			
			rm(micro, ectoghcnd, ectowetghcnd, ectowetshadeghcnd)
		}
		
		rm(microghcnd)

	}

	# write out
	write.csv(data, "output/dataset_1_hotwater_Nichemapped_GHCNDnonequil.csv", row.names=FALSE) #write out data
}

# Now quit R to free up memory, then restart from the file that was just written


###############################
# Examine missing data
###############################
# find rows that we could nichemap
landrows <- which(data$Realm == "Terrestrial")
nodatarows<-NA
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -45 & data$long_max > 35 & data$long_max < 40)) # remove Prince Edward Islands (sub-Antarctic)
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -70)) # remove Antarctic
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max < -52 & data$long_max > 70 & data$long_max < 75)) # remove Heard Island
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 18.3 & data$lat_max < 18.9 & data$long_max > -115 & data$long_max < -110)) # Revillagigedo Islands, Mexico
nodatarows <- c(nodatarows, which(data$Realm == "Terrestrial" & data$lat_max > 39 & data$lat_max < 40 & data$long_max > 0 & data$long_max < 1)) # Columbretes archipellago
NicheMapablerows <- setdiff(landrows, nodatarows)

# show rows not filled in
data[NicheMapablerows,][!is.na(weight) & is.na(NMGHCND_2m_shade_Tb95), .(Genus, Species, lat_max, long_max, NMGHCND_2m_shade_Tb95)]

################################
# compare Te and Tb
################################

# plot comparison of Te vs. Tb
data[,plot(NMGHCND_2m_airshade95, NMGHCND_2m_shade_Tb95)]
abline(0,1)

data[,plot(NMGHCND_exposed_Te95, NMGHCND_exposed_Tb95)]
abline(0,1)

data[,plot(NMGHCND_exposed_Te_wet95, NMGHCND_exposed_Tb_wet95)]
abline(0,1)

data[,plot(NMGHCND_shade_Te_wet95, NMGHCND_shade_Tb_wet95)]
abline(0,1)

# find outliers
data[abs(NMGHCND_2m_airshade95 - NMGHCND_2m_shade_Tb95)>0.15, .(Genus, Species, lat_max, long_max, weight, NMGHCND_2m_airshade95, NMGHCND_2m_shade_Tb95, NMGHCND_2m_airshade95 - NMGHCND_2m_shade_Tb95)]

data[abs(NMGHCND_exposed_Te95 - NMGHCND_exposed_Tb95)>0.5, .(Genus, Species, lat_max, long_max, weight, NMGHCND_exposed_Te95, NMGHCND_exposed_Tb95)]

data[abs(NMGHCND_exposed_Te_wet95 - NMGHCND_exposed_Tb_wet95)>0.5, .(Genus, Species, lat_max, long_max, weight, NMGHCND_exposed_Te_wet95, NMGHCND_exposed_Tb_wet95)]

data[abs(NMGHCND_shade_Te_wet95 - NMGHCND_shade_Tb_wet95)>0.3, .(Genus, Species, lat_max, long_max, weight, NMGHCND_shade_Te_wet95, NMGHCND_shade_Tb_wet95)]

# weight summaries and histogram
data[Realm=='Terrestrial', summary(weight)]
data[Realm=='Terrestrial', hist(log10(weight))]


# plot difference vs. weight
data[,plot(weight, NMGHCND_2m_airshade95 - NMGHCND_2m_shade_Tb95, log='xy', ylab='Difference', xlab='Body mass (g)', xlim=c(5e-1, 1e4), las=1)]
data[,plot(weight, NMGHCND_exposed_Te95 - NMGHCND_exposed_Tb95, log='x')]
data[,plot(weight, NMGHCND_exposed_Te_wet95 - NMGHCND_exposed_Tb_wet95, log='x')]
data[,plot(weight, NMGHCND_shade_Te_wet95 - NMGHCND_shade_Tb_wet95+0.001, log='xy')]

# summary of differences
data[,summary(NMGHCND_2m_airshade95 - NMGHCND_2m_shade_Tb95)] # 
data[,summary(NMGHCND_exposed_Te95 - NMGHCND_exposed_Tb95)]
data[,summary(NMGHCND_exposed_Te_wet95 - NMGHCND_exposed_Tb_wet95)]
data[,summary(NMGHCND_shade_Te_wet95 - NMGHCND_shade_Tb_wet95)]

data[,summary(lm(NMGHCND_2m_airshade95 ~ NMGHCND_2m_shade_Tb95))$r.squared]
data[,summary(lm(NMGHCND_exposed_Te95 ~ NMGHCND_exposed_Tb95))]
data[,summary(lm(NMGHCND_exposed_Te_wet95 ~ NMGHCND_exposed_Tb_wet95))]
data[,summary(lm(NMGHCND_shade_Te_wet95 ~ NMGHCND_shade_Tb_wet95))]


##########################################
# Transient dynamics plot
##########################################
ind1 <- data[, which(Realm=='Terrestrial' & !is.na(NMGHCND_2m_shade_Tb95) & weight == 10)] # 10g
maxwt <- data[Realm=='Terrestrial' & !is.na(NMGHCND_2m_shade_Tb95), max(weight)] # 10657

microghcnd1 <- micro_ghcnd_fromfiles(loc = c(data$long_max[ind1], data$lat_max[ind1]), timeinterval = 365, nyears = 20, soiltype = 4,
	REFL = 0.15, slope = 0, aspect = 0,
	DEP = c(0., 2.5,  5.,  10.,  15,  20,  30,  50,  100,  200), minshade = 0, maxshade = 90,
	Usrhyt = 0.01, Refhyt=2, 
	ghcnd_dir='data_dl/ghcnd/') # won't return anything if hits an error


	
# time-constants in minutes. From Grigg et al. 1979 J Therm Biol 4(1):95-103
tau_up1 <- exp(0.72 + 0.36 * log(10)) 
tau_down1 <- exp(0.42 + 0.44 * log(10))
tau_up2 <- exp(0.72 + 0.36 * log(maxwt))  
tau_down2 <- exp(0.42 + 0.44 * log(maxwt))

# intialize Tb with mean Te
Tbshd1 <- rep(NA, length(microghcnd1$shadmet[,'TAREF'])); Tbshd1[1] <- mean(microghcnd1$shadmet[,'TAREF']) # shade
Tbshd2 <- rep(NA, length(microghcnd1$shadmet[,'TAREF'])); Tbshd2[1] <- mean(microghcnd1$shadmet[,'TAREF']) # shade

# calculate non-equilibrium Tb based on time-constant
for(k in 2:length(microghcnd1$shadmet[,'TAREF'])){
	if(microghcnd1$shadmet[k,'TAREF'] >= Tbshd1[k-1]){
		Tbshd1[k] <- Tbshd1[k-1] + (1-exp(-60/tau_up1)) * (microghcnd1$shadmet[k,'TAREF'] - Tbshd1[k-1])
	} else {
		Tbshd1[k] <- Tbshd1[k-1] + (1-exp(-60/tau_down1)) * (microghcnd1$shadmet[k,'TAREF'] - Tbshd1[k-1])
	}

	if(microghcnd1$shadmet[k,'TAREF'] >= Tbshd2[k-1]){
		Tbshd2[k] <- Tbshd2[k-1] + (1-exp(-60/tau_up2)) * (microghcnd1$shadmet[k,'TAREF'] - Tbshd2[k-1])
	} else {
		Tbshd2[k] <- Tbshd2[k-1] + (1-exp(-60/tau_down2)) * (microghcnd1$shadmet[k,'TAREF'] - Tbshd2[k-1])
	}
}

# plot
quartz(width=3,height=3)
# pdf()
par(mai=c(0.5, 0.5, 0.1, 0.1), las=1, mgp=c(1.5,0.5,0), tcl=-0.3)
plot(microghcnd1$shadmet[175100:175200,'TAREF'], ylab='°C', xlab='Hour', type='l', lwd=4, col='grey')
lines(Tbshd1[175100:175200], col='black')
lines(Tbshd2[175100:175200], col='red')

dev.off()


####################################
# Nice difference vs. weight plot
####################################
data <- fread('output/dataset_1_hotwater_Nichemapped_GHCNDnonequil.csv')

library(sfsmisc)

quartz(width=4, height=3)
# png(width=4, height=3, file='figures/Te_vs_Tb.png', units='in', res=300)
par(mai=c(0.5, 0.8, 0.1, 0.15), las=1, mgp=c(1.5,0.5,0), tcl=-0.3)
data[,plot(weight, NMGHCND_2m_airshade95 - NMGHCND_2m_shade_Tb95, log='xy', ylab='', yaxt='n', xlab='Body mass (g)', xlim=c(5e-1, 1e4), las=1)]
eaxis(2, padj=-0.5, cex.axis=1, at=c(1e-14, 1e-10, 1e-6, 1e-2), f.smalltcl=0)
mtext('Te - Tb', side=2, line=3, las=0)

dev.off()