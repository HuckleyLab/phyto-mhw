# Find body sizes

require(data.table)
require(rfishbase)

#############
# Read in dataset
#############
dat <- fread('temp/warmingtolerance_byspecies.csv')

# trim to marine fishes and arthropods
dat <- subset(dat, habitat_intertidal=='marine' & dat$Class %in% c('Actinopterygii', 'Elasmobranchii', 'Malacostraca', 'Pycnogonida'))


#####################################
# Get fishbase and sealifebase names and body sizes
# and movement type
#####################################

fbdat <- dat[!duplicated(paste(Genus, Species)), .(Genus, Species, Family, Class)] # translation table from my scientific names to those in fishbase or sealifebase
fbdat[, c("db", "fbsci", "weight") :=NA] 
fbdat[, c("length", "lengthType", "a", "b") :=NA] 
fbdat[, c("notes") :=NA] 
fbdat[,spp := paste(Genus, Species)]

options(nwarnings=300)
nrow(fbdat) # 72
for(i in 1:nrow(fbdat)){ # check sci names
	cat(paste(i, " ", sep=''))
	if(fbdat$Class[i] %in% c('Malacostraca', 'Pycnogonida')){
		fbdat$db[i] <- 'sealifebase'
		server <- "https://fishbase.ropensci.org/sealifebase"
	}
	if(fbdat$Class[i] %in% c('Actinopterygii', 'Elasmobranchii')){
		fbdat$db[i] <- 'fishbase'
		server <- "https://fishbase.ropensci.org/"
	}

	if(!(fbdat$spp[i] %in% c('Sebastes marinus', 'Pagrus auratus', 'Chaetodon lunulatus', 'Trachinotus falcatus', 'Bathygobius sp1', 'Bathygobius sp2', 'Clupea herengus', 'Myoxocephalus quadricornis_hexacornis', 'Pseudalibrotus litoralis', 'Hippolyte obliquimanus', 'Liocarcinus marmoreus', 'Eudorella splendida', 'Heterophoxus videns', 'Nymphon sp.', 'Orchomene pinguides', 'Paramoera walkeri', 'Philomedidae sp.', 'Paraleptognathia antarctica', 'Metacarcinus magister', 'Saduria entomon', 'Crangon crangon', 'Palaemon longirostris'))){ # run basic code, except for species where it doesn't work.
		fbdat$fbsci[i] <- validate_names(as.character(fbdat$spp[i]), server=server) # check that sci names are correct. returned warnings, all about potential synonyms.
		fbdat$weight[i] <- species(fbdat$fbsci[i], fields='Weight', server=server)$Weight
	}
	if(fbdat$spp[i] == 'Sebastes marinus'){
		fbdat$fbsci[i] <- 'Sebastes norvegicus' # what FB calls it (from a web search)
		fbdat$weight[i] <- species(fbdat$fbsci[i], fields='Weight', server=server)$Weight
	}
	if(fbdat$spp[i] == 'Pagrus auratus'){
		fbdat$fbsci[i] <- 'Pagrus auratus' # correct since our samples are from New Zealand
		fbdat$weight[i] <- species(fbdat$fbsci[i], fields='Weight', server=server)$Weight
	}
	if(fbdat$spp[i] == 'Chaetodon lunulatus'){
		fbdat$fbsci[i] <- 'Chaetodon lunulatus' # assume this isn't C. lunula (other option FB gives)
		fbdat$weight[i] <- species(fbdat$fbsci[i], fields='Weight', server=server)$Weight
	}
	if(fbdat$spp[i] == 'Trachinotus falcatus'){
		fbdat$fbsci[i] <- 'Trachinotus falcatus' # correct since our samples are from Puerto Rico (not Indo-pacific)
		fbdat$weight[i] <- species(fbdat$fbsci[i], fields='Weight', server=server)$Weight
	}
	if(fbdat$spp[i] == 'Clupea herengus'){
		fbdat$fbsci[i] <- 'Clupea harengus' # 
		fbdat$weight[i] <- species(fbdat$fbsci[i], fields='Weight', server=server)$Weight
	}
	if(fbdat$spp[i] == 'Myoxocephalus quadricornis_hexacornis'){
		fbdat$fbsci[i] <- 'Myoxocephalus quadricornis' # hexacornis doesn't work
		fbdat$weight[i] <- species(fbdat$fbsci[i], fields='Weight', server=server)$Weight
	}
	if(fbdat$spp[i] == 'Pseudalibrotus litoralis'){
		fbdat$fbsci[i] <- 'Onisimus litoralis '
		fbdat$weight[i] <- 1 # only up to 17 mm. See Boudrias & Carey 1988 Marine Ecology
	}
	if(fbdat$spp[i] == 'Hippolyte obliquimanus'){
		fbdat$fbsci[i] <- 'Hippolyte obliquimanus' # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 1 # only up to 3.4 mm carapace length. See Terossi et al. 2008 Marine Biology
	}
	if(fbdat$spp[i] == 'Liocarcinus marmoreus'){
		fbdat$fbsci[i] <- 'Liocarcinus marmoreus' # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 5 # only up to 35 mm carapace length. See https://en.wikipedia.org/wiki/Liocarcinus_marmoreus and http://www.marlin.ac.uk/species/detail/1173
	}
	if(fbdat$spp[i] == 'Eudorella splendida'){
		fbdat$fbsci[i] <- 'Eudorella splendida' # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 1 # only up to 8.5 mm body length. See Blazewicz-Paszkowycz 2001 Polish Polar Research
	}
	if(fbdat$spp[i] == 'Heterophoxus videns'){
		fbdat$fbsci[i] <- 'Heterophoxus videns' # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 1 # only up to 10 mm body length. See Dauby et al. 2001 Hydrobiologia. Or Bates et al. 2010 Nat Comm supplemental table.
	}
	if(fbdat$spp[i] == 'Nymphon sp.'){
		fbdat$fbsci[i] <- NA # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 1 # only 12.5 mm body length. See Bates et al. 2010 Nat Comm supplemental table.
	}
	if(fbdat$spp[i] == 'Orchomene pinguides'){
		fbdat$fbsci[i] <- 'Orchomene pinguides' # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 1 # only 7.5 mm body length. See Bates et al. 2010 Nat Comm supplemental table.
	}
	if(fbdat$spp[i] %in% c('Bathygobius sp1', 'Bathygobius sp2')){
		# spps <- apply(as.data.frame(fishbase[fishbase$SubFamily=='Gobiinae' & !is.na(fishbase$SubFamily), c('Genus', 'Species')]), MARGIN=1, FUN=paste, collapse=' ') # all in the Gobiinae subfamily, since no weights for Bathygobius genus
		fbdat$fbsci[i] <- NA
		fbdat$weight[i] <- 1 # species are very small
	}
	if(fbdat$spp[i] == 'Paramoera walkeri'){
		fbdat$fbsci[i] <- 'Paramoera walkeri' # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 1 # only 6 mm body length. See Bates et al. 2010 Nat Comm supplemental table.
	}
	if(fbdat$spp[i] == 'Philomedidae sp.'){
		fbdat$fbsci[i] <- NA # sealifebase spp name search turns up nothing, oddly
		fbdat$weight[i] <- 1 # only 1 mm body length. See Bates et al. 2010 Nat Comm supplemental table.
	}
	if(fbdat$spp[i] == 'Paraleptognathia antarctica'){
		fbdat$fbsci[i] <- NA # couldn't find in sealifebase
		fbdat$weight[i] <- 1 # only 4 mm body length. See Bates et al. 2010 Nat Comm supplemental table.
	}
	if(fbdat$spp[i] == 'Metacarcinus magister'){
		fbdat$fbsci[i] <- 'Metacarcinus magister' # validate_names returns Poropuntius laoensis incorrectly
		fbdat$weight[i] <- 10 # up to 22.5 cm body length. See Bates et al. 2010 Nat Comm supplemental table.
	}
	if(fbdat$spp[i] == 'Saduria entomon'){
		fbdat$fbsci[i] <- 'Saduria entomon' # validate_names returned Ketengus typus incorrectly
		fbdat$weight[i] <- 1 # an isopod. probably small.
	}
	if(fbdat$spp[i] == 'Crangon crangon'){
		fbdat$fbsci[i] <- 'Crangon crangon' # validate_names returned something else incorrectly
		fbdat$weight[i] <- 5 # to 8.9cm length
	}
	if(fbdat$spp[i] == 'Palaemon longirostris'){
		fbdat$fbsci[i] <- 'Palaemon longirostris' # validate_names returned something else incorrectly
		fbdat$weight[i] <- 5 # to 7cm length
	}
}
	warnings() # All are "... can also be misapplied to other species"
	# for(i in 1:nrow(fbdat)) print(synonyms(fbdat$fbsci[i])) # there is only ever 1 accepted name

	inds <- which(fbdat$spp != fbdat$fbsci)
	fbdat[inds,] # examine rows where fishbase has a different name. all look like the same species, but different names, so ok.

	# where weight missing
	inds <- which(is.na(fbdat$weight))
	fbdat[inds,] # examine rows
	
# fill in a weight by hand
fbdat[spp=='Mugil cephalus', weight:=12000] # from fishbase text, which says 12kg likely too high
	
# try to fill in missing weights
inds <- which(is.na(fbdat$weight))
length(inds); max(inds)
for(i in inds){ # check sci names
	cat(paste(i, " ", sep=''))
	fbdat$length[i] <- species(fbdat$fbsci[i], 'Length')$Length
	fbdat$lengthType[i] <- species(fbdat$fbsci[i], 'LTypeMaxM')$LTypeMaxM
	if(!is.na(fbdat$length[i])){
		lw <- length_weight(fbdat$fbsci[i])
		if(nrow(lw)>0){
			ltypes <- table(lw$Type)
			if(sum(lw$Type==fbdat$lengthType[i], na.rm=TRUE)>0){ # if length measurements of the right type are available
				fbdat$a[i] <- mean(lw$a[!is.na(lw$Type) & lw$Type==fbdat$lengthType[i]])
				fbdat$b[i] <- mean(lw$b[!is.na(lw$Type) & lw$Type==fbdat$lengthType[i]])
				fbdat[i, weight := a * length ^ b]
			} else { # try to convert to the right length measurement type
				ll <- length_length(fbdat$fbsci[i])
				if(sum(ll$Length2==fbdat$lengthType[i] & ll$Length1 %in% names(ltypes))>0){ # if ll table has the right conversation
					ll <- ll[ll$Length2==fbdat$lengthType[i],] # trim to the right initial length type
					ll <- ll[ll$Length1 %in% names(ltypes),] # trim to the right final length type (has to be in lw table)
					ltypes <- ltypes[names(ltypes) %in% unique(ll$Length1)] # trim ltypes from lw table to those in ll table
					comltype <- names(ltypes)[which.max(ltypes)] # most common length type in the remaining lw table
					ll <- ll[ll$Length1 == comltype,] # trim ll to the common final length type
				
					length2 <- mean(ll$a) + mean(ll$b)*fbdat$length[i]

					fbdat$a[i] <- mean(lw$a[!is.na(lw$Type) & lw$Type==comltype])
					fbdat$b[i] <- mean(lw$b[!is.na(lw$Type) & lw$Type==comltype])
					fbdat[i, weight := a * length2 ^ b]
				}
				if(sum(ll$Length1==fbdat$lengthType[i] & ll$Length2 %in% names(ltypes))>0){ # if ll table has the backwards conversion
					ll <- ll[ll$Length1==fbdat$lengthType[i],] # trim to the right initial length type
					ll <- ll[ll$Length2 %in% names(ltypes),] # trim to the right final length type (has to be in lw table)
					ltypes <- ltypes[names(ltypes) %in% unique(ll$Length2)] # trim ltypes from lw table to those in ll table
					comltype <- names(ltypes)[which.max(ltypes)] # most common final length type in the remaining lw table
					ll <- ll[ll$Length2 == comltype,] # trim ll to the common final length type


					length2 <- (fbdat$length[i] - mean(ll$a))/mean(ll$b)
					type2 <- ll$Length2

					fbdat$a[i] <- mean(lw$a[!is.na(lw$Type) & lw$Type==comltype])
					fbdat$b[i] <- mean(lw$b[!is.na(lw$Type) & lw$Type==comltype])
					fbdat[i, weight := a * length2 ^ b]
				}
			
			}
		} else {
			fbdat$notes[i] <- 'no length-weight table'
		}
	}
}
fbdat[inds,]

	# where weight still missing
	inds <- which(is.na(fbdat$weight))
	fbdat[inds,] # examine rows

# Fill in reamaining missing with average length-weight measurements
ave_a <- fbdat[,mean(a, na.rm=TRUE)]
ave_b <- fbdat[,mean(b, na.rm=TRUE)]
fbdat[is.na(weight), notes:= 'weight from length and ave a and b values'] # ignores TL vs. SL differences
fbdat[is.na(weight), weight:= ave_a*length ^ ave_b] # ignores TL vs. SL differences

	# check carefully those close but < 1kg
	fbdat[weight>750 & weight<1000,] # examine rows

	# other scarus
	spps <- apply(as.data.frame(fishbase[fishbase$Genus=='Scarus' & !is.na(fishbase$Genus), c('Genus', 'Species')]), MARGIN=1, FUN=paste, collapse=' ')
	scarus <- species(spps, fields=c('Length', 'Weight'))
	scarus[!is.na(scarus$Weight),]
		plot(scarus$Length, scarus$Weight, log='xy'); abline(h=1000); abline(v=35) # <1kg looks reasonable for 35cm Scarus iseri

	# other Nototheniidae
	spps <- apply(as.data.frame(fishbase[fishbase$Family=='Nototheniidae', c('Genus', 'Species')]), MARGIN=1, FUN=paste, collapse=' ')
	noto <- species(spps, fields=c('Length', 'Weight'))
	noto[!is.na(noto$Weight),]
		plot(log10(noto$Length), log10(noto$Weight)); abline(h=log10(1000)); abline(v=log10(41)); abline(lm(log10(Weight) ~ log10(Length), data=noto)) # <1kg looks reasonable for 41 cm Trematomus hansoni

	# other Sparidae
	spps <- apply(as.data.frame(fishbase[fishbase$Family=='Sparidae', c('Genus', 'Species')]), MARGIN=1, FUN=paste, collapse=' ')
	diplod <- species(spps, fields=c('Length', 'Weight'))
	diplod[!is.na(diplod$Weight),]
		plot(log10(diplod$Length), log10(diplod$Weight)); abline(h=log10(1000)); abline(v=log10(30)); abline(lm(log10(Weight) ~ log10(Length), data=diplod)) # <1kg looks reasonable for 30 cm Diplodus bellottii

	# other Tetraodontidae
	fishbase[fishbase$Genus=='Sphoeroides',]
	spps <- apply(as.data.frame(fishbase[fishbase$Family=='Tetraodontidae', c('Genus', 'Species')]), MARGIN=1, FUN=paste, collapse=' ')
	sphoer <- species(spps, fields=c('Length', 'Weight'))
	sphoer[!is.na(sphoer$Weight),]
		plot(log10(sphoer$Length), log10(sphoer$Weight)); abline(h=log10(1000)); abline(v=log10(36)); abline(lm(log10(Weight) ~ log10(Length), data=sphoer)) # <1kg looks reasonable for 36 cm Sphoeroides maculatus

	# other Zoarcidae
	spps <- apply(as.data.frame(fishbase[fishbase$Family=='Zoarcidae', c('Genus', 'Species')]), MARGIN=1, FUN=paste, collapse=' ')
	pachy <- species(spps, fields=c('Length', 'Weight'))
	pachy[!is.na(pachy$Weight),]
		plot(log10(pachy$Length), log10(pachy$Weight)); abline(h=log10(1000)); abline(v=log10(35)); abline(lm(log10(Weight) ~ log10(Length), data=pachy)) # <1kg looks reasonable for 35 cm Pachycara brachycephalum

		
# Add movement type
fbdat[, AnaCat := NA]
nrow(fbdat)
for(i in 71:nrow(fbdat)){
	cat(paste(i, " ", sep=''))
	if(fbdat$Class[i] %in% c('Actinopterygii', 'Elasmobranchii')){
		if(!(fbdat$spp[i] %in% c('Bathygobius sp1', 'Bathygobius sp2'))){
			fbdat$AnaCat[i] <- species(fbdat$fbsci[i], 'AnaCat')$AnaCat
		}
		if(fbdat$spp[i] %in% c('Bathygobius sp1', 'Bathygobius sp2')){
#			spps <- apply(fishbase[fishbase$Genus=='Bathygobius', c('Genus', 'Species')], 1, paste, collapse=' ')
#			anacats <- species(spps, 'AnaCat')$AnaCat
			fbdat$AnaCat[i] <- 'non-migratory' # both seem non-migratory from paper descriptions
		}
	}
}
setkey(fbdat, Class, Family, Genus, Species)
fbdat[, .(Class, Family, fbsci, spp, AnaCat)]
	# boxplot(log10(weight) ~ AnaCat, fbdat)
	# summary(lm(log10(weight) ~ AnaCat, fbdat))
	# table(fbdat$AnaCat[fbdat$weight<100])
	# table(fbdat$AnaCat[fbdat$weight>1000])
	
# examine
hist(fbdat$weight)
hist(log10(fbdat$weight))

# write out
write.csv(fbdat, file='temp/fbdat.csv', row.names=FALSE)



#################################################
# Compare to weights of all fishes in Fishbase
#################################################
######### rfishbase approach
# get a list of all species
tax <- load_taxa()
classes <- sort(unique(tax$Class))
spps <- character()
for(i in 1:length(classes)){
	spps <- c(spps, species_list(Class = classes[i]))
}
	length(spps) # 33104
	
# get weight data
wghts <- species(spps, fields = c('sciname', 'Genus', 'Species', 'Subfamily', 'Weight', 'Saltwater')) # slow. 1 hour?

nrow(wghts) # 33104
sum(!is.na(wghts$Weight)) # 2117. not many!

i <- wghts$Saltwater == -1 # only saltwater species
sum(i)
sum(!is.na(wghts$Weight[i])) # 1184. not many!
sum(wghts$Weight[i]>=1000, na.rm=TRUE)/sum(!is.na(wghts$Weight[i])) # 79%

write.csv(wghts, 'temp/fishbase_weights.csv')


######### FishLife approach (Thorson predictions for all species)
library(FishLife)

# visualize
hist(log10(exp(FishLife::database$Y_ij$Winfinity)), breaks=seq(-2,8,length.out=18), main='Observations') # observations
hist(log10(exp(FishLife::database$ParHat$beta_gj[,3])), breaks=seq(-2,8,length.out=18), main='Predictions') # predictions

# get predictions for all species
wghts2 <- data.frame(spp=spps, weight=NA) # 33104 species
for(i in 1:length(spps)){ # slow. 20 min?
	if(i %% 200 == 0) cat(i)
	splt <- unlist(strsplit(as.character(wghts2$spp[i]), split=' '))
	inds <- NA

	result <- tryCatch({

		suppressMessages(inds <- Search_species(Genus=splt[1], Species=splt[2], add_ancestors=FALSE)$GroupNum)

	}, warning = function(warn) {

		message(paste('Warning for', wghts2$spp[i], ':', warn))

	}, error = function(err) {

		message(paste('Error or otherwise could not find', wghts2$spp[i], ':', err))
	
	}, finally = {

	})

	if(!is.na(inds)){
		wghts2$weight[i] <- exp(database$ParHat$beta_gj[inds,3]) # Weight (originally log-transformed)
	}
	
}

# examine
nrow(wghts2) # 33104
sum(!is.na(wghts2$weight)) # 32950

sum(wghts2$weight>=1000, na.rm=TRUE)/sum(!is.na(wghts2$weight)) # 25%




write.csv(wghts2, file='temp/fishlife_weights.csv')

# trim to saltwater
wghts <- read.csv('temp/fishbase_weights.csv', row.names=1)
wghts3 <- merge(wghts, wghts2, by.x='sciname', by.y='spp')
	dim(wghts3) # 33107: missing a few
	with(wghts3, plot(Weight, weight, xlab='Observed', ylab='Predicted', log='xy'))

i <- wghts3$Saltwater == -1 # only saltwater species
sum(i) # 17222
sum(!is.na(wghts3$weight[i])) # 17149
sum(wghts3$weight[i]>=1000, na.rm=TRUE)/sum(!is.na(wghts3$weight[i])) # 26%


# plot
bks <- seq(-2,9,by=0.5)
par(mfrow=c(2,1))
hist(log10(wghts3$Weight), breaks=bks, col='grey', main='FishBase') # Fishbase
	abline(v=3, lty=2, col='red')
hist(log10(wghts3$weight), breaks=bks, col='grey', main='FishLife') # FishLife
	abline(v=3, lty=2, col='red')


#############
# Examine home range size
#############
hr <- fread('data_dl/mccauley/McCauley_et_al_2015_animal_mobility.csv') # hr in km2, bm in g

hr[Group=='F', summary(BM)]
hr[Group=='F', summary(HR)]

hr[Group=='F', plot(log10(BM/1000), log10(HR), xlab='log10(kg)', ylab='log10(km2)', pch=16)]

hr[System=='M', plot(log10(BM/1000), log10(HR), xlab='log10(kg)', ylab='log10(km2)')]
hr[Group=='F', points(log10(BM/1000), log10(HR), pch=16, col='red')]

mod <- hr[,lm(log10(HR) ~ log10(BM))]
summary(mod)
hr[Group=='F', plot(log10(BM), log10(HR), xlab='log10(g)', ylab='log10(km2)', pch=16)]
abline(a=coef(mod)[1], b=coef(mod)[2])

# add to fbdat
fbdat[, hr_km2 := 10^(coef(mod)[1] + coef(mod)[2] * log10(weight))]

fbdat[,summary(hr_km2)]
fbdat[,hist(log10(hr_km2))]


# write out
write.csv(fbdat, file='temp/fbdat.csv', row.names=FALSE)
