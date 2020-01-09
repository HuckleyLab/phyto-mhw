# Find body sizes

require(data.table)
require(rfishbase)

#############
# Read in dataset
#############
dat <- fread('data/tmax_data/dataset_1_hotwater.csv')


#####################################
# Get fishbase and sealifebase names and body sizes
# and movement type
#####################################

# initialize data.table with new columns
fbdat <- copy(dat)
a <- rep(as.character(NA), nrow(fbdat))
fbdat[, ':='(fishbase_sci=a, lengthType=a, weight_source=a, length_source=a, demers_pelag=a, demers_pelag_fb=a, demers_pelag_source=a)] 
a <- rep(as.numeric(NA), nrow(fbdat))
fbdat[, ':='(weight=a, length=a, length_a=a, length_b=a)]


# get fishbase data
options(nwarnings=300)
sum(fbdat$Realm=='Marine') # 
for(i in which(fbdat$Realm=='Marine')){ # check sci names. # don't try for terrestrial
	cat(paste(i, " ", sep=''))
	if(fbdat$Phylum[i] %in% c('Arthropoda', 'Brachiopoda', 'Echinodermata', 'Mollusca')){
		db <- 'sealifebase'
		server <- "https://fishbase.ropensci.org/sealifebase"
	}
	if(fbdat$Phylum[i] %in% c('Chordata')){
		db <- 'fishbase'
		server <- "https://fishbase.ropensci.org/"
	}

	tempname <- validate_names(as.character(fbdat[i,paste(Genus, Species)]), server=server) # check that sci names are correct. returned warnings, all about potential synonyms.	
	if(length(tempname)==0) tempname <- as.character(fbdat[i,paste(Genus, Species)]) # try name from table if validate failed

	if(length(tempname)>0){ # make sure we now have a name
		thisdat <- species(tempname, fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server=server)
		
		if(nrow(thisdat)>0){ # if data were returned, assume the name was in Fishbase or Sealifebase
			fbdat[i, fishbase_sci := tempname]
		}

		if('Weight' %in% names(thisdat)){ # make sure Weight was returned
			if(!is.na(thisdat$Weight)){
				fbdat[i, weight := thisdat$Weight]
				fbdat[i, weight_source := db]
			}
		}

		if('DemersPelag' %in% names(thisdat)){ # make sure Weight was returned
			if(!is.na(thisdat$DemersPelag)){
				fbdat[i, demers_pelag_fb := thisdat$DemersPelag]
				fbdat[i, demers_pelag_source := db]
			}
		}
		
		if('Length' %in% names(thisdat)){ # make sure Length was returned
			if(!is.na(thisdat$Length)){
				# we want total length (TL)
				if(thisdat$LTypeMaxM=='TL'){
					fbdat[i, length := thisdat$Length]
					fbdat[i, lengthType := thisdat$LTypeMaxM]
					fbdat[i, length_source := db]

				# try to convert it if not in TL
				} else {
					ll <- length_length(fbdat$fishbase_sci[i])
				
					if(nrow(ll)>0){ # if L-L relationships returned
						if(sum(ll$Length2==thisdat$LTypeMaxM & ll$Length1=='TL')>0){ # if ll table has the right conversion
							ll <- ll[ll$Length2==thisdat$LTypeMaxM,] # trim to the right initial length type
							ll <- ll[ll$Length1=='TL',] # trim to the right final length type (TL)
			
							if(nrow(ll)>0){ # if we have the right conversion
								fbdat[i, length := mean(ll$a) + mean(ll$b)*thisdat$Length]
								fbdat[i, lengthType := 'TL']
								fbdat[i, length_source := 'fishbase from LL relationship']
							}
						}

						if(sum(ll$Length1==thisdat$LTypeMaxM & ll$Length2=='TL')>0){ # if ll table has the backwards conversion
							ll <- ll[ll$Length1==thisdat$LTypeMaxM ,] # trim to the right initial length type
							ll <- ll[ll$Length2=='TL',] # trim to the right final length type (TL)

							if(nrow(ll)>0){ # if we have the right conversion
								fbdat[i, length := (thisdat$Length-mean(ll$a))/mean(ll$b)]
								fbdat[i, lengthType := 'TL']
								fbdat[i, length_source := 'fishbase from reverse LL relationship']
							}
						}
					}
					
					# if length still missing, use the next best available
					if(fbdat[i, is.na(length)]){
						fbdat[i, length := thisdat$Length]
						fbdat[i, lengthType := thisdat$LTypeMaxM]
						fbdat[i, length_source := db]
					}
				}
			}
		}
	}
}

warnings() # Warnings are "... can also be misapplied to other species", or about assuming server should be fishbase, or spp name not found, or...
	
## veryify Fishbase/Sealifebase species name lookups by hand
inds <- fbdat[,which(paste(Genus, Species) != fishbase_sci)]
fbdat[inds, .(Genus, Species, fishbase_sci, Family)] # examine rows where fishbase has a different name. None appear wrong.

		
## fix some names by hand and fill in weight where we have sources
# problem was often that validate_names returned an incorrect result
thisdat <- species('Myoxocephalus quadricornis', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'))
fbdat[paste(Genus, Species) == 'Myoxocephalus quadricornis_hexacornis', 
	':=' (fishbase_sci='Myoxocephalus quadricornis', 
	weight=thisdat$Weight, 
	weight_source='fishbase', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='fishbase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='fishbase')]

thisdat <- species('Apeltes quadracus', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'))
fbdat[paste(Genus, Species) == 'Apeltes quadracus<ca>', 
	':=' (fishbase_sci='Apeltes quadracus', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='fishbase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='fishbase')]

thisdat <- species('Gammarus locusta', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Gammarus locusta', 
	':=' (fishbase_sci='Gammarus locusta', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase')]

thisdat <- species('Gemma gemma', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Gemma gemma', 
	':=' (fishbase_sci='Gemma gemma', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase')]

thisdat <- species('Liothyrella neozelanica', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Liothyrella neozelanica', 
	':=' (fishbase_sci='Liothyrella neozelanica', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase')]

thisdat <- species('Mya arenaria', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Mya arenaria', 
	':=' (fishbase_sci='Mya arenaria', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='sealifebase')]

thisdat <- species('Leukoma thaca', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Protothaca thaca', 
	':=' (fishbase_sci='Leukoma thaca', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='sealifebase')]

thisdat <- species('Eucidaris tribuloides', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Eucidaris tribuloides', 
	':=' (fishbase_sci='Eucidaris tribuloides', 
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='sealifebase')]

thisdat <- species('Metacarcinus magister', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Metacarcinus magister', 
	':=' (fishbase_sci='Metacarcinus magister', 
	weight=1500, 
	weight_source='http://www.specialistidelvivo.com/granciporro_cancer-magister_eng.asp',
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='sealifebase')]

thisdat <- species('Saduria entomon', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Saduria entomon', 
	':=' (fishbase_sci='Saduria entomon', # validate_names returned Ketengus typus incorrectly
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase')]

thisdat <- species('Crangon crangon', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Crangon crangon', 
	':=' (fishbase_sci='Crangon crangon', # validate_names returned something else incorrectly
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='sealifebase')]

thisdat <- species('Palaemon longirostris', fields=c('Weight', 'DemersPelag', 'Length', 'LTypeMaxM'), server='https://fishbase.ropensci.org/sealifebase')
fbdat[paste(Genus, Species) == 'Palaemon longirostris', 
	':=' (fishbase_sci='Palaemon longirostris', # validate_names returned something else incorrectly
	demers_pelag_fb=thisdat$DemersPelag, 
	demers_pelag_source='sealifebase',
	length=thisdat$Length,
	lengthType=thisdat$LTypeMaxM,
	length_source='sealifebase')]

fbdat[paste(Genus, Species) == 'Onisimus litoralis', 
	':=' (fishbase_sci='Onisimus litoralis', 
	weight=1, 
	weight_source='Boudrias & Carey 1988 Marine Ecology. only up to 17 mm',
	length=1.7,
	lengthType='unknown',
	length_source='Boudrias & Carey 1988 Marine Ecology. only up to 17 mm')]

fbdat[paste(Genus, Species) == 'Hippolyte obliquimanus', 
	':=' (fishbase_sci='Hippolyte obliquimanus', 
	weight=1, 
	weight_source='only up to 3.4 mm carapace length. See Terossi et al. 2008 Marine Biology',
	length=0.34,
	lengthType='carapace length',
	length_source='only up to 3.4 mm carapace length. See Terossi et al. 2008 Marine Biology')]

fbdat[paste(Genus, Species) == 'Liocarcinus marmoreus', 
	':=' (fishbase_sci='Liocarcinus marmoreus', 
	weight=5, 
	weight_source='only up to 35 mm carapace length. See https://en.wikipedia.org/wiki/Liocarcinus_marmoreus and http://www.marlin.ac.uk/species/detail/1173',
	length=3.5,
	lengthType='carapace length',
	length_source='only up to 35 mm carapace length. See https://en.wikipedia.org/wiki/Liocarcinus_marmoreus and http://www.marlin.ac.uk/species/detail/1173')]

fbdat[paste(Genus, Species) == 'Eudorella splendida', 
	':=' (fishbase_sci='Eudorella splendida', 
	weight=1, 
	weight_source='only up to 8.5 mm body length. See Blazewicz-Paszkowycz 2001 Polish Polar Research',
	length=0.85,
	lengthType='body length',
	length_source='only up to 8.5 mm body length. See Blazewicz-Paszkowycz 2001 Polish Polar Research')]

fbdat[paste(Genus, Species) == 'Heterophoxus videns', 
	':=' (fishbase_sci='Heterophoxus videns', 
	weight=1, 
	weight_source='only up to 10 mm body length. See Dauby et al. 2001 Hydrobiologia. Or Bates et al. 2010 Nat Comm supplemental table.',
	length=1,
	lengthType='body length',
	length_source='only up to 10 mm body length. See Dauby et al. 2001 Hydrobiologia. Or Bates et al. 2010 Nat Comm supplemental table.')]

fbdat[paste(Genus, Species) == 'Nymphon sp.1', 
	':=' (weight=1, 
	weight_source='only 12.5 mm body length. See Bates et al. 2010 Nat Comm supplemental table.',
	length=1.25,
	lengthType='body length',
	length_source='only 12.5 mm body length. See Bates et al. 2010 Nat Comm supplemental table.')]

fbdat[paste(Genus, Species) == 'Orchomene pinguides', 
	':=' (fishbase_sci='Orchomene pinguides', 
	weight=1, 
	weight_source='only 7.5 mm body length. See Bates et al. 2010 Nat Comm supplemental table.',
	length=0.75,
	lengthType='body length',
	length_source='only 7.5 mm body length. See Bates et al. 2010 Nat Comm supplemental table.')]

fbdat[paste(Genus, Species) %in% c('Bathygobius sp.1', 'Bathygobius sp.2'),
	':=' (weight=10, 
	weight_source='comparison against other Gobiinae subfamily in fishbase')]

fbdat[paste(Genus, Species) == 'Paramoera walkeri', 
	':=' (fishbase_sci='Paramoera walkeri', 
	weight=1, 
	weight_source='only 6 mm body length. See Bates et al. 2010 Nat Comm supplemental table.',
	length=0.6,
	lengthType='body length',
	length_source='only 6 mm body length. See Bates et al. 2010 Nat Comm supplemental table.')]

fbdat[paste(Genus, Species) == 'Philomedidae sp.1', 
	':=' (weight=1, 
	weight_source='only 1 mm body length. See Bates et al. 2010 Nat Comm supplemental table.',
	length=0.1,
	lengthType='body length',
	length_source='only 1 mm body length. See Bates et al. 2010 Nat Comm supplemental table.')]

fbdat[paste(Genus, Species) == 'Paraleptognathia antarctica', 
	':=' (weight=1, 
	weight_source='only 4 mm body length. See Bates et al. 2010 Nat Comm supplemental table.',
	length=0.4,
	lengthType='body length',
	length_source='only 4 mm body length. See Bates et al. 2010 Nat Comm supplemental table.')]

fbdat[paste(Genus, Species)=='Mugil cephalus', 
	':=' (fishbase_sci='Mugil cephalus',
	weight=12000,
	weight_source='fishbase manual entry. fishbase text says 12kg likely too high')]



# examine where weight missing
inds <- which(is.na(fbdat$weight) & fbdat$Realm=='Marine')
fbdat[inds, .(Genus, Species, fishbase_sci, Phylum, Order, Family, demers_pelag_fb, weight_source)] # examine rows

	

	
# try to fill in missing weights for fishes with length and length-weight relationships
inds <- fbdat[, which(is.na(weight) & Realm=='Marine' & Phylum=='Chordata' & !is.na(fishbase_sci))]
length(inds); max(inds)

for(i in inds){
	cat(paste(i, " ", sep=''))

	if(fbdat[i,!is.na(length)]) { # if we now have a length measurement
		lw <- fbdat[i, length_weight(fishbase_sci)]
		if(nrow(lw)>0){ # if L-W relationships returned
			ltypes <- table(lw$Type)
			
			# if length measurements of the right type are available
			if(sum(lw$Type==fbdat$lengthType[i], na.rm=TRUE)>0){ 
				fbdat$length_a[i] <- mean(lw$a[!is.na(lw$Type) & lw$Type==fbdat$lengthType[i]])
				fbdat$length_b[i] <- mean(lw$b[!is.na(lw$Type) & lw$Type==fbdat$lengthType[i]])
				fbdat[i, weight := length_a * length ^ length_b]
				fbdat[i, weight_source := 'fishbase from LW relationship']

			# else try to convert to the right length measurement type
			} else { 
				ll <- length_length(fbdat$fishbase_sci[i])
				
				if(nrow(ll)>0){ # if L-L relationships returned
					if(sum(ll$Length2==fbdat$lengthType[i] & ll$Length1 %in% names(ltypes))>0){ # if ll table has the right conversion
						ll <- ll[ll$Length2==fbdat$lengthType[i],] # trim to the right initial length type
						ll <- ll[ll$Length1 %in% names(ltypes),] # trim to the right final length type (has to be in lw table)
						ltypes <- ltypes[names(ltypes) %in% unique(ll$Length1)] # trim ltypes from lw table to those in ll table
						comltype <- names(ltypes)[which.max(ltypes)] # most common length type in the remaining lw table
						ll <- ll[ll$Length1 == comltype,] # trim ll to the common final length type
			
						length2 <- mean(ll$a) + mean(ll$b)*fbdat$length[i]

						fbdat$length_a[i] <- mean(lw$a[!is.na(lw$Type) & lw$Type==comltype])
						fbdat$length_b[i] <- mean(lw$b[!is.na(lw$Type) & lw$Type==comltype])
						fbdat[i, weight := length_a * length2 ^ length_b]
						fbdat[i, weight_source := 'fishbase from LW and LL relationship']
					}

					if(sum(ll$Length1==fbdat$lengthType[i] & ll$Length2 %in% names(ltypes))>0){ # if ll table has the backwards conversion
						ll <- ll[ll$Length1==fbdat$lengthType[i],] # trim to the right initial length type
						ll <- ll[ll$Length2 %in% names(ltypes),] # trim to the right final length type (has to be in lw table)
						ltypes <- ltypes[names(ltypes) %in% unique(ll$Length2)] # trim ltypes from lw table to those in ll table
						comltype <- names(ltypes)[which.max(ltypes)] # most common final length type in the remaining lw table
						ll <- ll[ll$Length2 == comltype,] # trim ll to the common final length type

						length2 <- (fbdat$length[i] - mean(ll$a))/mean(ll$b)
#						type2 <- ll$Length2

						fbdat$length_a[i] <- mean(lw$a[!is.na(lw$Type) & lw$Type==comltype])
						fbdat$length_b[i] <- mean(lw$b[!is.na(lw$Type) & lw$Type==comltype])
						fbdat[i, weight := length_a * length2 ^ length_b]
						fbdat[i, weight_source := 'fishbase from LW and reverse LL relationship']
					}
				}		
			}
		}
	}
}

warnings()

fbdat[inds, .(Genus, Species, weight, length, length_a, length_b, weight_source)] # examine what was filled in

# examine where weight still missing
fbdat[is.na(weight) & Realm=='Marine', .(Genus, Species, fishbase_sci, Phylum, Class, Order, Family, demers_pelag, demers_pelag_fb, weight, length)]

# Could fill in reamaining missing with average length-weight measurements (not done)

	
# examine
fbdat[, hist(weight)]
fbdat[, hist(log10(weight))]

fbdat[weight>10000,] # large species

# create a simplified demers_pelag
fbdat[,sort(unique(demers_pelag_fb))]
fbdat[demers_pelag_fb %in% c('benthopelagic', 'bathydemersal', 'benthic', 'demersal', 'reef-associated'), demers_pelag := 'demersal']
fbdat[demers_pelag_fb %in% c('pelagic-neritic', 'pelagic-oceanic'), demers_pelag := 'pelagic']

	# check that it worked
	fbdat[demers_pelag=='demersal', sort(unique(demers_pelag_fb))]
	fbdat[demers_pelag=='pelagic', sort(unique(demers_pelag_fb))]
	fbdat[is.na(demers_pelag), sort(unique(demers_pelag_fb))]

# organize rows
setkey(fbdat, Realm, Phylum, Class, Order, Family, Genus, Species)

# remove columns and rearrange
fbdat[, ':='(length_a=NULL, length_b=NULL)]
setcolorder(fbdat, c('Genus', 'Species', 'N', 'Tmax', 'max_metric', 'error', 'error.measure', 'Multiple.measures', 'max_pretreatment', 'max_ramp', 'lat_max', 'long_max', 'elevation_max', 'REF_max', 'location_max', 'Phylum', 'Class', 'Order', 'Family', 'Realm', 'thermy', 'demers_pelag', 'demers_pelag_fb', 'demers_pelag_source', 'mobility', 'weight', 'weight_source', 'length', 'lengthType', 'length_source', 'fishbase_sci'))


# write out
write.csv(fbdat, file='data/tmax_data/dataset_1_traits.csv', row.names=FALSE) # write out traits


