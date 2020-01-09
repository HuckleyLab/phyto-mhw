## Make a table with taxonomy information for all the species in the range contraction dataset

###############
# Functions
###############
library(taxize)
library(plyr)
library(reshape2)

# pull first n words
firstwords <- function(x, n) {
  ul = unlist(strsplit(x, split = "\\s+"))[1:n]
  paste(ul,collapse=" ")
}

# parse results from classification() call
getclassification <- function(x){
	nms <- c('kingdom', 'subkingdom', 'infrakingdom', 'superphylum', 'phylum', 'subphylum', 'infraphylum', 'superclass', 'class', 'subclass', 'infraclass', 'superorder', 'order', 'suborder', 'infraorder', 'superfamily', 'family', 'subfamily', 'tribe', 'genus', 'subgenus', 'species') # column names for the output

	if(any(!is.na(x))){
		if(nrow(x)==1) print(paste('No taxonomy for', paste(x, collapse=',')))
		m <- melt(x[,1:2], measure.vars='name')
		out <- dcast(m, variable ~ rank)
		miss1 <- which(!(nms %in% colnames(out))) # find names missing from out
		if(length(miss1)>0) for(i in miss1) out[[nms[i]]] <- NA
		miss2 <- which(!(colnames(out) %in% c('variable', nms))) # find new names in out that weren't in nms
		if(length(miss2)>0) print(paste("Didn't have", paste(colnames(out)[miss2], collapse=','), "in nms"))
	
		return(out[,nms])
	} else {
		out <- setNames(data.frame(matrix(NA, ncol = length(nms), nrow = 1)), nms)		
		return(out)
	}
}

######################

dat <- read.csv('data/contraction/range_contraction.csv')
	nrow(dat)

# get good names from Global Names Resolver (EOL)
gnrs <- gnr_resolve(dat$Species)

	# try to use ITIS name. If not, get user input.
dat$Species2 <- as.character(NA)
for(i in 1:nrow(dat)){
	inds <- gnrs$submitted_name == dat$Species[i]
	if(sum(inds)>0){
		inds2 <- gnrs$data_source_title[inds] == 'ITIS'
		if(sum(inds2)==1){
			tmp <- gnrs$matched_name[inds][inds2]
			tmp <- firstwords(tmp,2) # trim off taxonomist name, since ITIS won't recognize it when fed back through classification
			dat$Species2[i] <- tmp
		}
		if(sum(inds2)>1) error(paste('too many ITIS matches for i=', i))
		if(sum(inds2)==0) { # ITIS didn't find it


			print(cbind(1:sum(inds), gnrs[inds,]))
			inds2 <- readline(prompt=paste("i=", i, ". Choose a line: "))
			tmp <- gnrs$matched_name[inds][as.numeric(inds2)]
			tmp <- twowords(tmp) # trim off taxonomist name, since ITIS won't recognize it when fed back through classification
			dat$Species2[i] <- tmp
		}
	} else {
		dat$Species2[i] <- NA
	}
}

	# examine any still missing
dat[is.na(dat$Species2),c('Species', 'Taxonomic.group', 'Species2')] # only missing copepods

	# fix a mistake
dat$Species2[dat$Species=='Arctic species assemblage'] <- NA
dat$Species2[dat$Species=='Sebastes spp.'] <- 'Sebastes'
dat$Species2[dat$Species=='Glaucopsyche alexis'] <- 'Glaucopsyche alexis'

	# examine any that changed
dat[dat$Species != dat$Species2 & !is.na(dat$Species2),c('Species', 'Taxonomic.group', 'Species2')]


# check validity of names. fix to accepted name
dat$Species3 <- NA
skip <- c('Fabulina fabula') # these fail for some reason
for(i in 1:nrow(dat)){
	cat(paste('i=',i))
	if(!(dat$Species2[i] %in% skip)) {
		tmp <- synonyms(dat$Species2[i], db='itis')
		if(!('message' %in% names(tmp[[1]]))) { # if not error message, proceed
			if('acc_name' %in% names(tmp[[1]])){
				newnms <- sort(unique(tmp[[1]]$acc_name))
				if(length(newnms)==1) {
					dat$Species3[i] <- newnms
				} else {
					print(paste('i=', i, 'too many accepted names'))
				}
			} else {
				dat$Species3[i] <- dat$Species2[i]
			}
		} else { # if an error message
			print(paste('i=', i, tmp[[1]]$message))
			dat$Species3[i] <- dat$Species2[i]
		}
	}
}
	
	# fix known errors
	dat$Species3[dat$Species2=='Fabulina fabula'] <- 'Tellina fabula'
	dat$Species3[dat$Species2=='Poecile atricapilla'] <- 'Poecile atricapillus'
	dat$Species3[dat$Species2=='Heodes tityrus'] <- 'Lycaena tityrus'
	dat$Species3[dat$Species2=='Heodes alciphron'] <- 'Lycaena alciphron'
	dat$Species3[dat$Species2=='Heodes virgaureae'] <- 'Lycaena virgaureae'
	dat$Species3[dat$Species2=='Strymonidia w-album'] <- 'Satyrium w-album'
	dat$Species3[dat$Species2=='Polygonum c-album'] <- 'Polygonia c-album'
	dat$Species3[dat$Species2=='Everes argiades'] <- 'Cupido argiades'
	dat$Species3[dat$Species2=='Clossiana dia'] <- 'Boloria dia'
	dat$Species3[dat$Species2=='Clossiana selene'] <- 'Boloria selene'
	dat$Species3[dat$Species2=='Clossiana euphrosyne'] <- 'Boloria euphrosyne'

	# examine names where they have changed
	dat[is.na(dat$Species3) | (dat$Species != dat$Species3), c('Species', 'Species2', 'Species3')]


# get classification from ITIS
inds <- !is.na(dat$Species3) # only search for values with cleaned up species names (skip NAs, which return too many possibilities)
cls <- classification(dat$Species3[inds], db='itis')

	# cls[[27]] <- classification('Poecile atricapillus', db='itis')[[1]]
	# cls[[80]] <- classification('Cupido argiades', db='itis')[[1]]
	# cls[[82]] <- classification('Lycaena tityrus', db='itis')[[1]]
	# cls[[84]] <- classification('Boloria dia', db='itis')[[1]]
	# cls[[85]] <- classification('Boloria selene', db='itis')[[1]]
	# cls[[92]] <- classification('Lycaena alciphron', db='itis')[[1]]
	# cls[[93]] <- classification('Lycaena virgaureae', db='itis')[[1]]
	# cls[[95]] <- classification('Satyrium w-album', db='itis')[[1]]
	# cls[[102]] <- classification('Boloria euphrosyne', db='itis')[[1]]
	# cls[[106]] <- classification('Polygonia c-album', db='itis')[[1]]
	# names(cls)[c(27, 80, 82, 84, 85, 92, 93, 95, 102, 106)] <- c('Poecile atricapillus', 'Cupido argiades', 'Lycaena tityrus', 'Lycaena alciphron', 'Boloria dia', 'Boloria selene', 'Lycaena virgaureae', 'Satyrium w-album', 'Boloria euphrosyne', 'Polygonia c-album')

clsdf <-ldply(.data=cls, .fun=getclassification) # make a dataframe of the results
names(clsdf)[1] <- 'Species3'
clsdf[] <- lapply(clsdf, factor) # convert columns to factors. [] necessary to keep clsdf as a dataframe
	summary(clsdf)
	nrow(clsdf) # same as number of non-NA Species3 entries in dat
	sum(inds)

	# Add NAs back in
	notdups <- !duplicated(dat[,c('Species', 'Species3')])
	clsdf <- merge(clsdf, dat[notdups,c('Species', 'Species3')], all.y=TRUE)
	nrow(clsdf)
	
	# Examine missing values
	clsdf[is.na(clsdf$kingdom), c('Species', 'Species3', 'kingdom')]
	
	

# try classification by genus for trouble species
# work on clsdf data.frame
inds <- which(is.na(clsdf$kingdom) & !is.na(clsdf$Species3))
gens <- sapply(as.character(clsdf$Species3[inds]), firstwords, 1)
cls2 <- classification(gens, db='itis') # may have to make some choices: a bad sign. Enter manually in taxonomy.csv instead

cls2df <-ldply(.data=cls2, .fun=getclassification)
names(cls2df)[1] <- 'Species3_genus'
cls2df[] <- lapply(cls2df, factor) # convert columns to factors. [] necessary to keep clsdf as a dataframe
	summary(cls2df)
	nrow(cls2df)
	length(inds) # should match
	
	# Examine missing values, then remove
	cls2df[is.na(cls2df$kingdom), c('Species3_genus', 'kingdom')]

	cls2df <- cls2df[!is.na(cls2df$kingdom), ]

	# Examine rows with species filled in: not good (we are searching on genus)
	cls2df[!is.na(cls2df$species), c('Species3_genus', 'kingdom', 'species')]

	cls2df <- cls2df[is.na(cls2df$species), ]
	
	# Plant rows: not good: remove if any
	cls2df[cls2df$kingdom != 'Animalia' & !is.na(cls2df$kingdom), c('Species3_genus', 'kingdom', 'species')]

	# Examine the rest
	cls2df[, c('Species3_genus', 'family', 'genus')]
	nrow(cls2df)
	length(gens)

	# put cls2df (by genus) back into clsdf (by species)
	clsdf[] <- lapply(clsdf, as.character) # convert columns to characters to allow new values input
	cls2df[] <- lapply(cls2df, as.character) # convert columns to characters to allow new values input

	matches <- match(names(gens), clsdf$Species3) # match rows
	cbind(clsdf$Species3[matches], names(gens), cls2df$Species3_genus) # examine to make sure the order matches
	all(cls2df$Species3_genus == sapply(as.character(clsdf$Species3[matches]), firstwords, 1)) # TRUE: genus order matches
	all(names(gens) == clsdf$Species3[matches]) # TRUE: species order matches
	nms <- c("kingdom", "subkingdom", "infrakingdom", "superphylum", "phylum", "subphylum", "infraphylum", "superclass", "class", "subclass", "infraclass", "superorder", "order", "suborder", "infraorder", "superfamily", "family", "subfamily", "tribe", "genus", "subgenus", "species")
	all(names(clsdf[matches,nms]) == names(cls2df[,nms])) # TRUE: column names match
	clsdf[matches,nms] <- cls2df[,nms] # copy over
	clsdf$species[matches] <- clsdf$Species3[matches] # copy over species (binomial)

	
# Fix missing or incorrect entries from a table made by hand
handfix <- read.csv('data/contraction/taxonomy_by_hand.csv', stringsAsFactors=FALSE) # read in
	nrow(handfix)
all(names(cls2df[,nms]) == names(handfix[,nms])) # check that columns match (other than first one in handfix)

inds1 <- clsdf$Species3 %in% handfix$Species3 # find rows to match (Species3 column)
inds2 <- clsdf$Species %in% handfix$Species3 # find rows to match (look in Species column)
	sum(inds1)
	sum(inds2)
	sum(inds2 | inds1)
	nrow(handfix) # should match
matches1 <- match(clsdf$Species3[inds1], handfix$Species3) # match rows (Species3)
matches2 <- match(clsdf$Species[inds2], handfix$Species3) # match rows (Species3)
	length(matches1)
	length(matches2)
sum(inds1) == length(matches1) # true? make sure they match
sum(inds2) == length(matches2) # true? make sure they match
clsdf[inds1,nms] <- handfix[matches1,nms] # copy over data
clsdf[inds2,nms] <- handfix[matches2,nms] # copy over data

	# Examine clsdf again
	clsdf[inds1 | inds2, c('Species', 'Species3', 'kingdom', 'phylum', 'family', 'genus')]


# check the taxonomy table
nrow(clsdf)
nrow(dat)

	# Obvious errors	
	clsdf[is.na(clsdf$kingdom),c('Species', 'Species3', 'kingdom', 'phylum', 'family', 'genus')] # missing kingdom
	clsdf[clsdf$kingdom != 'Animalia',] # not Animal
	
	# Missing taxonomy
	clsdf[is.na(clsdf$phylum),c('Species', 'Species3', 'kingdom', 'phylum', 'family', 'genus')]
	clsdf[is.na(clsdf$class),c('Species', 'Species3', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus')]
	clsdf[is.na(clsdf$order),c('Species', 'Species3', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus')]
	clsdf[is.na(clsdf$family),c('Species', 'Species3', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus')]
	clsdf[is.na(clsdf$genus),c('Species', 'Species3', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus')]
	
# fill in some missing data
clsdf$order[clsdf$Species=='Notomastus latericeus'] <- 'Capitellida'
clsdf$order[clsdf$Species=='Ophelina acuminata'] <- 'missing'
clsdf$order[clsdf$Species=='Scoloplos armiger'] <- 'missing'
clsdf$order[clsdf$Species=='Levinsenia gracilis'] <- 'missing'
clsdf$order[clsdf$Species=='Scalibregma inflatum'] <- 'Capitellida'
	
# Remove any duplicate rows
dups <- duplicated(clsdf)
	sum(dups)
clsdf <- clsdf[!dups,]
	
# Format nicely
out <- clsdf[, c("Species", "kingdom", "subkingdom", "infrakingdom", "superphylum", "phylum", "subphylum", "infraphylum", "superclass", "class", "subclass", "infraclass", "superorder", "order", "suborder", "infraorder", "superfamily", "family", "subfamily", "tribe", "genus", "subgenus", "species")]
out <- out[order(out$kingdom, out$phylum, out$class, out$order, out$family, out$genus, out$species),]
	
	
# Write out
write.csv(out, file='temp/range_contraction_taxonomy.csv', row.names=FALSE)