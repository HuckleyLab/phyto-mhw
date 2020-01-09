require(lme4)
require(MuMIn)
require(piecewiseSEM) # for sem.model.fits

############
# Data prep
############
dat<-read.csv(file="data/contraction/range_contraction.csv", header=TRUE)
tax <- read.csv(file='temp/range_contraction_taxonomy.csv', header=TRUE)

# trim out records to exclude (e.g., records not at a warm range edge)
	dim(dat)
dat <- dat[dat$Include==TRUE,]
	dim(dat)
	
# add taxonomy
	nrow(dat)
dat <- merge(dat, tax[,c('Species', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus')])
	nrow(dat)

# new columns
dat$duration <- dat$DateEnd - dat$DateStart # duration of study
dat$duration.c <- dat$duration - mean(dat$duration) # re-center (mean 33)
dat$extirp <- dat$Local.extinction == 'Local extinction' # TRUE if extirpated
dat$DateEnd.rs <- (dat$DateEnd - mean(dat$DateEnd))/sd(dat$DateEnd) # re-scaled end date
dat$absLat <- abs(dat$Latitude) # absolute latitude
	absLat.mn <- mean(dat$absLat)
	absLat.sd <- sd(dat$absLat)
dat$absLat.c <- (dat$absLat - absLat.mn) # re-centered for mean zero
dat$absLat.rs <- (dat$absLat - absLat.mn)/absLat.sd # re-scaled for SD 1
dat$mobilityTF <- dat$Mobility == 'mobile' # true/false. put crawl with sessile

#######################
# basic proportions
# with binomial CIs
#######################
tab2 <- as.matrix(table(dat$Local.extinction, dat$Habitat))
prop2 <- tab2/rbind(colSums(tab2),colSums(tab2)) # proportions
cis2_1 <- prop.test(x = t(as.matrix(tab2[,1])), conf.level = 0.95)$conf.int # for marine
cis2_2 <- prop.test(x = t(as.matrix(tab2[,2])), conf.level = 0.95)$conf.int # for terrestrial

	# format for output
	out2 <- data.frame(habitat=colnames(tab2), nExt=tab2[1,], nSp=colSums(tab2), propext=prop2[1,], l95=c(cis2_1[1], cis2_2[1]), u95=c(cis2_1[2], cis2_2[2]))

	out2

	# write out proportions
	write.csv(out2, 'temp/warm_edge_contractions.csv')

# proportions by mobility class
tab3 <- as.array(table(dat$Local.extinction, dat$Habitat, dat$mobilityTF))
prop3 <- tab3
prop3[,,1] <- prop3[,,1]/rbind(colSums(prop3[,,1]), colSums(prop3[,,1]))
prop3[,,2] <- prop3[,,2]/rbind(colSums(prop3[,,2]), colSums(prop3[,,2]))
prop3

tab4 <- as.array(table(dat$Local.extinction, dat$Habitat, dat$Mobility))
tab4
prop4 <- tab4
prop4[,,1] <- prop4[,,1]/rbind(colSums(prop4[,,1]), colSums(prop4[,,1]))
prop4[,,2] <- prop4[,,2]/rbind(colSums(prop4[,,2]), colSums(prop4[,,2]))
prop4[,,3] <- prop4[,,3]/rbind(colSums(prop4[,,3]), colSums(prop4[,,3]))
prop4

################
#plot the data
################
source('analysis/error.bar.R')

# stacked barchart of latitudes
bks <- seq(-90,90,by=10)
h1 <- hist(dat$Latitude[dat$Habitat=='Terrestrial'], breaks=bks, plot=FALSE)
h2 <- hist(dat$Latitude[dat$Habitat=='Marine'], breaks=bks, plot=FALSE)

tab <- rbind(h1$counts, h2$counts)

quartz(height=3, width=4)
# pdf(height=3, width=4, file='figures/local_extirpations_lat_hist.pdf')
par(mai=c(0.7, 0.7, 0.1, 0.1), las=1, mgp=c(1.7, 0.5, 0), cex.axis=1, cex.lab=1.2, tcl=-0.2)
barplot(height=tab, beside=FALSE, col=c('#1f78b4', '#b2df8a'), ylab='Count', xlab='Latitude', names.arg=h1$mids)

dev.off()


# plot proportions
cols <- '#ca0020'

quartz(height=4, width=4)
xs <- barplot(height=prop[1,], names.arg=colnames(prop), col=cols, ylim=c(0, 0.7), ylab='Local extirpations (proportion)', las=1)
error.bar(x=xs, y=prop[1,], lower=prop[1,] - c(cis1[1], cis2[1]), upper=c(cis1[2], cis2[2])-prop[1,])

dev.off()



# plot vs. latitude
cols <- rep('#1f78b464', nrow(dat))
cols[dat$Habitat=='Terrestrial'] <- '#b2df8a64'
plot(dat$absLat, jitter(as.numeric(dat$extirp)), col=cols, pch=16)

###############################
# make a table of observations by study
# TableS4
###############################

out <- data.frame(study=sort(unique(dat$Reference)))
out <- merge(out, aggregate(list(habitat=dat$Habitat), by=list(study=dat$Reference), FUN= function(x) paste(unique(x), sep=',')))
out <- merge(out, aggregate(list(animal=dat$Taxonomic.group), by=list(study=dat$Reference), FUN= function(x) paste(unique(x), collapse=',')))
out <- merge(out, aggregate(list(lat=dat$Latitude), by=list(study=dat$Reference), FUN= function(x) round(mean(x),1)))
out <- merge(out, aggregate(list(dur=dat$duration), by=list(study=dat$Reference), FUN= function(x) round(mean(x),0)))
out <- merge(out, aggregate(list(num_contraction=dat$Local.extinction), by=list(study=dat$Reference), FUN= function(x) sum(x=='Local extinction')))
out <- merge(out, aggregate(list(num_sampled=dat$Local.extinction), by=list(study=dat$Reference), FUN= function(x) length(x)))
out$prop_contract <- round(out$num_contraction/out$num_sampled,2)
out$prop_contract <- round(out$num_contraction/out$num_sampled,2)

out <- out[order(out$habitat, out$study),]
out

write.csv(out, file='tables/TableS4.csv')


#########################################
# output data for supplemental dataset
# dataset S2
#########################################
out <- dat[,c('Species', 'Habitat', 'Reference', 'extirp', 'Latitude', 'DateStart', 'DateEnd', 'Method', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'Mobility')]

head(out)

write.csv(out, quote=8, na='', file='tables/datasetS2.csv', row.names=FALSE)


######################
# analysis
######################

# how much data
table(dat$Habitat)
table(dat$Taxonomic.group, dat$Habitat)
table(dat$Taxonomic.group, dat$Habitat)
	
table(dat$Habitat)
with(dat, table(Taxonomic.group, Habitat)) # Taxon
with(dat, table(Latitude>0, Habitat)) # hemisphere
with(dat, table(Latitude>=35 & Latitude<=66.5, Habitat)) # temperate
with(dat, table(Latitude>=23.5 & Latitude<=35, Habitat)) # subtropical
with(dat, table(Latitude<=23.5, Habitat)) # tropical

# summary stats
table(dat$Local.extinction, dat$Habitat)

range(dat$Latitude)
range(abs(dat$Latitude))

sort(unique(dat$Latitude))
range(abs(dat$Latitude))
range(dat$Latitude[dat$Latitude>0])
range(dat$Latitude[dat$Latitude<0])


# Proportion test (approximate)
prop.test(x=out2$nExt, n=out2$nSp, alternative='two.sided') 




# statistical models for extirpation
mod2 <- glmer(extirp ~ Habitat + absLat.c + Method + duration.c + (1|phylum/class/order/family), data=dat, family=binomial, control=glmerControl(optimizer='Nelder_Mead', optCtrl=list(maxfun=50000)))
	summary(mod2)
	rsquared(mod2, method='delta') # piecewiseSEM
	rsquared(mod2, method='theoreticals') # piecewiseSEM
	
#	confs <- confint(mod2) # confidence intervals
#	confs
	
	# calculate p(ext) on response scale (from coefficients)
	n <- 1000 # how many samples
	summ <- summary(mod2)$coefficients
	mareff <- rnorm(n, summ[1,1], sd=summ[1,2]) # marine effect (intercept)
	terreff <- mareff + rnorm(n, summ[2,1], sd=summ[2,2])
	mresp <- exp(mareff)/(1+exp(mareff)) # response scale (p(ext)
	tresp <- exp(terreff)/(1+exp(terreff))
	median(mresp); quantile(mresp, c(0.025, 0.975))
	median(tresp); quantile(tresp, c(0.025, 0.975))
	
	# calculate p(ext) on response scale (from predictions)
	predict(mod2, newdata=data.frame(Habitat=c('Marine', 'Terrestrial'), absLat.c=c(0,0), Method='Many times, many locations', duration.c=0), re.form=NA, type='response') # but won't do CIs
		
		# bootstrap approach
		boot1 <- bootMer(mod2, nsim=100, FUN= function(x) predict(x, newdata=data.frame(Habitat=c('Marine', 'Terrestrial'), absLat.c=c(0,0), Method='Many times, many locations', duration.c=0), re.form=NA, type='response'))
		require(boot)
		boot.ci(boot1, index=1, type=c('norm', 'basic', 'perc')) # can't do BCA because need bootstrap variances (how to get?)
		
		# predictIntervals() approach (from https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r)
		require(merTools)
		preds <- predictInterval(mod2, newdata=data.frame(Habitat=c('Marine', 'Terrestrial'), absLat.c=0, Method='Many times, many locations', duration.c=0, phylum='Mollusca', class='Bivalvia', order='Veneroida', family='Semelidae'), n.sims=1000, which='fixed', level=0.95, type='probability', include.resid.var=FALSE)
			preds # marine and terrestrial
		
	# calculate RVI
	options(na.action = "na.fail")
	mod2s <- dredge(mod2, beta=FALSE, evaluate=TRUE, rank='AICc', trace=1)
#	subset(mod2s, delta < 6)
#
	modavg2 <- model.avg(mod2s)
	summary(modavg2)
#	
#	mod2best <- get.models(mod2s, subset=1)[[1]]

	
# predictions
	nd <- data.frame(Habitat=rep(c('Marine', 'Terrestrial'), c(40,40)), absLat.c=rep(seq(-30,30,length.out=40), 2), Method='Many times, many locations', duration.c=0, DateEnd.rs=0)
	nd$absLat <- nd$absLat.c + absLat.mn
	
	# trim to latitudes with dat
	nd <- nd[nd$Habitat=='Marine' | (nd$Habitat=='Terrestrial' & nd$absLat <= max(abs(dat$Latitude[dat$Habitat=='Terrestrial'])) & nd$absLat >= min(abs(dat$Latitude[dat$Habitat=='Terrestrial']))),]
	nd <- nd[nd$Habitat=='Terrestrial' | (nd$Habitat=='Marine' & nd$absLat <= max(abs(dat$Latitude[dat$Habitat=='Marine'])) & nd$absLat >= min(abs(dat$Latitude[dat$Habitat=='Marine']))),]
	
	# full and best model
	nd$pExt_mod2full <- predict(mod2, newdata=nd, re.form=NA, type='response') # full model
#	nd$pExt_mod2best <- predict(get.models(mod2s,subset=1)[[1]], newdata=nd, re.form=NA, type='response') # best
	
	
	# model-averaged predictions
#	pred.parms2 <- get.models(mod2s, subset=delta<4)
#	model.preds2 = sapply(pred.parms2, predict, newdata = nd, type='response', re.form=NA)
#	nd$pExt_mod2avg <- model.preds2 %*% Weights(mod2s)[1:ncol(model.preds2)]





##############
# plot the models
##############

cols1 <- rep('#1f78b464', nrow(dat))
cols2 <- c('#1f78b464', '#b2df8a64') # marine, terrestrial
cols1[dat$Habitat=='Terrestrial'] <- '#b2df8a64'

quartz(width=5, height=4)
par(las=1)
plot(dat$absLat, jitter(as.numeric(dat$extirp)), col=cols1, pch=16, cex=0.5, xlab='abs(Latitude)', ylab='Probability of extirpation')

	# full model (thin)
lines(pExt_mod2full ~ absLat, data=nd[nd$Habitat=='Marine',], col=cols2[1], type='l', lwd=1)
lines(pExt_mod2full ~ absLat, data=nd[nd$Habitat=='Terrestrial',], col=cols2[2], type='l', lwd=1)

	# best model (thin dashed)
lines(pExt_mod2best ~ absLat, data=nd[nd$Habitat=='Marine',], col=cols2[1], type='l', lwd=1, lty=2)
lines(pExt_mod2best ~ absLat, data=nd[nd$Habitat=='Terrestrial',], col=cols2[2], type='l', lwd=1, lty=2)

	# model averages (thick)
lines(pExt_mod2avg ~ absLat, data=nd[nd$Habitat=='Marine',], col=cols2[1], type='l', lwd=2, lty=1) 
lines(pExt_mod2avg ~ absLat, data=nd[nd$Habitat=='Terrestrial',], col=cols2[2], type='l', lwd=2, lty=1)



####################################
# make a table of the model results
####################################
modtable <- as.data.frame(summary(mod2)$coefficients) # for full model

# round
modtable <- signif(modtable, 2)

# add RVI
modtable$RVI <- c('', signif(as.numeric(modavg2$importance),2))

# adjust colnames
names(modtable) <- c('Estimate', 'Standard error', 'z', 'p-value', 'RVI')

# adjust rownames
rownames(modtable) <- c('Intercept', 'Habitat (Terrestrial)', 'abs(Latitude)', 'Method (Two surveys)', 'Duration')

#examine
modtable

write.csv(modtable, file='tables/Table1.csv', quote=FALSE)