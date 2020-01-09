# Test whether acclimation temperature is important for upper thermal limits
require(lme4)

#################
# Read in data
#################

# Measurements of upper thermal tolerance
dat<-read.csv(file="./data/tmax_data/dataset_1_hotwater.csv", header=T)
	table(dat$used_for_thermal_safety_margin_analysis, dat$habitat_intertidal) # 102 marine, 249 terrestrial

	#process data####
	dat<-subset(dat, !habitat=="freshwater") #remove freshwater species
	dat<-subset(dat, !habitat_intertidal=="intertidal") #remove intertidal species
	dat<-subset(dat, !is.na(tmax) & !is.na(tmax_acc)) #remove missing data
	dat <- droplevels(dat)


###########################################
# Tmax model with acclimation temperature
###########################################

terr1 <- lmer(tmax~tmax_acc + tmax_metric + (1|Phylum/Class/Order/Family/Genus), data=dat, subset=habitat=='terrestrial')
	summary(terr1)
mari1 <- lmer(tmax~tmax_acc + tmax_metric + (1|Order/Family/Genus), data=dat, subset=habitat=='marine') #ignore phylum and class since invariant in this subset
	summary(mari1)


# predictions
l <- 20
pred_terr <- data.frame(tmax_acc=seq(0,50,length.out=l), tmax_metric=rep('crit', l))
pred_mari <- data.frame(tmax_acc=seq(0,50,length.out=l), tmax_metric=rep('crit', l))
pred_terr$tmax <- predict(terr1, newdata=pred_terr, re.form=NA) # re.form to ignore random effects
pred_mari$tmax <- predict(mari1, newdata=pred_mari, re.form=NA)

#############
# Plots
#############

quartz(width=8, height=5)
# pdf(width=8, height=5, file='figures/tmax_vs_tmax_acc_w_fit.pdf')
par(mfrow=c(1,2))
plot(tmax ~ tmax_acc, data=dat, subset=habitat=='terrestrial', main='Terrestrial', col=c('red', 'blue')[1+(dat$tmax_metric=='crit')])
	lines(tmax ~ tmax_acc, data=pred_terr, col='blue')
plot(tmax ~ tmax_acc, data=dat, subset=habitat=='marine', main='Marine', col=c('red', 'blue')[1+(dat$tmax_metric=='crit')])
	lines(tmax ~ tmax_acc, data=pred_mari, col='blue')

legend('bottomright', legend=c('leth', 'crit'), col=c('red', 'blue'), pch=1)


dev.off()