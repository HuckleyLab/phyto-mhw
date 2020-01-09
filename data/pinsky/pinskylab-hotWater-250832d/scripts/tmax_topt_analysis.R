### hotwater analysis of tmax vs topt in different taxa
### goal for this script: test relationships between tmax and topt
### code by JS, April 9


# load packages -----------------------------------------------------------
library(tidyverse)
library(cowplot)

getwd()

# read in data ------------------------------------------------------------
microbe_data <- read_csv("data/topt_tmax/Chen_2015_fbv009supp.csv") # Chen 2015 J Plankton Res
insect_data <- read_csv("data/topt_tmax/Deutsch_thermal_curves.csv") # Deutsch et al. 2008 PNAS
lizard_data <- read_csv("data/topt_tmax/Huey_2009_supp_data.csv") # Huey et al. 2009 Proceedings B
alldata<-data.frame(topt=c(microbe_data$t_opt, insect_data$Topt, lizard_data$newTopt), 
                    tmax=c(microbe_data$t_max, insect_data$CTmax, lizard_data$CTmax), 
                    group=c(rep("microbes", length(microbe_data$t_opt)),
                                rep("insects", length(insect_data$Topt)),
                                rep("lizards", length(lizard_data$newTopt))))

# plot
alldata %>%
  ggplot(aes(x=topt, y=tmax, colour=group)) + geom_point() +
  theme_bw() + geom_smooth(method = "lm") + 
  ylab("Upper thermal limit, °C") + xlab("Optimum temperature, °C") + geom_abline(linetype=2, col="grey")
ggsave("figures/Tmax_Topt.pdf", width=4, height=4)


# analysis
mod <- lm(tmax ~ topt*group, data=alldata)
	summary(mod)
	
mod2 <- lm(tmax ~ topt+group, data=alldata)
	summary(mod2)

mod3 <- lm(tmax ~ topt, data=alldata)
	summary(mod3)

anova(mod, mod2)
anova(mod2, mod3)
AIC(mod, mod2, mod3)


# make predictions
	# mod2
preds <- rbind(data.frame(group='microbes', topt=seq(3,35,by=1)), data.frame(group='insects', topt=seq(20,36,by=1)), data.frame(group='lizards', topt=seq(27,40,by=1)))
preds$tmax <- predict(mod2, newdata=preds)
preds$tmax.se <- predict(mod2, newdata=preds, se.fit=TRUE)$se.fit

	# mod1
preds2 <- data.frame(topt=seq(3,40,by=1))
preds2$tmax <- predict(mod3, newdata=preds2)
preds2$tmax.se <- predict(mod3, newdata=preds2, se.fit=TRUE)$se.fit

# write predictions
write.csv(preds2, file='temp/topt_tmax_fit.csv', row.names=FALSE)

# plot with mod2
quartz(height=4, width=4)
par(mai=c(1,1,0.1, 0.1))
plot(tmax ~ topt, data=alldata, col=sapply(as.character(alldata$group), FUN= function(x) switch(x, microbes='#1b9e7755', insects='#d95f0255', lizards='#7570b355')))
abline(0,1, col='grey', lty=2)

with(preds[preds$group=='microbes', ], polygon(c(topt, rev(topt)), c(tmax-tmax.se, rev(tmax+tmax.se)), col='#1b9e7755', border=NA))
with(preds[preds$group=='microbes', ], lines(topt, tmax, col='#1b9e77'))

with(preds[preds$group=='insects', ], polygon(c(topt, rev(topt)), c(tmax-tmax.se, rev(tmax+tmax.se)), col='#d95f0255', border=NA))
with(preds[preds$group=='insects', ], lines(topt, tmax, col='#d95f02'))

with(preds[preds$group=='lizards', ], polygon(c(topt, rev(topt)), c(tmax-tmax.se, rev(tmax+tmax.se)), col='#7570b355', border=NA))
with(preds[preds$group=='lizards', ], lines(topt, tmax, col='#7570b3'))



# plot with mod3
quartz(height=4, width=4)
par(mai=c(1,1,0.1, 0.1))
plot(tmax ~ topt, data=alldata, col='#00000055')
abline(0,1, col='grey', lty=2)

with(preds2, polygon(c(topt, rev(topt)), c(tmax-tmax.se, rev(tmax+tmax.se)), col='#00000055', border=NA))
with(preds2, lines(topt, tmax, col='#000000'))