# Temperature change vs. depth in the trawl data

require(data.table)


load("data_dl/trawl/master_hauls_March7_2017.RData") # loads hauls
hauls <- data.table(hauls)

hauls[, log10depth := log10(depth)]

hauls[,plot(surftemp_orig, surftemp)] # 1:1

hauls[, dT := surftemp - bottemp]

hauls[,plot(depth, dT, cex=0.5, col=rgb(0,0,0,0.01))]
abline(h=0, col='red')

hauls[,hist(dT)]

# divide by region
quartz(width=7, height=5.5)
par(mfrow=c(3,4), mai=c(0.4, 0.4, 0.1, 0.05), mgp=c(2,1,0), las=1)
for(reg in hauls[,sort(unique(region))]){
	hauls[region==reg, scatter.smooth(depth, dT, cex=0.5, col=rgb(0,0,0,0.05), main=reg, lpars=list(col='red'))]
	abline(h=0, col='red', lty=2)
}


# a model
require(mgcv)
mod <- gam(dT ~ s(depth) + s(month) + s(surftemp), data=hauls)
	summary(mod)
	plot(mod, pages=1)
	
mod2 <- gam(dT ~ s(log10depth,surftemp), data=hauls)
	summary(mod2)
	plot(mod2, pages=1)

	vis.gam(mod2, view=c('log10depth', 'surftemp'), type='response', plot.type='contour', zlim=c(-10,18), levels=c(-10, 0, 5, 10))

	
mod3 <- gam(dT ~ s(log10depth,surftemp) + s(month), data=hauls)
	summary(mod3)
	plot(mod3, pages=1, scheme=2) # suggests 5 °C cooler is 100-300m deeper

	par(mfrow=c(2,3), mai=c(0.4,0.4,0.1,0.05), mgp=c(2,1,0), las=1)
	for(i in c(1,3,5,7,9,11)){
		vis.gam(mod3, view=c('log10depth', 'surftemp'), cond=list(month=i), type='response', plot.type='contour', zlim=c(-10,18), levels=c(-10, 0, 5, 10), main=paste('month', i))
	}
	
mod4 <- gam(dT ~ s(log10depth,surftemp,month), data=hauls)
	summary(mod4)

	par(mfrow=c(2,3), mai=c(0.4,0.4,0.1,0.05), mgp=c(2,1,0), las=1)
	for(i in c(1,3,5,7,9,11)){
		vis.gam(mod4, view=c('log10depth', 'surftemp'), cond=list(month=i), type='response', plot.type='contour', zlim=c(-10,18), levels=c(-10, 0, 5, 10), main=paste('month', i))
	}