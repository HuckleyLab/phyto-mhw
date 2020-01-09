# Calculate lat gradients of mean and max habitat temperatures from observations

# load data
load('temp/tosmax.rdata') # tosmax 0.25x0.25 from OISST daily + HadDTR
load('temp/hadex2_txx.rdata') # hadex2txx

# lat averages for max habitat temperature
sstmaxhrlat <- data.frame(lat = as.numeric(rownames(tosmax)))
sstmaxhrlat$mean <- apply(tosmax, MARGIN=1, FUN=mean, na.rm=TRUE)
sstmaxhrlat$sd <- apply(tosmax, MARGIN=1, FUN=sd, na.rm=TRUE)
sstmaxhrlat$n <- apply(tosmax, MARGIN=1, FUN=function(x) sum(!is.na(x)))

hadex2txxlat <- data.frame(lat = as.numeric(rownames(hadex2txx)))
hadex2txxlat$mean <- apply(hadex2txx, MARGIN=1, FUN=mean, na.rm=TRUE)
hadex2txxlat$sd <- apply(hadex2txx, MARGIN=1, FUN=sd, na.rm=TRUE)
hadex2txxlat$n <- apply(hadex2txx, MARGIN=1, FUN=function(x) sum(!is.na(x)))

# plots
par(mfrow=c(1,2))
	# land
with(hadex2txxlat, lines(lat, mean, col='orange'))
with(hadex2txxlat, lines(lat, mean-sd, col='orange', lty=2))
with(hadex2txxlat, lines(lat, mean+sd, col='orange', lty=2))

	# ocean
with(sstmaxhrlat, lines(lat, mean, col='red'))
with(sstmaxhrlat, lines(lat, mean-sd, col='red', lty=2))
with(sstmaxhrlat, lines(lat, mean+sd, col='red', lty=2))


# write out
write.csv(hadex2txxlat, file='temp/hadex2txxlat.csv')
write.csv(sstmaxhrlat, file='temp/sstmaxhrlat.csv')