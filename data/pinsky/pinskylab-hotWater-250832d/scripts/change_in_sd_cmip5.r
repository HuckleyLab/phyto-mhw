# Calculate SD of monthly mean temperatures from CMIP5
# For 1986-2005 and 2081-2100

source('analysis/image.scale.r') # for plotting color bar on image
require(ncdf4)
require(RColorBrewer)
require(raster)

# read in rcp85 temperatures
infile <- nc_open('data/cmip5/tas_Amon_modmean_rcp85_ave.nc') # multi-model mean for each month 1861-2100. near-surface air.
	#print(infile)
	lst <- ncvar_get(infile, 'tas') # lst lon  x lat x time (144x72x2880)
	dim(lst)
	dimnames(lst) <- list(lon=1:144, lat=1:72, time=1:2880)
	dimnames(lst)[[1]] <- infile$var[['tas']]$dim[[1]]$vals # add lon
	dimnames(lst)[[2]] <- infile$var[['tas']]$dim[[2]]$vals # add lat
	dimnames(lst)[[3]] <- infile$var[['tas']]$dim[[4]]$vals # add time
	nc_close(infile)
	
infile <- nc_open('data/cmip5/tos_Omon_modmean_rcp85_ave.nc') # multi-model mean for each month 1861-2100. sea.
	#print(infile)
	sst <- ncvar_get(infile, 'tos') # lon  x lat x time (288x144x2880)
	dim(sst)
	dimnames(sst) <- list(lon=1:288, lat=1:144, time=1:2880)
	dimnames(sst)[[1]] <- infile$var[['tos']]$dim[[1]]$vals # add lon
	dimnames(sst)[[2]] <- infile$var[['tos']]$dim[[2]]$vals # add lat
	dimnames(sst)[[3]] <- infile$var[['tos']]$dim[[4]]$vals # add time
	nc_close(infile)
	
# Mask out ocean from tas
searast <- raster(t(!is.na(sst[,,1]))[ncol(sst):1,], xmn=0, xmx=360, ymn=-90, ymx=90)
landrast <- raster(t(!is.na(lst[,,1]))[ncol(lst):1,], xmn=0, xmx=360, ymn=-90, ymx=90)
seamask <- round(resample(searast, landrast, method='bilinear')) # includes areas 50% land as land (floor): 0 is land, 1 is ocean
seamask <- t(as.matrix(seamask)) # rearrange again to match lst
seamask <- seamask[,ncol(seamask):1]
seamask[seamask==1] <- NA # turn ocean to NA

lst2 <- lst
for(i in 1:dim(lst)[3]){
	lst2[,,i] <- lst[,,i] + seamask # turns ocean to NAs
}
		
# Detrend historical and future time-periods within each grid cell
dt <- function(x){
	if(all(!is.na(x))){
		resid(lm(x ~ I(1:length(x))))
	} else {
		return(rep(NA, length(x)))
	}
}
mohist1 <- (1986-1861)*12
mohist2 <- (2005-1861)*12+11
lstdthist <- apply(lst2[,,mohist1:mohist2], MARGIN=c(1,2), FUN=dt) # dimensions now time x lon x lat
sstdthist <- apply(sst[,,mohist1:mohist2], MARGIN=c(1,2), FUN=dt)

mofut1 <- (2081-1861)*12
mofut2 <- (2100-1861)*12+11
lstdtfut <- apply(lst2[,,mofut1:mofut2], MARGIN=c(1,2), FUN=dt)
sstdtfut <- apply(sst[,,mofut1:mofut2], MARGIN=c(1,2), FUN=dt)

# SD (seasonality)
lst_sdhist <- apply(lstdthist, MARGIN=c(2,3), FUN=sd)
sst_sdhist <- apply(sstdthist, MARGIN=c(2,3), FUN=sd)
	
lst_sdfut <- apply(lstdtfut, MARGIN=c(2,3), FUN=sd)
sst_sdfut <- apply(sstdtfut, MARGIN=c(2,3), FUN=sd)


# Plot of SD historical, SD future, and difference of SDs (tas)
zlims1 <- range(c(lst_sdhist, lst_sdfut), na.rm=TRUE)
bks1 <- seq(floor(zlims1[1]), ceiling(zlims1[2]),by=1)
cols1 <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks1)-1)
zlims2 <- range(lst_sdfut - lst_sdhist, na.rm=TRUE)
bks2 <- seq(-ceiling(max(abs(zlims2))),ceiling(max(abs(zlims2))),by=1)
cols2 <- colorRampPalette(brewer.pal(9, "RdBu"))(length(bks2)-1)
mais <- c(0.2, 0.3, 0.3, 0.1)

quartz(height=4, width=10)
# pdf(height=4, width=10, file='figures/seasonality_tas_cmip5_hist&fut_map_rcp85.pdf')
layout(matrix(c(1,2,3,6,4,5), nrow=2, ncol=3, byrow=FALSE), heights=c(3,1))
par(mai=mais)
image(x=as.numeric(dimnames(lst)[[1]]), y=as.numeric(dimnames(lst)[[2]]), lst_sdhist, col=cols1, breaks=bks1, main='1986-2005')
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims1, col=cols1, breaks=bks1, horiz=TRUE, xlab='SD historical')

par(mai=mais)
image(x=as.numeric(dimnames(lst)[[1]]), y=as.numeric(dimnames(lst)[[2]]), lst_sdfut, col=cols1, breaks=bks1, main='2081-2100')

par(mai=mais)
image(x=as.numeric(dimnames(lst)[[1]]), y=as.numeric(dimnames(lst)[[2]]), lst_sdfut-lst_sdhist, col=cols2, breaks=bks2, main='(2081-2100) minus (1986-2005)')	
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims2, col=cols2, breaks=bks2, horiz=TRUE, xlab='SD difference')
	
dev.off()


# Plot of SD historical, SD future, and difference of SDs (tos)
zlims1 <- range(c(sst_sdhist, sst_sdfut), na.rm=TRUE)
bks1 <- seq(floor(zlims1[1]), ceiling(zlims1[2]),by=1)
cols1 <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks1)-1)
zlims2 <- range(sst_sdfut - sst_sdhist, na.rm=TRUE)
bks2 <- seq(-ceiling(max(abs(zlims2))),ceiling(max(abs(zlims2))),by=1)
cols2 <- colorRampPalette(brewer.pal(9, "RdBu"))(length(bks2)-1)
mais <- c(0.2, 0.3, 0.3, 0.1)

quartz(height=4, width=10)
# pdf(height=4, width=10, file='figures/seasonality_tos_cmip5_hist&fut_map_rcp85.pdf')
layout(matrix(c(1,2,3,6,4,5), nrow=2, ncol=3, byrow=FALSE), heights=c(3,1))
par(mai=mais)
image(x=as.numeric(dimnames(sst)[[1]]), y=as.numeric(dimnames(sst)[[2]]), sst_sdhist, col=cols1, breaks=bks1, main='1986-2005')
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims1, col=cols1, breaks=bks1, horiz=TRUE, xlab='SD historical')

par(mai=mais)
image(x=as.numeric(dimnames(sst)[[1]]), y=as.numeric(dimnames(sst)[[2]]), sst_sdfut, col=cols1, breaks=bks1, main='2081-2100')

par(mai=mais)
image(x=as.numeric(dimnames(sst)[[1]]), y=as.numeric(dimnames(sst)[[2]]), sst_sdfut-sst_sdhist, col=cols2, breaks=bks2, main='(2081-2100) minus (1986-2005)')	
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims2, col=cols2, breaks=bks2, horiz=TRUE, xlab='SD difference')
	
dev.off()


# Plot of SD historical, SD future, and ratio of SDs (tas)
zlims1 <- range(c(lst_sdhist, lst_sdfut), na.rm=TRUE)
bks1 <- seq(floor(zlims1[1]), ceiling(zlims1[2]),by=1)
cols1 <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks1)-1)
zlims2 <- range(log(lst_sdfut/lst_sdhist), na.rm=TRUE)
bks2 <- seq(-ceiling(max(abs(zlims2))),ceiling(max(abs(zlims2))),by=0.1)
cols2 <- colorRampPalette(brewer.pal(9, "RdBu"))(length(bks2)-1)
mais <- c(0.2, 0.3, 0.3, 0.1)

quartz(height=4, width=10)
# pdf(height=4, width=10, file='figures/seasonality_tas_cmip5_hist-futratio_map_rcp85.pdf')
layout(matrix(c(1,2,3,6,4,5), nrow=2, ncol=3, byrow=FALSE), heights=c(3,1))
par(mai=mais)
image(x=as.numeric(dimnames(lst)[[1]]), y=as.numeric(dimnames(lst)[[2]]), lst_sdhist, col=cols1, breaks=bks1, main='1986-2005')
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims1, col=cols1, breaks=bks1, horiz=TRUE, xlab='SD historical')

par(mai=mais)
image(x=as.numeric(dimnames(lst)[[1]]), y=as.numeric(dimnames(lst)[[2]]), lst_sdfut, col=cols1, breaks=bks1, main='2081-2100')

par(mai=mais)
image(x=as.numeric(dimnames(lst)[[1]]), y=as.numeric(dimnames(lst)[[2]]), log(lst_sdfut/lst_sdhist), col=cols2, breaks=bks2, main='log((2081-2100) / (1986-2005))')	
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims2, col=cols2, breaks=bks2, horiz=TRUE, xlab='SD difference')
	
dev.off()


# Plot of SD historical, SD future, and ratio of SDs (tos)
zlims1 <- range(c(sst_sdhist, sst_sdfut), na.rm=TRUE)
bks1 <- seq(floor(zlims1[1]), ceiling(zlims1[2]),by=1)
cols1 <- colorRampPalette(brewer.pal(9, "OrRd"))(length(bks1)-1)
#zlims2 <- range(log(sst_sdfut/sst_sdhist), na.rm=TRUE)
zlims2 <- c(-3,3) # force narrow
bks2 <- c(-30, seq(-ceiling(max(abs(zlims2))),ceiling(max(abs(zlims2))),by=0.1), 30)
cols2 <- colorRampPalette(brewer.pal(9, "RdBu"))(length(bks2)-1)
mais <- c(0.2, 0.3, 0.3, 0.1)

quartz(height=4, width=10)
# pdf(height=4, width=10, file='figures/seasonality_tos_cmip5_hist-futratio_map_rcp85.pdf')
layout(matrix(c(1,2,3,6,4,5), nrow=2, ncol=3, byrow=FALSE), heights=c(3,1))
par(mai=mais)
image(x=as.numeric(dimnames(sst)[[1]]), y=as.numeric(dimnames(sst)[[2]]), sst_sdhist, col=cols1, breaks=bks1, main='1986-2005')
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims1, col=cols1, breaks=bks1, horiz=TRUE, xlab='SD historical')

par(mai=mais)
image(x=as.numeric(dimnames(sst)[[1]]), y=as.numeric(dimnames(sst)[[2]]), sst_sdfut, col=cols1, breaks=bks1, main='2081-2100')

par(mai=mais)
image(x=as.numeric(dimnames(sst)[[1]]), y=as.numeric(dimnames(sst)[[2]]), log(sst_sdfut/sst_sdhist), col=cols2, breaks=bks2, main='(2081-2100) minus (1986-2005)')	
	par(mar=c(3,1,1,1))
	image.scale(zlim=zlims2, col=cols2, breaks=bks2, horiz=TRUE, xlab='SD difference')
	
dev.off()