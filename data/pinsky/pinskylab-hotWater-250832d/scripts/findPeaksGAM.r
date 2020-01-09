# Function to detect uncertainty in local peaks in a GAM and return the number and location of them after sampling from parameter uncertainty
# Arguments:
#	mod: the GAM model (produced by mgcv)
#	newdat: a data.frame of the new data on which to predict
#	xvar: the name of the variable over which peaks should be detected
#	xtol: the sliding window within which peaks are detected. Only one peak found per window. Width measured in # data points.
#	findmaxima: TRUE if goal is to find peaks. FALSE if goal is to find valleys.
# Returns a data.frame with 

findPeaksGAM_CI <- function(mod, newdat, n, xvar, xtol, findmaxima=TRUE){
	library(mgcv)
	Xp <- predict(mod,newdat,type="lpmatrix") # prediction decomposed into the linear predictors
	br <- rmvn(n,coef(mod),mod$Vp) ## n replicate parameter vectors sampled with uncertainty
	res <- matrix(NA, ncol=n, nrow=nrow(newdat)) # to hold the predictions
	for(i in 1:n) res[,i] <- Xp %*% br[i,] ## replicate predictions

#	ord <- order(newdat[[xvar]]) # find order by xvar
#	yord <- newdat[[xvar]][ord] # order explanatory variable (xvar)
#	resord <- res[ord,] # order rows or res by xvar
	peaks <- data.frame(npeaks=rep(NA, n), peaklocs=I(vector('list', n)), y=I(vector('list', n))) # hold the number and location of peaks
	for(i in 1:n){
		temp <- find_peaks(res[,i], m=xtol, findmaxima)
		peaks$npeaks[i] <- length(temp)
		peaks$peaklocs[[i]] <- newdat[[xvar]][temp]
		peaks$y[[i]] <- res[,i]
	}
	return(peaks)
}

# Same function, but from the mean prediction (no uncertainty)
findPeaksGAM <- function(mod, newdat, xvar, xtol, findmaxima=TRUE){
	library(mgcv)
	res <- predict(mod,newdat) # prediction

	peaks <- data.frame(npeaks=rep(NA, 1), peaklocs=I(vector('list', 1)), y=I(vector('list', 1))) # hold the number and location of peaks
	temp <- find_peaks(res, m=xtol, findmaxima)
	peaks$npeaks <- length(temp)
	peaks$peaklocs[[1]] <- newdat[[xvar]][temp]
	peaks$y[[1]] <- res
	
	return(peaks)
}

## produce multivariate normal random deviates needed by findPeaksGAM
rmvn <- function(n,mu,sig) { 
	L <- mroot(sig)
	m <- ncol(L)
	t(mu + L%*%matrix(rnorm(m*n),m,n)) 
}

# find peaks in a vector
# from https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_peaks <- function (x, m = 3, findmaxima=TRUE){
	shape <- diff(sign(diff(x, na.pad = FALSE)))
	if(findmaxima){
		pks <- sapply(which(shape < 0), FUN = function(i){
			z <- i - m + 1
			z <- ifelse(z > 0, z, 1)
			w <- i + m + 1
			w <- ifelse(w < length(x), w, length(x))
			if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
		})
	}
	if(!findmaxima){
		pks <- sapply(which(shape > 0), FUN = function(i){
			z <- i - m + 1
			z <- ifelse(z > 0, z, 1)
			w <- i + m + 1
			w <- ifelse(w < length(x), w, length(x))
			if(all(x[c(z : i, (i + 2) : w)] >= x[i + 1])) return(i + 1) else return(numeric(0))
		})
	}
	pks <- unlist(pks)
	return(pks)
}


# Example using findPeaksGAM
#library(mgcv)
#n <- 200
#sig <- 2
#dat <- gamSim(1,n=n,scale=sig) # an example dataset
#mod<-gam(y~s(x0)+s(I(x1^2))+s(x2)+offset(x3),data=dat) # fit a gam
#newdat <- data.frame(x0=(0:60)/60,x1=(0:60)/60,x2=(0:60)/60,x3=(0:60)/60) # new data for projection
#
#peaks <- findPeaksGAM(mod, newdat, 500, 'x0', 5)
#
#hist(peaks$npeaks) # almost all find 2 peaks
#
#
#plot(newdat$x0, predict(mod, newdat), type='l', ylim=range(res)) # plot the basic prediction
#for(i in 1:nrow(peaks)) lines(newdat$x0, peaks$y[[i]], col=rgb(0,0,0,0.05), lty=1) # plot the uncertainty
#
#
#plot(newdat$x0, predict(mod, newdat), type='l', ylim=range(res)) # plot the basic prediction
#inds <- which(peaks$npeaks == 1)
#for(i in 1:length(inds)) lines(newdat$x0, peaks$y[[inds[i]]], col=rgb(1,0,0,0.5), lty=1) # plot the lines with 1 peak
#inds <- which(peaks$npeaks == 3)
#for(i in 1:length(inds)) lines(newdat$x0, peaks$y[[inds[i]]], col=rgb(0,1,0,0.5), lty=1) # plot the lines with 3 peaks

