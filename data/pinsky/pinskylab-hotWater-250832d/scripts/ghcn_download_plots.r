# plot GHCND-Daily data that was previously downloaded with ghcn_download.r

ghcnd_dir <- 'data_dl/ghcnd'
files <- list.files(path=ghcnd_dir, pattern='.rds', full.names=TRUE)

quartz(width=7, height=5)

for(i in 1:length(files)){

	ghcn <- readRDS(files[i])
	
	if(!is.null(ghcn)){
		plot(ghcn$date, ghcn$tmax, type='l', main=paste(i, 'of', length(files)))
	}

	readline('hit any key to continue')
}