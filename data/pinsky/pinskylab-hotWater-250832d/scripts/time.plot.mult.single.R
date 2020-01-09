## Nice time series plots with geological time scale as x-axis
## This function plots multiple windows with a single time-axis
## if directly writing to file, close dev.off
time.plot.mult <- function(nrow=1, ncol=1, plot.height=6, plot.width=9, time.height=1, bg="white", 
                           cex.axis=1.2, cex.lab=1.2, cex=1.2, cex.intervals=0.75, 
                           bottom.mar = 3.25, left.mar = 3.5, top.mar = 0.75, right.mar = 1, mgp=c(2, 0.75, 0), las = 0, xaxs="i", 
                           lab.length = 1, eps.name = NA, pdf.name = "Figure 4B new.pdf", tiff.name = NA, all.cols=NULL, timescale='phanerozoic') {
  
  if(length(plot.width) == 1 & ncol > 1) {
    plot.width <- rep(plot.width, ncol)
  }
  total.width <- sum(plot.width)
  n.frames <- nrow*ncol+ncol
  #	print(n.frames)
  total.height = sum(plot.height) + time.height
  height.prop <- c(plot.height/total.height, time.height/total.height)
  #	print(height.prop)
  width.prop <- plot.width/total.width
  
  my.layout <- matrix(c((1:n.frames)[-(1:ncol)],1:ncol), nrow=nrow+1, ncol=ncol, byrow=TRUE)
  
  
  c((1:n.frames)[-(1:ncol)],1:ncol)
  # create a timescale axis
  if(timescale=='post-paleozoic'){
    periods <- rev(c(0.00, 23.03, 66.0, 145.0, 201.3, 252.2))
    names(periods) <- rev(c("Recent","Neogene","Paleogene","Cretaceous","Jurassic","Triassic"))
    n.periods <- length(periods)
    period.mid <- vector(mode="numeric", length=length(periods)-1)
    for (i in 1:(length(periods)-1)) {
      period.mid[i] <- mean(periods[i:(i+1)])
    }
    cambrian <- max(periods)
    if(lab.length==1) {
      period.labs <- c("Tr", "J", "K", "Pg", "Ng")
    } else if (lab.length==2) {
      period.labs <- c("Tr", "J", "K", "Pg", "N")
    } else if (lab.length==3) {
      period.labs <- c("Tr", "J", "K", "P", "N")
    } else if (lab.length==4) {
      period.labs <- c("T", "J", "K", "P", "N")
    }
    my.y <- 1:length(periods)
  } else if(timescale=='geozoic'){
    periods <- rev(c(0.00, 541.0, 2500, 4000))
    names(periods) <- rev(c("Phanerozoic","Proterozoic","Archaean"))
    n.periods <- length(periods)
    period.mid <- vector(mode="numeric", length=length(periods)-1)
    for (i in 1:(length(periods)-1)) {
      period.mid[i] <- mean(periods[i:(i+1)])
    }
    cambrian <- max(periods)
    if(lab.length==1) {
      period.labs <- c("Archaean","Proterozoic","Phanerozoic")
    } else if (lab.length==2) {
      period.labs <- c("Archaean","Proterozoic","Phanerozoic")
    } else if (lab.length==3) {
      period.labs <- c("Archaean","Proterozoic","Phanero.")
    } else if (lab.length==4) {
      period.labs <- c("Archaean","Protero.","Phanero.")
    }
    my.y <- 1:length(periods)
  } else if(timescale=='cenozoic') {
    periods <- rev(c(0.00, 0.0117, 2.58, 5.333, 23.03, 33.9, 56, 66.0))
    names(periods) <- rev(c("Holocene","Pleistocene","Pliocene","Miocene","Oligocene","Eocene","Paleocene"))
    n.periods <- length(periods)
    period.mid <- vector(mode="numeric", length=length(periods)-1)
    for (i in 1:(length(periods)-1)) {
      period.mid[i] <- mean(periods[i:(i+1)])
    }
    cambrian <- max(periods)
    if(lab.length==1) {
      period.labs <- c("Pal", "E", "O", "M", "Pli", "Ple","")
    } else if (lab.length==2) {
      period.labs <- c("P", "E", "O", "M", "Pli", "Ple","")
    } else if (lab.length==3) {
      period.labs <- c("P", "E", "O", "M", "P", "P","")
    } else if (lab.length==4) {
      period.labs <- c("P", "E", "O", "M", "P", "P","")
    }
    my.y <- 1:length(periods)
  }   else if(timescale=='maas-cenozoic') {
    periods <- rev(c(0.00, 0.0117, 2.58, 5.333, 23.03, 33.9, 56, 66.0, 72.1))
    names(periods) <- rev(c("Holocene","Pleistocene","Pliocene","Miocene","Oligocene","Eocene","Paleocene","Cretaceous"))
    n.periods <- length(periods)
    period.mid <- vector(mode="numeric", length=length(periods)-1)
    for (i in 1:(length(periods)-1)) {
      period.mid[i] <- mean(periods[i:(i+1)])
    }
    cambrian <- max(periods)
    if(lab.length==1) {
      period.labs <- c("Maas","Pal", "E", "O", "M", "Pli", "Ple","")
    } else if (lab.length==2) {
      period.labs <- c("Maas","P", "E", "O", "M", "Pli", "Ple","")
    } else if (lab.length==3) {
      period.labs <- c("Maas","P", "E", "O", "M", "P", "P","")
    } else if (lab.length==4) {
      period.labs <- c("K","P", "E", "O", "M", "P", "P","")
    }
    my.y <- 1:length(periods)
  }  else if(timescale=='post-cambrian') {
    periods <- rev(c(0.00, 23.03, 66.0, 145.0, 201.3, 252.2, 298.9, 358.9, 419.2, 443.4, 485.4))
    names(periods) <- rev(c("Recent","Neogene","Paleogene","Cretaceous","Jurassic","Triassic","Permian","Carboniferous","Devonian","Silurian","Ordovician"))
    n.periods <- length(periods)
    period.mid <- vector(mode="numeric", length=length(periods)-1)
    for (i in 1:(length(periods)-1)) {
      period.mid[i] <- mean(periods[i:(i+1)])
    }
    cambrian <- max(periods)
    if(lab.length==1) {
      period.labs <- c("O", "S", "D", "C", "P", "Tr", "J", "K", "Pg", "Ng")
    } else if (lab.length==2) {
      period.labs <- c("O", "S", "D", "C", "P", "Tr", "J", "K", "Pg", "N")
    } else if (lab.length==3) {
      period.labs <- c("O", "S", "D", "C", "P", "Tr", "J", "K", "P", "N")
    } else if (lab.length==4) {
      period.labs <- c("O", "S", "D", "C", "P", "T", "J", "K", "P", "N")
    }
    my.y <- 1:length(periods)
  } else {
    periods <- rev(c(0.00, 23.03, 66.0, 145.0, 201.3, 252.2, 298.9, 358.9, 419.2, 443.4, 485.4, 541.0))
    names(periods) <- rev(c("Recent","Neogene","Paleogene","Cretaceous","Jurassic","Triassic","Permian","Carboniferous","Devonian","Silurian","Ordovician","Cambrian"))
    n.periods <- length(periods)
    period.mid <- vector(mode="numeric", length=length(periods)-1)
    for (i in 1:(length(periods)-1)) {
      period.mid[i] <- mean(periods[i:(i+1)])
    }
    cambrian <- max(periods)
    if(lab.length==1) {
      period.labs <- c("Cm", "O", "S", "D", "C", "P", "Tr", "J", "K", "Pg", "Ng")
    } else if (lab.length==2) {
      period.labs <- c("Cm", "O", "S", "D", "C", "P", "Tr", "J", "K", "Pg", "N")
    } else if (lab.length==3) {
      period.labs <- c("Cm", "O", "S", "D", "C", "P", "Tr", "J", "K", "P", "N")
    } else if (lab.length==4) {
      period.labs <- c("C", "O", "S", "D", "C", "P", "T", "J", "K", "P", "N")
    }
    my.y <- 1:length(periods)
  }
  
  # set up plot window
  if(!is.na(pdf.name)) {
    pdf(file = pdf.name, height=total.height, width=total.width, bg=bg)
  } else if (!is.na(eps.name)) {
    postscript(file = eps.name, height=total.height, width=total.width, bg=bg)
  } else if (!is.na(tiff.name)) {
    tiff(filename=tiff.name, height=total.height, width=total.width, units = "in", bg = bg, res = 300)
  } else {
    quartz(height=total.height, width=total.width, bg=bg)
  }
  layout(my.layout, widths=width.prop, heights=height.prop)
  #layout.show(length(my.layout))
  par(mar=c(bottom.mar, left.mar, 0, right.mar), mgp=mgp, xaxs=xaxs, yaxs="i", cex=cex, cex.axis=cex.axis, cex.lab=cex.lab)
  
  #plot time scale
  for(i in 1:ncol) {
    if(is.null(all.cols) | is.element(i, all.cols)) {		
      plot(periods, my.y, xlim=rev(range(periods)), xlab="Geologic time (Ma)", ylab="", type="n", axes=FALSE, frame.plot=TRUE)	
      axis(side=1, at=pretty(range(periods)), lwd.ticks=1, lwd=0)
      #lines(x=c(cambrian, 0), y=rep(min(my.y), 2), lwd=1) # horizontal line below time scale
      #lines(x=c(cambrian, 0), y=rep(max(my.y), 2), lwd=1) # horizontal line above time scale
      for(i in 1:n.periods) {
        lines(x=rep.int(periods[i],2),  y=range(periods), lwd=1)	
        text(x=period.mid[i], y=mean(my.y), labels=period.labs[i], cex=cex.intervals)
      }
    } else {
      plot(periods, my.y, xlim=rev(range(periods)), xlab="", ylab="", type="n", axes=FALSE)	
    }
  }
  par(mar=c(0, left.mar, top.mar, right.mar), mgp=mgp, yaxs="r", cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, xaxs=xaxs)	
}

