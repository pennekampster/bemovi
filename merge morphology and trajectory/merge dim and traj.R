rm(list=ls())
library(multicore)
library(lattice)


~/work/git/franco/merge morphology and trajectory/example data/trajectory data.csv

tt <- read.csv("~/work/git/franco/merge morphology and trajectory/example data/trajectory data.csv", row.names=1)
# str(tt)
# tt$Trajectory
# tt$Frame
# tt$x
# tt$y


pp <- read.csv("~/work/git/franco/merge morphology and trajectory/example data/part.dims.csv", row.names=1)
# str(pp)
# pp$Slice


## function to match the particles and trajectories in each frame
PTMatch <- function(frame, tt, pp) {
	##f = 127
	f <- frame
	##print(f)
	these.tt <- tt[tt$Frame==f,]
	these.pp <- pp[pp$Slice==f,]
	plot(these.tt[,c("x", "y")], xlim=c(0,2048), ylim=c(0,2048), col="blue")
	points(these.pp[,c("Y")], these.pp[,c("X.1")], col="red")
	mm <- matrix(NA, length(these.tt[,1]), length(these.pp[,1]))
	for(i in 1:length(these.tt[,1]))
		for(j in 1:length(these.pp[,1])) {
			## in the next line it seems there is an error, that x and ys don't match
			## this is not an error...!
			mm[i,j] <- sqrt( (these.tt[i,"x"] - these.pp[j,"Y"])^2 + (these.tt[i,"y"] - these.pp[j,"X.1"])^2)
			##print(c(i,j))
			}
	##mm
	num.parts <- min(c(length(these.tt[,1]), length(these.pp[,1])))
	mm[order(mm)[num.parts]]
	rc <- which(mm<=mm[order(mm)[num.parts]], arr.in=T)
	mf <- cbind(these.tt[rc[1,1],], these.pp[rc[1,2],])
	for(i in 2:length(rc[,1]))
		mf <- rbind(mf, cbind(these.tt[rc[i,1],], these.pp[rc[i,2],]))
	mf
}

## only match where there is are duplicate frames:
these.frames <- sort(c(unique(pp$Slice), unique(tt$Frame))[duplicated(c(unique(pp$Slice), unique(tt$Frame)))])

rez <- lapply(these.frames, function(x, ...) PTMatch(x, tt, pp))

rez <- do.call("rbind", rez)

str(rez)

#### Whoops, I deleted the histograms code, and the code to plot the trajectories with the point size scaled by particle size. (If only I'd been using git!!!)










