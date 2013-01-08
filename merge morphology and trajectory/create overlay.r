rm(list=ls())


## I was trying to create a video overlay, and also hoped to merge this with the actual video in R.
## Ideally we would also add to this the species ID assigned to each particle.


## I didn't get this approach to work yet...
library(animation)
oopts = ani.options(ffmpeg = "/Applications/ffmpeg",
				outdir = "...")
ani.options()
saveVideo({
    par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, 
        cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
    ani.options(interval = 0.05, nmax = 300)
    brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, video.name = "BM.avi", other.opts = "-b 300k")



## So I created a pdf of the trajectories and made it transparent in the Keynote presentation.
## Very clunky, but it worked.
dd <- read.csv("~/work/git/franco/merge morphology and trajectory/example data/trajectory data.csv")
str(dd)
pdf("~/Desktop/test1overlay.pdf")
par(mar=c(0,0,0,0))
plot(dd$y, dd$x,
	xlim=c(0,2048), ylim=c(2048,0),
	col=dd$Trajectory,
	pch=19, cex=0.5)
dev.off()