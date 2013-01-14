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

# function to plot trajectories for overlay (must be merged with original video by ImageJ macro)
# creates a folder containing one jpeg plot containing all the positions till the respective frame
# for the moment colour not assigned by species identity but that's easy to add 
create_overlay_plots <- function(path,duration){ #should be taken automatically from the file containing the position and frame data
trajectory.data <- as.data.frame(read.table(paste(path,"trajectory.data.txt", sep = ""), header = TRUE, sep = "\t"))
file_names <- unique(trajectory.data$file)  
for (i in 1:length(file_names)){
dir.create(paste(path,substr(file_names[i],6,11),sep="/"))
duration <- duration
trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
j<- 0
while(j < duration){
jpeg(paste(path,substr(file_names[i],6,11),"/frame_",j,".jpg",sep=""), width = 2048, height = 2048, quality = 100)
par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame <= j, select=c("X","Y","trajectory"))
plot(print$Y, print$X+2048, xlim=c(0,2048), ylim=c(0,2048), col="#FFFF00", pch=15, cex=1, asp=1)
dev.off()
j <- j+1}}}

create_overlay_plots("C:/Users/Frank/Documents/PhD/Programming/franco/videos/",101)