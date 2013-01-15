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
# provide path of " 2 - trajectory data"
create_overlay_plots <- function(path){ 
trajectory.data <- as.data.frame(read.table(paste(path,"trajectory.data.txt", sep = ""), header = TRUE, sep = "\t"))
file_names <- unique(trajectory.data$file)  
# change path for output
dir.create(sub("2 - trajectory data/","3 - overlay plots/",path))
for (i in 1:length(file_names)){
dir.create(paste(sub("2 - trajectory data/","3 - overlay plots/",path),substr(file_names[i],6,11),sep="/"))
trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
j<- 0
while(j < max(trajectory.data$frame)+1){
jpeg(paste(sub("2 - trajectory data/","3 - overlay plots/",path),substr(file_names[i],6,11),"/frame_",j,".jpg",sep=""), width = 735, height = 690, quality = 100)
par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame <= j, select=c("X","Y","trajectory"))
plot(print$Y, print$X+690, xlim=c(0,735), ylim=c(0,690), col="#FFFF00", pch=15, cex=1, asp=1)
dev.off()
j <- j+1}}

# copy master copy of ImageJ macro there for treatment
text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_overlay.ijm")

# use regular expression to insert input and output directory
text[3] <- sub(text, "avi_input = ", paste("avi_input = ","'", sub("2 - trajectory data/","1 - raw/",path),"';", sep = ""))
text[4] <- sub(text, "overlay_input = ", paste("overlay_input = ","'", sub("2 - trajectory data/","3 - overlay plots/",path),"';", sep = ""))
text[5] <- sub(text, "overlay_output = ", paste("overlay_output = ","'", sub("2 - trajectory data/","4 - overlays/",path),"';", sep = ""))

# re-create ImageJ macro for batch processing of video files with ParticleTracker
writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm",sep=""),sep="\n")

# create directory to store overlays
dir.create(sub("2 - trajectory data/","4 - overlays/",path))

#call IJ macro to merge original video with the trajectory data
if(.Platform$OS.type == "unix"){
  cmd <- c("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ")}
if(.Platform$OS.type == "windows"){
  cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_overlay_tmp.ijm')}
system(cmd)

}

create_overlay_plots("C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/")