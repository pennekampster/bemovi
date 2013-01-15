
### This function takes the directory where the video files are stored
### and makes a imagej macro for each of the video.
### At the moment, the macro does image differencing, thresholding, and particle analysis.
MakeIJMacros <- function(vid.dir, difference.lag) {


## To do list:
# DONE... 1. Make the function look at the file type of the videos (either avi or cxd) and then use the appropriate import function.
## ImageJ code for importing cxd (run using fiji, or load the bioformats plugin to imagej):
#

# DONE... 2. Get the number of frames from the video, and use this to create the two substacks, rather than specify the number of frames in advance.

# DONE... 3. Import only once, and use substack to create the lagged videos that are subtracted from each other.
## imagej code for this (needs slices replaced by a variable (c.f., to do item 2 above).
#run("Make Substack...", "  slices=10-125");
#vid2 = getTitle();
#selectWindow(vid1)
#run("Slice Remover", "first=117 last=125 increment=1");
#imageCalculator("Subtract create stack", vid2, vid1);

# DONE... 4. Add properties change to make use of particle tracker possible.
# imagej code (needs number of frames replaced by a variable)
#run("Properties...", "channels=1 slices=1 frames=116 unit=micron pixel_width=1.0000 pixel_height=1.0000 voxel_depth=1.0000 frame=[0.04 sec] origin=0,0");

# 5. Make all user defined values arguments to the function, e.g., subtract lag, threshold values, particle size range).

# 6. Make a check for the operating system and then writes appropriate paths in the imagej macro code.
# At the moment the code can write only for a mac
# In R, .Platform$OS.type returns the operating system, so use if(.Platform$OS.type=="unix")...

	
	## the video file names
	all.files <- dir(path=vid.dir)
	avi.files <- all.files[grep(c("avi"), all.files)]
	cxd.files <- all.files[grep(c("cxd"), all.files)]
	
	vid.files <- c(avi.files, cxd.files)

	for(i in 1:length(vid.files)) {

		vf=vid.files[i]
		ijm.text <- list()

		## Read in the video file
		if(length(grep("avi", vf))>0) {	
			outfile <- strsplit(vf, "avi")[[1]]
			ijm.text[[1]] <- paste("run(\"AVI...\", \"select=[", vid.dir, "/", vf, "]\");", sep="")
			ijm.text[[2]] <- paste("getDimensions(width, height, channels, slices, frames);")
			ijm.text[[3]] <- "run(\"Properties...\", \"channels=1 slices=1 frames=\"+slices+\" unit=pixel pixel_width=1.0000 pixel_height=1.0000 voxel_depth=1.0000 frame=[0 sec] origin=0,0\");"
			
			

		}
		if(length(grep("cxd", vf))>0) {	
			outfile <- strsplit(vf, "cxd")[[1]]
			ijm.text[[1]] <- paste("run(\"Bio-Formats\", \"open=[", vid.dir, "/", vf, "] autoscale color_mode=Default view=[Standard ImageJ] stack_order=Default\");", sep="")
			outfile <- strsplit(vf, "cxd")[[1]]
			ijm.text[[2]] <-  ""
			ijm.text[[3]] <-  ""
						
		}
		ijm.text[[4]] <- c("vid1 = getTitle();")
		ijm.text[[5]] <- paste("getDimensions(width, height, channels, slices, frames);")

		## make substack 1
		ijm.text[[6]] <- paste("run(\"Make Substack...\", \"  slices=", difference.lag, "-\"+frames+\"\");", sep="")
		ijm.text[[7]] <- c("vid2 = getTitle();")
		
		## make substack 2
		ijm.text[[8]] <- c("selectWindow(vid1);")
		ijm.text[[9]] <- paste("run(\"Make Substack...\", \"  slices=1-\"+frames-", difference.lag-1, "+\"\");", sep="")
		ijm.text[[10]] <- c("vid3 = getTitle();")
		
		ijm.text[[11]] <- c("selectWindow(vid1);")
		ijm.text[[12]] <- c("close();")
	
		ijm.text[[13]] <- c("imageCalculator(\"Subtract create stack\", vid2, vid3);")
		ijm.text[[14]] <- c("vid4 = getTitle();")
		
		ijm.text[[15]] <- c("selectWindow(vid2);")
		ijm.text[[16]] <- c("close();")
		ijm.text[[17]] <- c("selectWindow(vid3);")
		ijm.text[[18]] <- c("close();")
	
		ijm.text[[19]] <- c("selectWindow(vid4);")		
		ijm.text[[20]] <- c("setAutoThreshold(\"Default dark\");")
		ijm.text[[21]] <- c("//run(\"Threshold...\");")
		ijm.text[[22]] <- c("//setThreshold(33,255);")
		ijm.text[[23]] <- c("run(\"Convert to Mask\", \" \");")
		ijm.text[[24]] <- c("run(\"Set Measurements...\", \"area min centroid fit shape stack redirect=None decimal=3\");")
		ijm.text[[25]] <- c("run(\"Analyze Particles...\", \"size=20-2000 circularity=0.00-1.00 show=Nothing display clear stack\");")
		ijm.text[[26]] <- paste("saveAs(\"Results\", \"", vid.dir, "/", outfile, "ijout\");", sep="")
		
		ijm.text[[27]] <- c("close();")
		ijm.text[[29]] <- c("run(\"Quit\");")
		
		write(ijm.text[[1]], paste(vid.dir, "/", outfile, "ijm", sep=""))
		for(j in 2:length(ijm.text)) 
			write(ijm.text[[j]], paste(vid.dir, "/", outfile, "ijm", sep=""), sep="", append=T)
		
	}
}

## This function runs the imagej macros that are in the specified directory
RunIJMacros <- function(vid.dir) {

# To do list:
# 1. Make a check for the operating system and then writes appropriate paths in the imagej macro code.
# At the moment the code can write only for a mac

# 2. Have imagej run without popping windows open and closed (perhaps this is called headless?).


	## the macro file names
	all.files <- dir(path=vid.dir)
	ijmac.files <- all.files[grep("ijm", all.files)]
	
	## now make a imagej macro for the first file
	for(i in 1:length(ijmac.files)) {

		cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", vid.dir, "/", ijmac.files[i], "", sep="")
	
		system(cmd)
	
	}
}

## This function gets the output files produced by the imagej macros previously created (by function MakeIJMacros)
## and run by function RunIJMacros
LoadIJOuts <- function(vid.dir) {
	
# To do list:
# 1. Make a check for the operating system and then writes appropriate paths in the imagej macro code.
# At the moment the code can write only for a mac

	
	## the macro file names
	all.files <- dir(path=vid.dir)
	ijout.files <- all.files[grep("ijout", all.files)]
	
	dd <- read.delim(paste(vid.dir, ijout.files[1], sep="//"))
	dd$X <- rep(ijout.files[1], length(dd$X))
	
	for(i in 2:length(ijout.files)) {

		dd.t <- read.delim(paste(vid.dir, ijout.files[i], sep="//"))
		dd.t$X <- rep(ijout.files[i], length(dd.t$X))
		dd <- rbind(dd, dd.t)
	}
	dd
}

