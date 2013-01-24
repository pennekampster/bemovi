## This is the code that one runs in order to do the following:
## 1. Run the Particle Analyzer for each of the video files in a specified directory/folder.
## 2. Run the Particle Tracker  for each of the video files in a specified directory/folder.  
## 3. Load into R the data produced by ParticleAnalyzer/ParticleTracker
## 4. Produce trajectory overlays
## 5. Missing: merge extracted data with information on sample (species,magnification, volume used etc.)

rm(list=ls())

## specify difference.lag for both Particle Analyzer as well as Tracker
difference.lag <- 25


## Owen's paths
to.code.owen <- "/Users/owenpetchey/work/git/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Desktop/franco.test.vids/"

## Frank's paths
to.code.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/automation/"
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"

## General folders
sample.description.folder <- "0 - sample description/"
sample.description.file <- "frank.video.description.txt"
raw.video.folder <- "1 - raw/"
particle.analyzer.folder <- "5 - Particle Analyzer data/"



## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
	to.code <- to.code.frank
	to.data <- to.data.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
	to.code <- to.code.owen
	to.data <- to.data.owen}
	

# load functions to call ImageJ from R
source(paste(to.code, "batch_process_videos.r", sep=""))
source(paste(to.code, "ParticleAnalyzer functions.r", sep=""))


# read the file that gives the important information about each video
sample.dir <- paste(to.data, sample.description.folder, sep="")
file.sample.info <- read.table(paste(sample.dir, sample.description.file, sep=""), sep= "\t", header = TRUE)


# run Particle Analyzer and merge result files into morphology database
video.dir <- paste(to.data, raw.video.folder, sep="")
IJ_output.dir <- paste(to.data, particle.analyzer.folder, sep="")
#specify directory and difference.lag
MakeIJMacros(video.dir,difference.lag)
LoadIJOuts(IJ_output.dir)


# run ParticleTracker, merge results and produce overlays
video.dir <- paste(to.data, raw.video.folder, sep="")
video_to_trajectory(video.dir,difference.lag)


# merge trajectory data into database
if(.Platform$OS.type == "windows"){
  trackdata.dir <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/"}
if(.Platform$OS.type == "unix"){
  trackdata.dir <- "/Users/owenpetchey/work/git/franco/data/2 - trajectory data/"}
#specify directory
LoadIJ_Traj_Outs(trackdata.dir)

# create overlay videos
if(.Platform$OS.type == "windows"){
  trackdata.dir <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/"
  width <- 735
  height <- 690}
if(.Platform$OS.type == "unix"){
  trackdata.dir <- "/Users/owenpetchey/work/git/franco/data/2 - trajectory data/"
  width <- 2048
  height <- 2048}
#specify directory
create_overlay_plots(trackdata.dir,width,height,difference.lag)

'''
## NOT WORKING PROPERLY FROM HERE ONWARDS
## put the sample info into the dataset

#keep only filename no prefix or suffix
trajectory.data$file <- gsub(".avi.txt","",gsub("Traj_","",trajectory.data$file))

trajectory.data$magnification <- file.sample.info$mag[match(toupper(trajectory.data$file), toupper(file.sample.info$video))]
trajectory.data$vol <- file.sample.info$vessel.vol.used[match(toupper(trajectory.data$file), toupper(file.sample.info$video))]
trajectory.data$vol <-  as.numeric(gsub("30/","",gsub("ml","",trajectory.data$vol)))
trajectory.data$real.density <- file.sample.info$real.density[match(toupper(trajectory.data$file), toupper(file.sample.info$video))]


dd1 <- aggregate(dd$X, list(file=dd$X, Slice=dd$Slice, volume=dd$volume, magnification=dd$magnification, real.density=dd$real.density), length)
str(dd1)
dd1
dd1$est <- dd1$x * dd1$magnification
dd1$est <- dd1$est / dd1$volume
boxplot(est ~ as.numeric(as.factor(dd1$file)), dd1, cex.axis=0.4)
points(real.density ~ as.numeric(as.factor(dd1$file)), dd1, pch=21, bg="red")
dd1$real.density


