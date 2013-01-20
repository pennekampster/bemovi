## This is the code that one runs in order to do the following:
## 1. Run the Particle Analyzer for each of the video files in a specified directory/folder.
## 2. Run the Particle Tracker  for each of the video files in a specified directory/folder.  
## 3. Load into R the data produced by ParticleAnalyzer/ParticleTracker
## 4. Produce trajectory overlays
## 5. Missing: merge extracted data with information on sample (species,magnification, volume used etc.)

rm(list=ls())

## specify difference.lag for both Particle Analyzer as well as Tracker
difference.lag <- 25

# load functions to call ImageJ from R
if(.Platform$OS.type == "unix"){
  source('~/work/git/franco/automation/batch_process_videos.r.r')
  source('~/work/git/franco/automation/ParticleAnalyzer functions.r')}
if(.Platform$OS.type == "windows"){
  source('C:/Users/Frank/Documents/PhD/Programming/franco/automation/batch_process_videos.r')
  source('C:/Users/Frank/Documents/PhD/Programming/franco/automation/ParticleAnalyzer functions.r')}

# read the file that gives the important information about each video
if(.Platform$OS.type == "windows"){
  sample.dir <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/0 - sample description/"}
if(.Platform$OS.type == "unix"){
  sample.dir <- "/Users/owenpetchey/work/git/franco/data/0 - sample description/"}
#specify filename
file.sample.info <- read.table(paste(sample.dir, "frank.video.description.txt", sep=""), sep= "\t", header = TRUE)


# run Particle Analyzer and merge result files into morphology database
if(.Platform$OS.type == "windows"){
   video.dir <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/"
   IJ_output.dir <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/5 - Particle Analyzer data/"}
if(.Platform$OS.type == "unix"){
   video.dir <- "~/work/git/franco/data/1 - raw/"
   IJ_output.dir <- "~/work/git/franco/data/5 - Particle Analyzer data/"}
#specify directory and difference.lag
MakeIJMacros(video.dir,difference.lag)
LoadIJOuts(IJ_output.dir)


# run ParticleTracker, merge results and produce overlays
if(.Platform$OS.type == "windows"){
  video.dir <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/"}
if(.Platform$OS.type == "unix"){
  video.dir <- "/Users/owenpetchey/work/git/franco/data/1 - raw/"}
#specify directory
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


