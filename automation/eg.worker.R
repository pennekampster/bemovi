## This is the code that one runs in order to do the following:
## 1. Run the Particle Analyzer for each of the video files in a specified directory/folder.
## 2. Run the Particle Tracker  for each of the video files in a specified directory/folder.  
## 3. Load into R the data produced by ParticleAnalyzer/ParticleTracker
## 4. Produce trajectory overlays
## 5. Missing: merge extracted data with information on sample (species,magnification, volume used etc.)

rm(list=ls())

## specify difference.lag for both Particle Analyzer as well as Tracker
difference.lag <- 10
## specify threshold values
thresholds = c(800,7245)
## background for stack maxima
stack.max.background = "light" ## any other values results in dark background

## Owen's paths
to.code.owen <- "/Users/owenpetchey/work/git/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Desktop/hard.test/"

## Frank's paths
to.code.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/automation/"
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"

## General folders
sample.description.folder <- "0 - sample description/"
sample.description.file <- "frank.video.description.txt"
raw.video.folder <- "1 - raw/"
raw.checkthreshold.folder <- "1 - raw checkthresh/"
trajectory.data.folder <- "2 - trajectory data/"
overlay.folder <- "3 - overlay plots/"
overlay.folder2 <- "4 - overlays/"
particle.analyzer.folder <- "5 - Particle Analyzer data/"
merge.folder <- "6 - Classification/"
particle.linker.out <- "9 - Particle Analyzer trajectories"



## put the sample info into the dataset
sample.description <- read.table(paste0(to.data,sample.description.folder,"frank.video.description.txt"), header=TRUE, sep="\t")
# vessel.vol.used as ml volume used in 30ml flasks
sample.description$vessel.vol.used <- gsub("30ml/","",sample.description$vessel.vol.used)
sample.description$vessel.vol.used <- gsub("ml","",sample.description$vessel.vol.used)

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
	
## set up paths
sample.dir <- paste(to.data, sample.description.folder, sep="")
video.dir <- paste(to.data, raw.video.folder, sep="")
IJ_output.dir <- paste(to.data, particle.analyzer.folder, sep="")
video.dir <- paste(to.data, raw.video.folder, sep="")
trackdata.dir <- paste(to.data, trajectory.data.folder, sep="")
class.dir <- paste(to.data, merge.folder, sep="")

# load functions to call ImageJ from R
source(paste(to.code, "batch_process_videos.r", sep=""))
source(paste(to.code, "ParticleAnalyzer functions.r", sep=""))
source(paste(to.code, "movement_analysis.r", sep=""))
source(paste(to.code, "merge.r", sep=""))

# read the file that gives the important information about each video
file.sample.info <- read.table(paste(sample.dir, sample.description.file, sep=""), sep= "\t", header = TRUE)

## check for unsupported file types, and for periods in the file name
Check.video.files(video.dir)

##Check_threshold(video.dir,difference.lag, thresholds)

# run Particle Analyzer and merge result files into morphology database
# specify directory, difference.lag, and thresholds
## particle analyser needs white background
video_to_morphology(video.dir,difference.lag,thresholds)
LoadIJ_morph_outs(IJ_output.dir)

# convert XY coordinates from ParticleAnalyzer into data, which is read by the standalone Particle
# linker and output as text file
convert_PA_to_traject(paste0(to.data.frank,particle.analyzer.folder),paste0(to.data.frank,particle.linker.out))

# run ParticleTracker, merge results and produce overlays
video_to_trajectory(video.dir, difference.lag, thresholds, stack.max.background)


# merge trajectory data into database
#specify directory
LoadIJ_Traj_Outs(trackdata.dir)

#same for ParticleLinker
merge_PA_results(paste0(to.data.frank,particle.linker.out),paste0(to.data.frank,trajectory.data.folder))

# create overlay videos
if(.Platform$OS.type == "windows"){
  width <- 735
  height <- 690}
if(.Platform$OS.type == "unix"){
  width <- 2048
  height <- 2048}
#specify directory
create_overlay_plots(trackdata.dir,width,height,difference.lag,type='traj')

# Feature extraction from data files

#load trajectory data
trajectory.data <- read.table(paste0(to.data,trajectory.data.folder,"trajectory.data.txt"), header=TRUE, sep="\t")

# filter trajectories to exclude artefacts and very short trajects which do not allow proper extraction
# of all relevant movement features (e.g. periodic patterns in turning angles)
filter_trajects(trajectory.data)

# extract movement metrics and save to classification folder
extract_movement(trajectory.data)

# merge mophology of ParticleAnalyzer on the filtered trajectories for classification
trajectory_morphology(trajectory.data)

# Classification




