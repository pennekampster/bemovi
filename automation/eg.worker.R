## This is the code that one runs in order to do the following:
## 1. Run the Particle Analyzer for each of the video files in a specified directory/folder.
## 2. Run the stand alone particle linker  for each of the video files in a specified directory/folder.  
## 3. Load into R the data produced by ParticleAnalyzer/ParticleTracker
## 4. Produce trajectory overlays
## 5. Merge extracted data with information on sample (species, magnification, volume used etc.)

rm(list=ls())


##require(plyr)


## specify difference.lag for both Particle Analyzer as well as Tracker
difference.lag <- 10
## specify threshold values
thresholds = c(10,255)


## Owen's paths (could read "OS X, linux, etc paths)
to.code.owen <- "/Users/owenpetchey/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Dropbox/testvids/"
to.particlelinker.owen <- to.code.owen


## Frank's paths (could read "Windows paths")
to.code.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/automation/"
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"
to.particlelinker.frank <-"C:/Users/Frank/Documents/PhD/Programming/franco/data/"


## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
	to.code <- to.code.frank
	to.data <- to.data.frank
	to.particlelinker <- to.particlelinker.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
	to.code <- to.code.owen
	to.data <- to.data.owen
	to.particlelinker <- to.particlelinker.owen}


## General folders
ijmacs.folder <- "ijmacs/"
video.description.folder <- "0 - video description/"
video.description.file <- "video.description.txt"
raw.video.folder <- "1 - raw/"
particle.data.folder <- "2 - particle data/"
trajectory.data.folder <- "3 - trajectory data/"
temp.overlay.folder <- "4a - temp overlays/"
overlay.folder <- "4 - overlays/"
merged.data.folder <- "5 - merged data/"


# load functions to call ImageJ from R
source(paste(to.code, "batch_process_videos.r", sep=""), encoding="utf-8")
##source(paste(to.code, "ParticleAnalyzer functions.r", sep="")) ## functions moved to batch_process_vids source file
#source(paste(to.code, "merge.r", sep=""))
source(paste(to.code, "movement_analysis.r", sep=""), encoding="utf-8")

	

## check for unsupported file types, and for periods in the file name
Check_video_file_names(to.data, raw.video.folder)


## Creates a imagej macro that can be run in imagej to check threshold values
Check_threshold_values(to.data, raw.video.folder,
					   difference.lag,
					   thresholds)


# run Particle Analyzer and merge result files into morphology database
# specify directory, difference.lag, and thresholds
## particle analyser needs white background
Locate_and_measure_particles(to.data, raw.video.folder, particle.data.folder,
					   		 difference.lag,
					   		 thresholds)
Organise_particle_data(to.data, particle.data.folder)


## convert XY coordinates from ParticleAnalyzer into data, which is read by the standalone Particle
## linker and output as text file
Link_particles(to.data, particle.data.folder, trajectory.data.folder)
Organise_link_data(to.data, trajectory.data.folder)


# create overlay videos
if(.Platform$OS.type == "windows"){
  width <- 1024
  height <- 768}
if(.Platform$OS.type == "unix"){
  width <- 2048
  height <- 2048}
#specify directory
type = "label" ## other type is "traj"
Create_overlay_videos(to.data, trajectory.data.folder, raw.video.folder,
	temp.overlay.folder, overlay.folder,
	width,
	height,
	difference.lag,
	type=type,
	original.vid.contrast.enhancement = 2.0)


## merge the morphology data, the trajectory data,
## and the video description data without any pre-processing
Merge_particle_link_experiment_data(to.data,
									particle.data.folder,
									trajectory.data.folder,
									video.description.folder,
									video.description.file,
									merged.data.folder)


## checked to here...


# filter trajectories to exclude artefacts and very short trajects which do not allow proper extraction
# of all relevant movement features (e.g. periodic patterns in turning angles)
filter_trajects(trajectory.data)


# extract movement metrics and save to classification folder
extract_movement(trajectory.data)


# merge mophology of ParticleAnalyzer on the filtered trajectories for classification
trajectory_morphology(trajectory.data)



# Classification




