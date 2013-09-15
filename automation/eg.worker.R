## This is the code that one runs in order to do the following:
## 1. Run the Particle Analyzer for each of the video files in a specified directory/folder.
## 2. Run the Particle Tracker  for each of the video files in a specified directory/folder.  
## 3. Load into R the data produced by ParticleAnalyzer/ParticleTracker
## 4. Produce trajectory overlays
## 5. Missing: merge extracted data with information on sample (species,magnification, volume used etc.)

rm(list=ls())

require(plyr)

## specify difference.lag for both Particle Analyzer as well as Tracker
difference.lag <- 10
## specify threshold values
thresholds = c(14,255)
## background for stack maxima
##stack.max.background = "light" ## any other values results in dark background

## Owen's paths (should read "OS X, linux, etc paths)
to.code.owen <- "/Users/owenpetchey/Desktop/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Desktop/forshawn/"
to.particlelinker.owen <- to.code.owen

## Frank's paths (should read "Windows paths")
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
sample.description.folder <- "0 - sample description/"
sample.description.file <- "video.description.txt"
raw.video.folder <- "1 - raw/"
raw.checkthreshold.folder <- "1 - raw checkthresh/"
trajectory.data.folder <- "2 - trajectory data/"
overlay.folder <- "3 - overlay plots/"
overlay.folder2 <- "4 - overlays/"
particle.analyzer.folder <- "5 - Particle Analyzer data/"
merge.folder <- "6 - Classification/"
merge.folder <- "6 - merged data"
particle.linker.out <- "9 - Particle Analyzer trajectories"


## set up paths
sample.dir <- paste(to.data.owen, sample.description.folder, sep="")
video.dir <- paste(to.data, raw.video.folder, sep="")
IJ_output.dir <- paste(to.data, particle.analyzer.folder, sep="")
video.dir <- paste(to.data, raw.video.folder, sep="")
trackdata.dir <- paste(to.data, trajectory.data.folder, sep="")
class.dir <- paste(to.data, merge.folder, sep="")


# load functions to call ImageJ from R
source(paste(to.code, "batch_process_videos.r", sep=""), encoding="utf-8")
##source(paste(to.code, "ParticleAnalyzer functions.r", sep="")) ## functions moved to batch_process_vids source file
#source(paste(to.code, "merge.r", sep=""))
source(paste(to.code, "movement_analysis.r", sep=""), encoding="utf-8")



## put the sample info into the dataset
sample.description <- read.table(paste0(to.data.frank,sample.description.folder,"frank.video.description.txt"), header=TRUE, sep="\t")
# vessel.vol.used as ml volume used in 30ml flasks
sample.description$vessel.vol.used <- gsub("30ml/","",sample.description$vessel.vol.used)
sample.description$vessel.vol.used <- gsub("ml","",sample.description$vessel.vol.used)

	

## check for unsupported file types, and for periods in the file name
Check.video.files(video.dir)

Check_threshold(video.dir,difference.lag, thresholds)

# run Particle Analyzer and merge result files into morphology database
# specify directory, difference.lag, and thresholds
## particle analyser needs white background
video_to_morphology(video.dir, difference.lag, thresholds)
LoadIJ_morph_outs(IJ_output.dir)

# convert XY coordinates from ParticleAnalyzer into data, which is read by the standalone Particle
# linker and output as text file
PA_output_dir <- paste0(to.data, particle.analyzer.folder)
traj_out.dir <- paste0(to.data, particle.linker.out)
convert_PA_to_traject(PA_output_dir, traj_out.dir)


# run ParticleTracker, merge results and produce overlays
# video_to_trajectory(video.dir, difference.lag, thresholds, stack.max.background)
 
# merge trajectory data into database
# specify directory
# LoadIJ_Traj_Outs(trackdata.dir)

#same for ParticleLinker
PA_dir <- paste0(to.data, particle.linker.out)
traj_out.dir <- paste0(to.data, trajectory.data.folder)
merge_PA_results(PA_dir, traj_out.dir)


# create overlay videos
if(.Platform$OS.type == "windows"){
  width <- 1024
  height <- 768}
if(.Platform$OS.type == "unix"){
  width <- 2048
  height <- 2048}
#specify directory
type = "label" ## other type is "traj"
create_overlay_plots(trackdata.dir,
	width,
	height,
	difference.lag,
	type=type,
	original.vid.contrast.enhancement = 1.0
)



#### Suggested workflow from here:

## merge the morphology data, the trajectory data, and the video description data without any pre-processing
merge_morphology_trajectory_expt_data(to.data, particle.analyzer.folder, trajectory.data.folder, merge.folder,
								sample.dir, sample.description.file)





# filter trajectories to exclude artefacts and very short trajects which do not allow proper extraction
# of all relevant movement features (e.g. periodic patterns in turning angles)
filter_trajects(trajectory.data)

# extract movement metrics and save to classification folder
extract_movement(trajectory.data)

# merge mophology of ParticleAnalyzer on the filtered trajectories for classification
trajectory_morphology(trajectory.data)

# Classification




