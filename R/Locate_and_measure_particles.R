#' Function to extract morphological measurements and X- and Y-coordinates for moving particles 
#' 
#' Function calls ImageJ software and its ParticleAnalyzer function to extract for each frame of the video
#' several morphological descriptors and the X- and Y-coordinates of all moving particles. All videos in the raw.video.folder are analyses, separately.
#' @param to.data path to the working directory
#' @param raw.video.folder directory with the raw video files 
#' @param particle.data.folder directory to which the data is saved as a text file
#' @param difference.lag numeric value specifying the offset between two video frames to 
#' compute the difference image
#' @param min_size minimum size for detection of particles
#' @param max_size maximum size for detection of particles
#' @param thresholds vector containing the min and max threshold values (defaults to c(10,255))
#' @param IJ.path path to ImageJ executable 
#' @param memory numeric value specifying the amount of memory available to ImageJ (defaults to 512)
#' @return saves the output of the ParticleAnalyzer function of ImageJ as a text file in the output directory and then assembles the data into a single database 
#' called 'particle.RData'. This data.frame contains information about the following properties: the area (transversal cut), 
#' the mean, minimum and maximum of the grey value, the perimeter, width, length and angle with the dominant-axis of a fitted ellipse, and finally shape parameters such as
#' circularity, aspect ratio, roundness and solidity. For details of the morphological output, please refer to http://rsbweb.nih.gov/ij/docs/guide/146-30.html 
#' @export 

locate_and_measure_particles <- function(to.data, raw.video.folder, particle.data.folder, difference.lag, min_size=0, max_size=10000, 
thresholds = c(10, 255), IJ.path, memory = 512) {
  
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  
  ## copy master copy of ImageJ macro there for treatment
  text <- readLines(paste0(system.file(package="fRanco"), "/", "ImageJ_macros/Video_to_morphology.ijm"))
  
  ## use regular expression to insert input & output directory as well as difference lag
  text[grep("avi_input = ", text)] <- paste("avi_input = ", "'", video.dir, "';", sep = "")
  text[grep("avi_output = ", text)] <- paste("avi_output = ", "'", to.data, particle.data.folder, "';", sep = "")
  text[grep("lag = ", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
  text[grep("size=", text)] <- paste('run("Analyze Particles...", "size=',min_size,'-',max_size,' circularity=0.00-1.00 show=Nothing clear stack");',sep = "")
    
  ## re-create ImageJ macro for batch processing of video files with Particle Analyzer
  if (.Platform$OS.type == "windows") {
    dir.create(paste0(to.data, ijmacs.folder), showWarnings = F)
    
    writeLines(text, con = paste(to.data,ijmacs.folder,"Video_to_morphology_tmp.ijm", sep = ""))}
  if (.Platform$OS.type == "unix") {
    dir.create(paste0(to.data, ijmacs.folder), showWarnings = F)
    writeLines(text, con = paste0(to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm"))}
  
  ## create directory to store Particle Analyzer data
  dir.create(paste0(to.data, particle.data.folder), showWarnings = FALSE)
  
  ## run to process video files by calling ImageJ
  if (.Platform$OS.type == "unix") 
    cmd <- paste0("java -Xmx", memory, "m -jar ", IJ.path," -ijpath /Applications/ImageJ -macro ","'", 
                  to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm'")
  if (.Platform$OS.type == "windows")
    cmd <- paste0("\"", IJ.path, "\""," -macro ","\"", paste0(gsub("/", "\\\\", paste0(to.data, ijmacs.folder))), "Video_to_morphology_tmp.ijm", "\"")
  
  system(cmd)
  
  organise_particle_data(to.data, particle.data.folder)
  
}
