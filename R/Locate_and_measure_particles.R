#' Function to extract morphological measurements and X- and Y-coordinates for moving particles 
#' 
#' Function calls ImageJ software and its ParticleAnalyzer function to extract for each frame of the video
#' several morphological descriptors and the X- and Y-coordinates of all moving particles
#' @param path Path to the raw videos to be treated
#' @param particle.data.folder Directory to which the data is saved as a text file
#' @param difference_lag Numeric value specifying the offset between two video frames to 
#' compute the difference image
#' @param thresholds Numeric vector containing the min and max threshold values
#' @param memory Numeric value specifying the amount of memory available to ImageJ
#' @return Saves the output of the ParticleAnalyzer function of ImageJ as a text file in the output directory
#' @export 
Locate_and_measure_particles <- function(to.data, raw.video.folder, particle.data.folder, difference.lag, thresholds = c(0, 
                                                                                                                         1000), memory = memory.alloc) {
  
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  
  ## copy master copy of ImageJ macro there for treatment
  text <- readLines(paste(to.code, "ImageJ macros/Video_to_morphology.ijm", sep = ""))
  
  ## use regular expression to insert input & output directory as well as difference lag
  text[grep("avi_input = ", text)] <- paste("avi_input = ", "'", video.dir, "';", sep = "")
  text[grep("avi_output = ", text)] <- paste("avi_output = ", "'", sub("1 - raw/", particle.data.folder, video.dir), 
                                             "';", sep = "")
  text[grep("lag = ", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
  
  ## re-create ImageJ macro for batch processing of video files with Particle Analyzer
  if (.Platform$OS.type == "windows") 
    writeLines(text, con = paste("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm", sep = ""), sep = "\n")
  if (.Platform$OS.type == "unix") {
    dir.create(sub(raw.video.folder, ijmacs.folder, video.dir), showWarnings = F)
    writeLines(text, con = paste(sub(raw.video.folder, ijmacs.folder, video.dir), "Video_to_morphology_tmp.ijm", sep = ""))
  }
  
  ## create directory to store Particle Analyzer data
  dir.create(sub(raw.video.folder, particle.data.folder, video.dir), showWarnings = FALSE)
  
  ## run to process video files by calling ImageJ
  if (.Platform$OS.type == "unix") 
    cmd <- paste0("java -Xmx", memory, "m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", 
                  paste0(sub("1 - raw", "ijmacs", video.dir), "Video_to_morphology_tmp.ijm"))
  if (.Platform$OS.type == "windows") 
    cmd <- c("\"C:/Program Files/FIJI.app/fiji-win64.exe\" -macro Video_to_morphology_tmp.ijm")
  system(cmd)
  
  ## delete temporary file after execution
  if (.Platform$OS.type == "windows") 
    file.remove("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm")
  
}
