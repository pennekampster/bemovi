#' Function to extract morphological measurements and X- and Y-coordinates for moving particles 
#' 
#' Function calls ImageJ software and its ParticleAnalyzer function to extract for each frame of the video
#' several morphological descriptors and the X- and Y-coordinates of all moving particles. All videos in the raw.video.folder are analyses, separately.
#' @param to.data path to the working directory
#' @param raw.video.folder directory with the raw video files 
#' @param particle.data.folder directory to which the data is saved as a text file
#' @param difference.lag numeric value specifying the offset between two video frames to 
#' compute the difference image. If 0, then no differencing applied.
#' @param min_size minimum size for detection of particles
#' @param max_size maximum size for detection of particles
#' @param thresholds vector containing the min and max threshold values (defaults to c(10,255))
#' @param IJ.path path to ImageJ folder, containing the 'ij.jar' executable
#' @param memory numeric value specifying the amount of memory available to ImageJ (defaults to 512)
#' @param ijmacs.folder directory for the macro to for ImageJ 
#' @param pixel_to_scale TODO
#' 
#' @return saves the output of the ParticleAnalyzer function of ImageJ as a text file in the output directory and then assembles the data into a single database 
#' called 'particle.rds'. This data.frame contains information about the following properties: the area (transversal cut), 
#' the mean, minimum and maximum of the grey value, the perimeter, width, length and angle with the dominant-axis of a fitted ellipse, and finally shape parameters such as
#' circularity, aspect ratio, roundness and solidity. For details of the morphological output, please refer to http://rsbweb.nih.gov/ij/docs/guide/146-30.html 
#' @export 

locate_and_measure_particles <- function(
  to.data = par_to.data(), 
  raw.video.folder = par_raw.video.folder(), 
  particle.data.folder = par_particle.data.folder(), 
  difference.lag = par_difference.lag(), 
  min_size = par_min_size(), 
  max_size = par_max_size(), 
  thresholds = par_thresholds(), 
  IJ.path = par_IJ.path(), 
  memory = par_memory(),
  ijmacs.folder = par_ijmacs.folder(),
  pixel_to_scale = par_pixel_to_scale()
) {
  
  #ijmacs.folder<-NULL
  
  video.dir <- file.path(to.data, raw.video.folder)
  
  ## copy master copy of ImageJ macro there for treatment
  ## if there is differencing (i.e., difference.lag>0)
  if (difference.lag > 0) {
    text <- readLines(file.path(system.file(package = "bemovi"), "ImageJ_macros", "Video_to_morphology.ijm"))
  }
  ## if there is no differencing (i.e., difference.lag==0)
  if (difference.lag == 0) {
    text <- readLines(file.path(system.file(package = "bemovi"), "ImageJ_macros", "Video_to_morphology_no_differencing.ijm"))
  }
  ## use regular expression to insert input & output directory as well as difference lag
  text[grep("video_input = ", text)] <- paste("video_input = ", "'", video.dir, "/';", sep = "")
  text[grep("video_output = ", text)] <- paste("video_output = ", "'", file.path(to.data, particle.data.folder), "/';", sep = "")
  text[grep("lag = ", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
  text[grep("size=", text)] <- paste('run("Analyze Particles...", "size=',min_size,'-',max_size,' circularity=0.00-1.00 show=Nothing clear stack");',sep = "")
  
  ## re-create ImageJ macro for batch processing of video files with Particle Analyzer
  # if (.Platform$OS.type == "windows") {
  dir.create(file.path(to.data, ijmacs.folder), showWarnings = FALSE)
  writeLines(text, con = file.path(to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm")) #}
# if (.Platform$OS.type == "unix") {
#   dir.createfile.path(to.data, ijmacs.folder), showWarnings = FALSE)
#   writeLines(text, con = paste(to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm"))}

## create directory to store Particle Analyzer data
  dir.create(file.path(to.data, particle.data.folder), showWarnings = FALSE)

## run to process video files by calling ImageJ

cmd <- switch(
  Sys.info()[['sysname']],
  Window = paste0(
    file.path( IJ.path ),
    " -macro ",  file.path(to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm")
  ),
  Linux  = paste0(
    "java", 
    " -Xmx", memory, "m ", 
    " -jar ", file.path( IJ.path, "ij.jar"), 
    " -ijpath ", IJ.path, 
    " -macro '", file.path(to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm"), "'"
  ),
  Darwin = paste0(
    # "java", 
    # " -Xmx", memory, "m ", 
    # " -jar ", file.path( IJ.path, "ij.jar"), 
    # " -ijpath ", IJ.path, 
    file.path( IJ.path, "ImageJ-macosx"),
    " --headless",
    " -macro '", file.path(to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm"), "'"
  ),
  stop( "Unsupported Platform!" )
)

system(cmd)

organise_particle_data(
  to.data = to.data, 
  particle.data.folder = particle.data.folder,
  pixel_to_scale = pixel_to_scale
)

}
