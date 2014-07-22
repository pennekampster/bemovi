#' Function to adjust the thresholds used in the ImageJ macro
#' 
#' This function creates an ImageJ macro that can be helpful for checking the thresholds; the macro will be saved in the ImageJ macro directory in the working directory and
#' has to be opened in ImageJ manually. After running the macro, the video stack will be opened until the thresholding step. By playing with the treshold manually, the correct
#' value is determined and can be provided to the R script.
#' 
#' @param to.data path to the working directory
#' @param raw.video.folder directory with the raw video files 
#' @param ijmacs.folder directory where the check_trehsolds macro is saved
#' @param difference_lag numeric value specifying the offset between two frames of a video
#' @param thresholds numeric vector containing the min and max threshold values (i.e. between 0 and 255 given that videos should be 8-bit grey scale)
#' @export

check_threshold_values <- function(to.data, raw.video.folder, ijmacs.folder, difference.lag, thresholds = c(10, 255)) {
  
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  ## generate the folders if not already existing
  #ijmacs.folder <- sub(raw.video.folder, ijmacs.folder, video.dir)
  dir.create(ijmacs.folder, showWarnings = FALSE)
  
  ## copy master copy of ImageJ macro there for treatment
  text <- readLines(paste(system.file(package="fRanco"), "/", "ImageJ macros/Check_threshold.ijm", sep = ""))
  
  ## use regular expression to insert input and output directory
  text[grep("avi_input =", text)] <- paste("avi_input = ", "'", video.dir, "';", sep = "")
  text[grep("lag =", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
  
  ## re-create ImageJ macro for batch processing of video files with ParticleTracker perhaps put this in a subdirectory of
  ## the data folder?  This is implemented in OSX but not windows, which is as you wrote it
  if (.Platform$OS.type == "windows") 
    writeLines(text, con = paste(to.data,ijmacs.folder,"Check_threshold_tmp.ijm", sep = ""), sep = "\n")
  if (.Platform$OS.type == "unix") 
    writeLines(text, con = paste(to.data,ijmacs.folder, "Check_threshold_tmp.ijm", sep = ""))
} 


