#' Function to check the thresholds used in the ImageJ macro
#' 
#' This function creates a image j macro that can be helpful for checking the thresholds
#' @param path Path to the raw video files to be treated
#' @param difference_lag Numeric value specifying the offset between two video frames to 
#' compute the difference image
#' @param thresholds Numeric vector containing the min and max threshold values
#' @return Null
#' @export
Check_threshold_values <- function(to.data, raw.video.folder, difference.lag, thresholds = c(10, 255)) {
  
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  ## generate the folders if not already existing
  ijmacs.folder <- sub(raw.video.folder, ijmacs.folder, video.dir)
  dir.create(ijmacs.folder, showWarnings = FALSE)
  
  ## copy master copy of ImageJ macro there for treatment
  text <- readLines(paste(to.code, "ImageJ macros/Check_threshold.ijm", sep = ""))
  
  ## use regular expression to insert input and output directory
  text[grep("avi_input =", text)] <- paste("avi_input = ", "'", video.dir, "';", sep = "")
  ## text[grep('avi_output =', text)] <- paste('avi_output = ',''',checkthresh.folder,'';', sep = '')
  text[grep("lag =", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
  
  ## re-create ImageJ macro for batch processing of video files with ParticleTracker perhaps put this in a subdirectory of
  ## the data folder?  This is implemented in OSX but not windows, which is as you wrote it
  if (.Platform$OS.type == "windows") 
    writeLines(text, con = paste("C:/Program Files/Fiji.app/macros/Check_threshold_tmp.ijm", sep = ""), sep = "\n")
  if (.Platform$OS.type == "unix") 
    writeLines(text, con = paste(ijmacs.folder, "Check_threshold_tmp.ijm", sep = ""))
} 


