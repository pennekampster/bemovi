#' Function to adjust the thresholds used for the segmentation of the difference video. If you run fRanco for the first time, verify with this function that all target objects are
#' properly identified
#'  
#' This function creates an ImageJ macro that can be helpful for checking the thresholds specified in the user section; the macro will be saved in the ImageJ macro directory in the working directory and
#' then automatically opened in ImageJ. Depending on the video size, it might take some time to open the video and the thresholding menu. First, the tresholds are set to the values 
#' given in the user section, but by playing with the treshold manually, a correct value is determined and if need be changed in the user section.
#' 
#' @param to.data path to the working directory
#' @param raw.video.folder directory with the raw video files 
#' @param ijmacs.folder directory where the check_trehsolds macro is saved
#' @param vid_select video selected to find appropriate thresholds; default is the first video
#' @param difference_lag numeric value specifying the offset between two frames of a video
#' @param thresholds Numeric vector containing the min and max threshold values
#' @param memory memory allocated to ImageJ
#' @export

check_threshold_values <- function(to.data, raw.video.folder, ijmacs.folder, vid_select = 0, difference.lag, thresholds, memory) {
  
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  ## generate the folders if not already existing
  dir.create(ijmacs.folder, showWarnings = FALSE)
  
  ## copy master copy of ImageJ macro there for treatment
  text <- readLines(paste(system.file(package="fRanco"), "/", "ImageJ macros/Check_threshold.ijm", sep = ""))
  
  ## use regular expression to insert input and output directory
  text[grep("avi_input =", text)] <- paste("avi_input = ", "'", video.dir, "';", sep = "")
  text[grep("i =", text)] <- paste("i = ", vid_select, ";", sep = "")
  text[grep("lag =", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
    
  ## re-create ImageJ macro for batch processing of video files with ParticleTracker and put this in a subdirectory of the data folder
  if (.Platform$OS.type == "windows") 
    writeLines(text, con = paste(to.data, ijmacs.folder, "Check_threshold_tmp.ijm", sep = ""), sep = "\n")
  if (.Platform$OS.type == "unix") 
    writeLines(text, con = paste(to.data, ijmacs.folder, "Check_threshold_tmp.ijm", sep = ""))
  
  ## run to process video files by calling ImageJ
  if (.Platform$OS.type == "unix") 
    cmd <- paste0("java -Xmx",memory ,"m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ","'", 
                  paste0(sub("1 - raw", "ijmacs", video.dir), "Check_threshold_tmp.ijm'"))
  if (.Platform$OS.type == "windows") 
    cmd <- c("\"C:/Program Files/FIJI.app/fiji-win64.exe\" -macro Check_threshold_tmp.ijm")
  
  system(cmd)
} 


