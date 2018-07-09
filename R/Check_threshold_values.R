#' Function to assist finding good thresholds used for the segmentation of the difference video. If you run bemovi for the first time, verify with this function that all target objects are
#' properly identified
#'  
#' This function creates an ImageJ macro that can be helpful for checking the thresholds specified in the user section; the macro will be saved in the ImageJ macro directory in the working directory and
#' then automatically opened in ImageJ. Depending on the video size, it might take some time to open the video and the thresholding menu. The default thresholds values 
#' should be adjusted in the ImageJ macro, until appropriate values are found. These values should then be used in the R functions / script, e.g., when calling the Locate_and_measure_particles function.
#' 
#' @param to.data path to the working directory
#' @param raw.video.folder directory with the raw video files 
#' @param ijmacs.folder directory where the check_trehsolds macro is saved
#' @param vid_select video selected to find appropriate thresholds; default is the first video
#' @param difference.lag numeric value specifying the offset between two frames of a video
#' @param thresholds Numeric vector containing the min and max threshold values
#' @param IJ.path path to ImageJ folder, containing the ij.jar executable
#' @param memory numeric value specifying the amount of memory available to ImageJ (defaults to 512)
#' @export

check_threshold_values <- function(
  to.data = par_to.data(), 
  raw.video.folder = par_raw.video.folder(), 
  ijmacs.folder = par_ijmacs.folder(), 
  vid_select = 0, 
  difference.lag = par_difference.lag(), 
  thresholds = par_thresholds(), 
  IJ.path = par_IJ.path(), 
  memory = par_memory()
) {
  
  video.dir <- file.path(to.data, raw.video.folder)
  ## generate the folders if not already existing
  dir.create(file.path(to.data, ijmacs.folder), showWarnings = FALSE)
  
  ## copy master copy of ImageJ macro there for treatment
  text <- readLines(file.path(system.file(package = "bemovi"), "/", "ImageJ_macros", "Check_threshold.ijm"))
  
  ## if there is differencing (i.e., difference.lag>0)
  if (difference.lag > 0)
    text <- readLines(file.path(system.file(package = "bemovi"), "/", "ImageJ_macros", "Check_threshold.ijm"))
  ## if there is no differencing (i.e., difference.lag==0)
  if (difference.lag == 0)
    text <- readLines(file.path(system.file(package = "bemovi"), "/", "ImageJ_macros", "Check_threshold_no_differencing.ijm"))
  
  
  
  ## use regular expression to insert input and output directory
  text[grep("video_input =", text)] <- paste("video_input = ", "'", video.dir, "';", sep = "")
  text[grep("i =", text)] <- paste("i = ", vid_select, ";", sep = "")
  text[grep("lag =", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
    
  ## re-create ImageJ macro for batch processing of video files with ParticleTracker and put this in a subdirectory of the data folder
  if (.Platform$OS.type == "windows") {
    writeLines(text, con = file.path(to.data, ijmacs.folder, "Check_threshold_tmp.ijm"))}
  if (.Platform$OS.type == "unix") {
    writeLines(text, con = file.path(to.data, ijmacs.folder, "Check_threshold_tmp.ijm"))}
  
  ## run to process video files by calling ImageJ
  if (.Platform$OS.type == "unix") 
    cmd <- paste0("java -Xmx", memory, "m -jar ", IJ.path, "/ij.jar", " -ijpath ", IJ.path, " -macro ", "'",
                  to.data,  ijmacs.folder, "Check_threshold_tmp.ijm'")
  if (.Platform$OS.type == "windows") 
   cmd <- paste0("\"", IJ.path,"\"", " -macro ","\"", paste0(gsub("/", "\\\\", paste0(to.data, ijmacs.folder))), "Check_threshold_tmp.ijm", "\"")
    
system(cmd)
} 


