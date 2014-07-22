#' Function to check that video files have extension cxd or avi, and that they are otherwise compatible.
#' 
#' Checks the files in the raw data for the supported avi and cxd file formats and that file names do not 
#' contain periods except before the file type extension
#' @param to.data path to the working directory 
#' @param raw.video.folder directory with the raw video files
#' @return returns an error message and a list with unsupported files or names
#' @export

check_video_file_names <- function(to.data, raw.video.folder) {
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  files <- dir(video.dir)
  
  ## check for unsupported video file format
  unsupported.files <- files[-c(grep("\\.avi", files), grep("\\.cxd", files))]
  if (length(unsupported.files) > 0) 
    print(paste("Unsupported video file:", unsupported.files))
 
  ## check for files with more than one period; I think this previously caused me a problem
  bad.filenames <- files[unlist(lapply(lapply(strsplit(files, "\\."), length), function(x) x > 2))]
  if (length(bad.filenames) > 0) 
    print(paste("Bad video filename (no periods please, except before extension:", bad.filenames))
} 

