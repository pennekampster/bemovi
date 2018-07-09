#' Function to check that video files have extension cxd or avi, and that they are otherwise compatible.
#' 
#' Checks the files in the raw data for the supported avi and cxd file formats and that file names do not 
#' contain periods except before the file type extension
#' @param to.data path to the working directory 
#' @param raw.video.folder directory with the raw video files
#' @return returns an error message and a list with unsupported files or names
#' @export

check_video_file_names <- function(
  to.data = par_to.data(), 
  raw.video.folder = par_raw.video.folder(),
  video.description.folder = par_video.description.folder(), 
  video.description.file = par_video.description.file()
) {
  
  error_flag = FALSE
  
  video.dir <- file.path( to.data, raw.video.folder )
  files <- dir(video.dir)
  
  #check whether directory exists
  if (!dir.exists(video.dir)) {
    stop(
      "Selected directory does not exist. Please check that the path to the project directory is correct."
    )
  }
  
  #check whether there are any files in the directory
  if (length(files) == 0) {
    stop(
      "No videos in selected directory. Please check that the path to the project directory and folder name for raw videos are correct."
    )
  }
  
  ## check for unsupported video file format
  unsupported.files <- files[-c(grep("\\.avi", files), grep("\\.cxd", files))]
  if (length(unsupported.files) > 0) {
    print(paste("Unsupported video file:", unsupported.files))
    error_flag = TRUE
  }
  
  ## check for files with more than one period; I think this previously caused me a problem
  bad.filenames <- files[unlist(lapply(lapply(strsplit(files, "\\."), length), function(x) x > 2))]
  if (length(bad.filenames) > 0) {
    print(paste("Bad video filename (no periods please, except before extension:", bad.filenames))
    error_flag = TRUE
  } 
  
  ## check for files with hypens "-", as these cause problems
  bad.filenames <- files[grepl("-", files)]
  if (length(bad.filenames) > 0) {
    print(paste("Bad video filename (no hyphens please):", bad.filenames))
    error_flag = TRUE
  }
  
  ## Check filenames match those in the video description file
  col_classes <- vector(mode = "character")
  col_classes[1] <- "character"
  names(col_classes) <- "file"
  file.sample.info <-
    as.data.table(read.table(
      file.path(
        to.data,
        video.description.folder,
        video.description.file
      ),
      sep = "\t",
      colClasses = col_classes,
      header = TRUE
    ))
  vd_files <- unlist(strsplit(files, "\\."))[seq(1,length(files)*2, by = 2)]
  if (any(!file.sample.info$file %in% vd_files) | any(!vd_files %in% file.sample.info$file)) {
    print("You have a mismatch between the names of the video files, and the names of the files in the video description folder (though be aware that the file extension is ignored in this comparison).")
    error_flag = TRUE
  }
  
  if (!error_flag)
    print("File names seem appropriate and to match with those in the video description files.")
  
} 

