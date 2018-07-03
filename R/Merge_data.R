#' Function to create global database containg morphology, trajectory, and video (e.g., experimental) information
#' 
#' Merges the morphology data, the trajectory data with the video descriptions, which can / should contain 
#' the information on sampling units, video date and time, treatments and replicate etc. The files are merged by the use of the
#' video file names. For the exact meaning of each of the columns, please refer to the locate_and_measure_particles() and link_particles() functions.
#' @param to.data path to the working directory 
#' @param particle.data.folder directory containing the global morphology data
#' @param trajectory.data.folder directory containing the global trajectory data
#' @param video.description.folder directory containing the video description file
#' @param video.description.file name of the video description file
#' @param merged.data.folder directory where the global database is saved
#' @return saves the global database Master.rds to the merged.data.folder
#' @export

merge_data <- function(to.data, particle.data.folder, trajectory.data.folder, video.description.folder,video.description.file, merged.data.folder) {
  
  #id<-NULL
  
  # read the file that gives the important information about each video (specify the file variable to be character)
  col_classes <- vector(mode = "character")
  col_classes[1] <- "character"
  names(col_classes) <- "file"
  file.sample.info <- as.data.table(read.table(file.path(to.data, video.description.folder, video.description.file), sep = "\t", colClasses = col_classes, header = TRUE))
  
  
  ## load the two datasets
  morphology.data <- readRDS(file.path(to.data, particle.data.folder, "particle.rds"))
  trajectory.data <- readRDS(file.path(to.data, trajectory.data.folder, "trajectory.rds"))
  
  # Prep for merging the trajectory data 
  # Note that the next lines also swap the x and y
  trajectory.data <- as.data.table(trajectory.data)
  
  trajectory.data$Y1 <- -trajectory.data$X
  trajectory.data$X1 <-  trajectory.data$Y
  trajectory.data$X <-   trajectory.data$X1
  trajectory.data$Y <-   trajectory.data$Y1
  ## trajectory frame starts with 0, therefore add one to adjust to morphology data
  trajectory.data$frame <- trajectory.data$frame + 1
  trajectory.data$X1 <- NULL
  trajectory.data$Y1 <- NULL
  #trajectory.data$file <- tolower(trajectory.data$file)
    
  ## Prep for merging the morphology data
  morphology.data <- as.data.table(morphology.data)
  morphology.data$frame <- morphology.data$Slice
  morphology.data$Slice <- NULL
  #morphology.data$file <- tolower(morphology.data$file)
  
  ## merge the two datasets
  merged1 <- merge(morphology.data, trajectory.data, by = c("X", "Y", "frame", "file"), all = T)
  ## make the merge of the file names case insensitive
  #merged1 <- merged1[, file:=tolower(as.character(file))]

  setkey(merged1, file)
  setkey(file.sample.info, file)
  merged2 <- merge(merged1, file.sample.info, all = FALSE)
  
  # check that file contains data, otherwise report error
  if(nrow(merged2) == 0)
    stop("The merged data has no observations. This is could be due to missing matches between filenames of the video description file and the particle.rds and trajectory.rds,
         or due to the wrong file format of the video description file (should be a tab-delimited text file).")
  
  dir.create(file.path(to.data, merged.data.folder), showWarnings = FALSE)
  # drop particles which are not part of trajectories
  trajectory.data <- merged2[!is.na(merged2$id), ]
  
  setkey(trajectory.data, file, id, frame)
  
  ## create Master.rds
  
  saveRDS(trajectory.data, file = file.path(to.data, merged.data.folder, "Master.rds"))
} 