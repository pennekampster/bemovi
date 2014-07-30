#' Function to create global database containg morphology data and trajectory information
#' 
#' Merges the morphology data, the trajectory data with the description file of the experiment containing
#' the information on sampling units, treatments and replication. The files are merged by the use of the
#' video file names
#' @param to.data path to the working directory 
#' @param particle.data.folder directory containing the global morphology data
#' @param trajectory.data.folder directory containing the global trajectory data
#' @param video.description.folder directory containing the video description file
#' @param video.description.file name of the video description file
#' @param merged.data.folder directory where the global database is saved
#' @return saves the global database to the merged.data.folder
#' @export

merge_data <- function(to.data, particle.data.folder, trajectory.data.folder, video.description.folder,video.description.file, merged.data.folder) {
  
  # read the file that gives the important information about each video
  file.sample.info <- as.data.table(read.table(paste(to.data, video.description.folder, video.description.file, sep = ""), sep = "\t", header = TRUE))
  file.sample.info$file <- tolower(file.sample.info$file)
  
  ## load the two datasets
  load(paste0(to.data, particle.data.folder,"particle.RData"))
  load(paste0(to.data, trajectory.data.folder,"trajectory.RData"))
  
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
    
  ## Prep for merging the morphology data
  morphology.data <- as.data.table(morphology.data)
  morphology.data$frame <- morphology.data$Slice
  morphology.data$Slice <- NULL
#  morphology.data$X <- morphology.data$X
#  morphology.data$Y <- morphology.data$Y
  morphology.data$file <- sub(".cxd|.avi", "", morphology.data$file)
  
  ## merge the two datasets
  merged1 <- merge(morphology.data, trajectory.data, by = c("X", "Y", "frame", "file"), all = T)
  ## make the merge of the file names case insensitive
  merged1 <- merged1[, file:=tolower(file)]
  #merged1$file <- tolower(merged1$file)

  setkey(merged1, file)
  setkey(file.sample.info, file)
  #merged2 <- merge(merged1, file.sample.info, by.x = "file", by.y = "video", all = F)
  merged2 <- merge(merged1, file.sample.info, all = F)
  #merged2 <- merged2[, is.na(match(names(merged2), c("X1", "Y1")))]
  
  dir.create(paste0(to.data, merged.data.folder), showWarnings = F)
  # drop particles which are not part of trajectories
  trajectory.data <- merged2[!is.na(merged2$id), ]
  
  setkey(trajectory.data, file, id, frame)
  
  save(trajectory.data, file = paste0(to.data, merged.data.folder,"Master.RData")) 
  
} 