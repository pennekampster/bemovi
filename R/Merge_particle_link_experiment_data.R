#' Function to create global database containg morphology data and trajectory information
#' 
#' Merges the morphology data, the trajectory data with the description file of the experiment containing
#' the information on sampling units, treatments and replication. The files are merged by the use of the
#' video file names
#' @param path Path to the data
#' @param particle.data.folder Directory containing the global morphology data
#' @param trajectory.data.folder Directory containing the global trajectory data
#' @param video.description.folder Directory containing the video description file
#' @param video.description.file Name of the video description file
#' @param merged.data.folder Directory where the global database is saved
#' @return Saves the global database to the merged.data.folder
#' @export
Merge_particle_link_experiment_data <- function(to.data, particle.data.folder, trajectory.data.folder, video.description.folder, 
                                                video.description.file, merged.data.folder) {
  
  # read the file that gives the important information about each video
  file.sample.info <- read.table(paste(to.data, video.description.folder, video.description.file, sep = ""), sep = "\t", 
                                 header = TRUE)
  
  ## load the two datasets
  morphology.data <- read.table(paste0(to.data, particle.data.folder, "particle.data.txt"), row.names = 1)
  trajectory.data <- read.table(paste0(to.data, trajectory.data.folder, "trajectory.data.txt"), header = TRUE, sep = "\t")
  
  # Prep for merging the trajectory data 
  # Note that the next lines also swap the x and y
  trajectory.data$Y1 <- -trajectory.data$X
  trajectory.data$X1 <- trajectory.data$Y
  trajectory.data$X <- trajectory.data$X1
  trajectory.data$Y <- trajectory.data$Y1
  ## trajectory frame starts with 0, therefore add one to adjust to morphology data
  trajectory.data$frame <- trajectory.data$frame + 1
  
  ## Prep for merging the morphology data
  morphology.data$frame <- morphology.data$Slice
  morphology.data$Slice <- NULL
  morphology.data$X <- morphology.data$X
  morphology.data$Y <- morphology.data$Y
  morphology.data$file <- sub(".cxd", "", morphology.data$file)
  
  ## merge the two datasets
  merged1 <- merge(morphology.data, trajectory.data, by.x = c("X", "Y", "frame", "file"), by.y = c("X", "Y", "frame", 
                                                                                                   "file"), all = T)
  ## make the merge of the file names case insensitive
  merged1$file <- tolower(merged1$file)
  file.sample.info$video <- tolower(file.sample.info$video)
  
  merged2 <- merge(merged1, file.sample.info, by.x = "file", by.y = "video", all = F)
  merged2 <- merged2[, is.na(match(names(merged2), c("X1", "Y1")))]
  
  dir.create(paste0(to.data, merged.data.folder), showWarnings = F)
  
  write.csv(merged2, file = paste(paste0(to.data, merged.data.folder), "MasterData.csv", sep = "/"), row.names = F)
  
} 