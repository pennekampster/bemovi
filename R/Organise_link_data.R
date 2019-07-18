#' Function to merge the output of the ParticleLinker into one large database
#' 
#' Merge the trajectory data from the ParticleLinker into one data file
#' @param to.data path to the working directory
#' @param trajectory.data.folder directory where the output of the ParticleLinker is saved
#' @return saves the data containing the X- and Y coordinates of a given trajectory, the frame, the trajectory ID and the file name of the video 
#' from which the data was extracted to disk
#' @export

organise_link_data <- function(to.data, trajectory.data.folder) {
  
  here <- file.path(to.data, trajectory.data.folder)
  
  dir.create(here, showWarnings = F)
  df <- data.frame(frame = numeric(), X = numeric(), Y = numeric(), trajectory = numeric(), file = character())
  files <- dir(here, pattern = ".ijout.txt", full.names=T)
  
  mylist <- lapply(files, fread, header=T)
  mylist <- mylist[lapply(mylist,length)>0]
  data.full <- rbindlist(mylist)
  data.full$file <- gsub(".ijout.txt", "", gsub("ParticleLinker_", "", rep(dir(here, pattern = ".ijout.txt"), lapply(mylist, nrow))))
  data.full$y <- -data.full$y
  trajectory.data <- as.data.frame(data.full)
  trajectory.data <- trajectory.data[, c(2, 4, 3, 1, 5)]
  colnames(trajectory.data) <- c("frame", "X", "Y", "trajectory", "file")
  
  return(trajectory.data)
}