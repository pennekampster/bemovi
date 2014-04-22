#' Function to merge the output of the ParticleLinker into one large database
#' 
#' Merge the trajectory data from the ParticleLinker into one data file
#' @param path Path to the directory where ParticleLinker output is stored 
#' @return Returns a text file (trajectory.data.txt) containing the X- and Y coordinates of a given trajectory, 
#' the frame, the trajectory ID and the file name of the video from which the data was extracted
#' @export
Organise_link_data <- function(to.data, trajectory.data.folder) {
  
  here <- paste0(to.data, trajectory.data.folder)
  
  dir.create(here, showWarnings = F)
  df <- data.frame(frame = numeric(), X = numeric(), Y = numeric(), trajectory = numeric(), file = character())
  files <- dir(here, pattern = ".ijout.txt.txt")
  for (i in 1:length(files)) {
    file <- gsub(".ijout.txt.txt", "", gsub("ParticleLinker_", "", files[i]))
    if (file.info(paste(here, files[i], sep = "/"))$size > 0) {
      data <- read.table(paste(here, files[i], sep = "/"), header = T, sep = ",")
      data$file <- rep(file, length(data$x))
      data$y <- -data$y
      if (i == 1) 
        data.full <- rbind(data, df)
      if (i > 1) 
        data.full <- rbind(data.full, data)
    }
  }
  data.full <- data.full[, c(2, 4, 3, 1, 5)]
  colnames(data.full) <- c("frame", "X", "Y", "trajectory", "file")
  write.table(data.full, file = paste(here, "trajectory.data.txt", sep = "/"), sep = "\t")
} 

