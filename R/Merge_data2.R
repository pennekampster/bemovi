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
#' @return saves the global database Master.RData to the merged.data.folder
#' @export

merge_data2 <- function (to.data, particle.data.folder, trajectory.data.folder, 
                         video.description.folder, video.description.file, merged.data.folder) 
{
  file.sample.info <- as.data.table(read.table(paste(to.data, 
                                                     video.description.folder, video.description.file, sep = ""), 
                                               sep = "\t", header = TRUE))
  file.sample.info$file <- tolower(file.sample.info$file)
  load(paste0(to.data, particle.data.folder, "particle.RData"))
  load(paste0(to.data, trajectory.data.folder, "trajectory.RData"))
  browser()
  trajectory.data <- as.data.table(trajectory.data)
  trajectory.data[, `:=`(Y1 = -X, X = Y), ]
  trajectory.data[, `:=`(Y = Y1), ]
  trajectory.data[, `:=`(Y = Y1, frame = frame + 1), ]
  trajectory.data[, `:=`(c("Y1"), NULL)]
  keycols = c("X","Y","frame","file")
  setkeyv(trajectory.data,keycols)
  morphology.data <- as.data.table(morphology.data)
  morphology.data[, `:=`(frame = Slice), ]
  morphology.data[, `:=`(Slice = NULL), ]
  morphology.data[, `:=`(file = sub(".cxd | .avi", "", file)), ]
  keycols = c("X","Y","frame","file")
  setkeyv(morphology.data,keycols)
  #morphology.data <- merge(morphology.data, trajectory.data, all = T)
  morphology.data <- morphology.data[trajectory.data]
  morphology.data[, `:=`(file, tolower(file))]
  setkey(morphology.data, file)
  setkey(file.sample.info, file)
  morphology.data <- merge(file.sample.info, morphology.data, by=c("file"), all = F)
  dir.create(paste0(to.data, merged.data.folder), showWarnings = F)
  trajectory.data <- morphology.data[!is.na(morphology.data$id), ]
  setkey(trajectory.data, file, id, frame)
  save(trajectory.data, file = paste0(to.data, merged.data.folder, 
                                      "Master.RData"))
}