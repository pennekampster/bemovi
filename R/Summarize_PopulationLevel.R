#' Function to summarize the morphology, movement and their variability at the population (sample) level
#' adds population density estimates; does not care about different species
#' 
#' Takes the data comprising the information for each frame and calculates summary statistics such as mean and sd (for all morphology metrics) and mean, sd and min/max 
#' for some of the movement metrics along the trajectory. Values are rounded to the second decimal. 
#' @param data dataframe with the information on morphology and movement for each frame
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param write logical argument to indicate whether aggregated information should be saved to disk
#' @param give.fps Give frames per second (in object named fps) if it is not already defined globally
#' @return returns a data.table with the aggregated morphology and movement information for each trajectory
#' @export

summarize_populations <- function(traj.data, sum.data, write=FALSE, to.data, merged.data.folder, video.description.folder, video.description.file){
  
  # checks whether frames per second are specified
  if(!exists("fps") ) stop("frames per second not specified (fps)")
  # checks whether the sample volume is specified
  if(!exists("measured_volume") ) stop("measured volume not specified (measured_volume)")
  
  # results object from video description file
  pop_output <- read.table(paste(to.data, video.description.folder, video.description.file, sep = ""), sep = "\t", header = TRUE)
  
  # now add the population densities
  pop_count_table <- tapply(traj.data$Major,list(as.factor(traj.data$file),as.factor(traj.data$frame)),length)
  pop_output$counts <- as.numeric(rowMeans(pop_count_table))
  pop_output$indiv_per_vol <- as.numeric(rowMeans(pop_count_table))/measured_volume
  
  # first get file from id
  sum.data$file <- sub("-.*$", "", sum.data$id )
  
  # get morphology
  pop_output$major_mean <- as.numeric(tapply(sum.data$major,sum.data$file,mean,na.rm=T))
  pop_output$major_sd <- as.numeric(tapply(sum.data$major,sum.data$file,sd,na.rm=T))
  pop_output$minor_mean <- as.numeric(tapply(sum.data$minor,sum.data$file,mean,na.rm=T))
  pop_output$minor_sd <- as.numeric(tapply(sum.data$minor,sum.data$file,sd,na.rm=T))
  
  # calculate gross speed
  sum.data$gross_speed <- sum.data$gross_disp/sum.data$duration 
  
  # get movement
  pop_output$gross_speed_mean <- as.numeric(tapply(sum.data$gross_speed,sum.data$file,mean,na.rm=T))
  pop_output$gross_speed_sd <- as.numeric(tapply(sum.data$gross_speed,sum.data$file,sd,na.rm=T))
  
  pop_output$net_speed_mean <- as.numeric(tapply(sum.data$net_speed,sum.data$file,mean,na.rm=T))
  pop_output$net_speed_sd <- as.numeric(tapply(sum.data$net_speed,sum.data$file,sd,na.rm=T))
  
  pop_output$sd_turning_mean <- as.numeric(tapply(sum.data$sd_turning,sum.data$file,mean,na.rm=T))
  
  
  #output population summary data
  if (write==TRUE){save(pop_output, file = paste0(to.data, merged.data.folder,"Population_Data.RData"))}
  return(as.data.frame(pop_output))
}
