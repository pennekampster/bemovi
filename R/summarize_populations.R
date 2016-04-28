#' Function to calculate densities and summarize morphology and movement at the population (sample) level
#' 
#' Takes the data comprising the information for each frame and calculates summary statistics such as mean and sd (for all morphology metrics) and mean, sd and min/max 
#' for some of the movement metrics along the trajectory. Values are rounded to the second decimal. 
#' 
#' @param traj.data dataframe with the information on morphology and movement for each frame (either "trajectoty.data" or "trajectory.data.filtered")
#' @param sum.data data.table with the aggregated morphology and movement information for each trajectory ("morph_mvt")
#' @param write logical argument to indicate whether aggregated information should be saved to disk (file name: "Population_Data.RData")
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param video.description.folder directory with the video description file
#' @param video.description.file name of the video description file
#' @return returns a data.table with the population densities, biovolume as well as the aggregated morphology and movement information for each population (sample)
#' @export
#' @examples
#' summarize_populations()

summarize_populations <- function(traj.data, sum.data, write=FALSE, to.data, merged.data.folder, video.description.folder, video.description.file){
  
  # checks whether frames per second are specified
  if(!exists("fps") ) stop("frames per second not specified (fps)")
  # checks whether the sample volume is specified
  if(!exists("measured_volume") ) stop("measured volume not specified (measured_volume)")
  
  # results object from video description file
  pop_output <- read.table(paste(to.data, video.description.folder, video.description.file, sep = ""), sep = "\t", header = TRUE)
  
  # now add the population densities
  pop_count_table <- tapply(traj.data$Major,list(as.factor(traj.data$file),as.factor(traj.data$frame)),length)
  
  help_cnt_rep <- which(is.element(pop_output$file,dimnames(as.matrix(rowMeans(pop_count_table)))[[1]]))
  
  pop_output$indiv_per_frame <- 0
  pop_output$indiv_per_frame[help_cnt_rep] <- as.numeric(apply(pop_count_table,1,sum,na.rm=T))/total_frames

  pop_output$indiv_per_volume <- 0
  pop_output$indiv_per_volume[help_cnt_rep] <- as.numeric(apply(pop_count_table,1,sum,na.rm=T))/total_frames /measured_volume
  
  # add the mean of total area per frame (bioarea by frame)
  pop_count_table2 <- tapply(traj.data$Area,list(as.factor(traj.data$file),as.factor(traj.data$frame)),sum)
  help_cnt_rep <- which(is.element(pop_output$file,dimnames(as.matrix(rowMeans(pop_count_table2)))[[1]]))
  pop_output$bioarea_per_frame <- 0
  pop_output$bioarea_per_frame[help_cnt_rep] <- as.numeric(apply(pop_count_table2,1,sum,na.rm=T))/total_frames 
  
  # add the mean of total area per volume (bioarea by volume; by Isabelle Gounand)
  pop_output$bioarea_per_volume <- 0
  pop_output$bioarea_per_volume[help_cnt_rep] <- as.numeric(apply(pop_count_table2,1,sum,na.rm=T))/total_frames/measured_volume
  
  # first get file from id
  sum.data$file <- sub("-.*$", "", sum.data$id )
  
  # get morphology
  pop_output$major_mean <- NA
  pop_output$major_sd <- NA
  pop_output$minor_mean <- NA
  pop_output$minor_sd <- NA
  
  pop_output$major_mean[help_cnt_rep] <- as.numeric(tapply(sum.data$mean_major,sum.data$file,mean,na.rm=T))
  pop_output$major_sd[help_cnt_rep] <- as.numeric(tapply(sum.data$mean_major,sum.data$file,sd,na.rm=T))
  pop_output$minor_mean[help_cnt_rep] <- as.numeric(tapply(sum.data$mean_minor,sum.data$file,mean,na.rm=T))
  pop_output$minor_sd[help_cnt_rep] <- as.numeric(tapply(sum.data$mean_minor,sum.data$file,sd,na.rm=T))
  
  # calculate gross speed
  sum.data$gross_speed <- sum.data$gross_disp/sum.data$duration 
  
  # get movement
  pop_output$gross_speed_mean <- NA
  pop_output$gross_speed_sd <- NA
  pop_output$net_speed_mean <- NA
  pop_output$net_speed_sd <- NA
  pop_output$sd_turning_mean <- NA
  
  # mean gross speed is not returned yet --> calc here
  sum.data$gross_speed <- sum.data$gross_disp/sum.data$duration
  
  pop_output$gross_speed_mean[help_cnt_rep] <- as.numeric(tapply(sum.data$gross_speed,sum.data$file,mean,na.rm=T))
  pop_output$gross_speed_sd[help_cnt_rep] <- as.numeric(tapply(sum.data$gross_speed,sum.data$file,sd,na.rm=T))
  
  pop_output$net_speed_mean[help_cnt_rep] <- as.numeric(tapply(sum.data$net_speed,sum.data$file,mean,na.rm=T))
  pop_output$net_speed_sd[help_cnt_rep] <- as.numeric(tapply(sum.data$net_speed,sum.data$file,sd,na.rm=T))
  
  pop_output$sd_turning_mean[help_cnt_rep] <- as.numeric(tapply(sum.data$sd_turning,sum.data$file,mean,na.rm=T))
  
  
  #output population summary data
  if (write==TRUE){save(pop_output, file = paste0(to.data, merged.data.folder,"Population_Data.RData"))}
  return(as.data.frame(pop_output))
}
