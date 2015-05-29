#' A function to calculate movement metrics for each trajectory
#' 
#' The function takes the X- and Y-coordinates for each unqiue trajectory and calculates movement metrics
#' such as the gross and net displacement, absolute and relative angles and duration
#' 
#' @param data dataframe containing the X- and Y-coordinates, the frame and the trajectory ID
#' @param to.data path to the working directory 
#' @param trajectory.data.folder directory where the data is saved
#' @param pixel_to_scale specify how a pixel scales to real dimensions
#' @param fps specify the frame rate of the video
#' @return returns a data.table with the movement metrics for each fix appended to the original data (NB: movement metrics often need two (e.g. step length), sometimes even 
#' three (e.g., turning angles) fixes; fixes for which metrics cannot be calculated are padded with NA). The movement parameters are the step length, the step duration, 
#' the step speed (step length/step duration), the gross displacement as the cumulative sum of the step lengths, the net displacement between the first fix of a given trajectory 
#' and the current fix and finally the relative angle (turning angle) and absolute angle (in radians). For details on these metrics, please refer to a dedicated textbook 
#' (e.g. Turch (1998): Quantitative Analysis of Movement: Measuring and Modeling Population Redistribution in Animals and Plants, Sinauer Associates, Sunderland).
#' @import data.table 
#' @export

calculate_mvt <- function(data,to.data,trajectory.data.folder,pixel_to_scale,fps){

  if(!exists("fps")) stop("No fps information provided. Please specify path in global options.")
  if(!exists("pixel_to_scale")) stop("No pixel to real scale conversion provided. Please specify path in global options.")

  # output path
  out.dir <- paste0(to.data,trajectory.data.folder)

  # create unique ID consisting of trajectory ID and file
  id <- paste(data$file,data$trajectory,sep="-")
  data <- as.data.table(cbind(data,id))
  data$id <- as.character(data$id)
  
  # keep a copy of the original data for left join later, but drop redundant columns
  data_full <- data

  #order dataframe
  setkey(data, file, trajectory, frame)
  
  # filter out single coordinates which do not form trajectories
  data <- data[!is.na(data$trajectory),]
  
  #filter out duplicate positions, if available
  data <- data[!(diff(X) == 0 & diff(Y) == 0),]
  
  #subset dataset to only include relevant movement information
  data <- data[,list(file,X,Y,frame,id,trajectory)]
  
  #rename frame column to avoid clashes with frame() function
  setnames(data, c("file","X","Y","frame","id","trajectory"), c("file","X","Y","frame_","id","trajectory"))

  # convert to real dimensions
  data$X <- data$X * pixel_to_scale
  data$Y <- data$Y * pixel_to_scale
  
  data$frame_ <- data$frame
  mvt_summary <- data[,list(frame=frame_,
                         step_length = round(step_length(X,Y),2),
                         step_duration = step_duration(frame_)/fps,
                         step_speed = round(step_length(X,Y)/(step_duration(frame_)/fps),2),
                         gross_disp = round(cumsum(step_length(X,Y)),2),
                         net_disp = round(net_displacement(X,Y),0),
                         abs_angle = round(anglefun(diff(X),diff(Y)),2),
                         rel_angle = round(rel.angle(anglefun(diff(X),diff(Y))),2)), by=id]

  mvt_summary <- mvt_summary[ , list(id, frame, step_length, step_duration, step_speed, gross_disp, net_disp, abs_angle, rel_angle)]

  trajectory.data <- merge(data_full,mvt_summary,by=c("id","frame"), all.x=T)

  save(trajectory.data, file = paste0(out.dir,"trajectory.RData"))

}


