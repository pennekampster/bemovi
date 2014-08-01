#' A function to calculate movement metrics for each trajectory, which can be used to predict the species identity
#' 
#' The function takes the X- and Y-coordinates for each unqiue trajectory and calculates movement metrics
#' such as the gross and net displacement, absolute and relative angles and duration
#' 
#' @param data dataframe containing the X- and Y-coordinates, the frame and the trajectory ID
#' @param to.data path to the working directory 
#' @param merged.data.folder directory where the data is saved
#' @return returns a data.table with the movement metrics for each fix appended to the original data (NB: movement metrics often need two (e.g. step length), sometimes even 
#' three (e.g., turning angles) fixes; fixes for which metrics cannot be calculated are padded with NA)
#' @export

calculate_mvt <- function(data,to.data,merged.data.folder){

  # output path
  out.dir <- paste0(to.data,merged.data.folder)

  # create unique ID consisting of trajectory ID and file
  id <- paste(data$file,data$trajectory,sep="-")
  data <- as.data.table(cbind(data,id))

  # keep a copy of the original data for left join later, but drop redundant columns
  data_full <- data

  #order dataframe
  setkey(data, file, trajectory, frame)
  
  # filter out single coordinates which do not form trajectories
  data <- data[!is.na(data$trajectory),]
  
  #filter out duplicate positions, if available
  data <- data[-which(diff(data$X) == 0 & diff(data$Y) == 0),]
  
  #subset dataset to only include relevant movement information
  data <- data[,list(file,X,Y,frame,id,trajectory)]
  #rename frame column to avoid clashes with frame() function
  setnames(data, c("file","X","Y","frame","id","trajectory"), c("file","X","Y","frame_","id","trajectory"))

  mvt_summary <- data[,list(frame = frame_,
                         step_length = round(step_length(X,Y),2),
                         step_duration = step_duration(frame_),
                         step_speed = round(step_length(X,Y)/step_duration(frame_),2),
                         gross_disp = round(cumsum(step_length(X,Y)),2),
                         net_disp = round(net_displacement(X,Y),0),
                         abs_angle = round(anglefun(diff(X),diff(Y)),2),
                         rel_angle = round(rel.angle(anglefun(diff(X),diff(Y))),2)), by=id]

  mvt_summary <- mvt_summary[ , list(id,frame,step_length, step_duration, step_speed, gross_disp, net_disp, abs_angle, rel_angle)]

  trajectory.data <- merge(data_full,mvt_summary,by=c("id","frame"), all.x=T)

  save(trajectory.data, file = paste0(out.dir,"trajectory.RData"))

}


