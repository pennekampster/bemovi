#' Improved function to filter trajectories based on movement and detection
#' 
#' The function will create a dataframe containing all trajectories that are valid for further analysis by selecting on minimum net displacement, detection rate, 
#' trajectory length and the median step_length which needs to surpass 2 pixels per frame
#' 
#' @param data Dataframe containing the rawdata on X- and Y-coordinates, frame, file and trajectory name, morphology and movement metrics
#' @param max_net_filter Minimum net displacement to be considered a valid trajectory
#' @param duration_filter Minimum duration to be considered a valid trajectory
#' @param detect_filter Minimum detection rate to be considered a valid trajectory
#' @param median_step_filter Threshold such that half of the step lengths are above the specified value
#' @return Returns a dataset with all fixes of valid trajectories
#' @export

filter_data2 <- function(raw_data,net_filter,duration_filter,detect_filter,median_step_filter){
  
  # filter out single coordinate detections
  raw_data <- raw_data[!is.na(raw_data$trajectory),]
  
  if (is.data.table(raw_data)==F){raw_data <- as.data.table(raw_data)}
  
  agg_data <- raw_data[ , list(duration= (max(frame)-min(frame))+1, N_frames=length(net_disp), max_net_disp=max(sqrt(net_disp), na.rm=T), median_step = median(step_length, na.rm=T)), by=traj] 
  agg_data[,detect:=N_frames/duration]
  
  agg_data <- agg_data[max_net_disp>net_filter & duration > duration_filter & detect > detect_filter & median_step > median_step_filter,]
  
  setkey(agg_data, traj)
  setkey(raw_data, traj)
  
  filter_data <- raw_data[agg_data]
  filter_data <- filter_data[,c("duration", "N_frames", "max_net_disp", "median_step", "detect"):=NULL]
  
  return(filter_data)
  
}