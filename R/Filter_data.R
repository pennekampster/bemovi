#' Improved function to filter trajectories based on movement and detection
#' 
#' The function will create a dataframe containing all trajectories that are valid for further analysis by selecting on minimum net displacement, detection rate, 
#' trajectory length and the median step_length which needs to surpass 2 pixels per frame
#' 
#' @param raw_data dataframe containing the rawdata on X- and Y-coordinates, frame, file and trajectory name, morphology and movement metrics
#' @param net_filter minimum net displacement to be considered a valid trajectory
#' @param duration_filter minimum duration to be considered a valid trajectory
#' @param detect_filter minimum detection rate to be considered a valid trajectory
#' @param median_step_filter threshold such that half of the step lengths are above the specified value
#' @return returns a dataset with all fixes of valid trajectories
#' @export

filter_data <- function(raw_data, net_filter, duration_filter, detect_filter, median_step_filter){
  
  # filter out single coordinate detections
  raw_data <- raw_data[!is.na(raw_data$trajectory),]
  # rename frame column due to clash with frame() function in base graphics
  raw_data$frame_ <- raw_data$frame  
  raw_data <- as.data.table(raw_data)
  raw_data$id_ <- raw_data$id
  
  # aggregate data
  agg_data <- raw_data[ , list(duration=max(frame_)-min(frame_)+1, N_frames=length(net_disp), max_net_disp=max(sqrt(net_disp), na.rm=T), median_step = median(step_length, na.rm=T)), by=id_] 
  agg_data[,detect:=N_frames/duration]
  
  # filter data based on specifications given as arguments
  agg_data <- agg_data[max_net_disp>net_filter & duration > duration_filter & detect > detect_filter & median_step > median_step_filter,]
  
  # set keys
  setkey(agg_data, id_)
  setkey(raw_data, id_)
  
  # only retain those trajectories that fulfill specs
  filter_data <- raw_data[agg_data]
  filter_data <- filter_data[,c("duration", "N_frames", "max_net_disp", "median_step", "detect", "frame_", "id_"):=NULL]
  
  setkey(filter_data, file, id, frame)
  return(filter_data)
  
}