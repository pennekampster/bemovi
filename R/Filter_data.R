#' Function to filter trajectories based on movement and detection
#' 
#' The function will create two dataframes: one containing the trajectories that are artefacts, one containing the valid trajectories for analysis
#' @param data Dataframe containing the rawdata on X- and Y-coordinates, frame, file and trajectory name, morphology and movement metrics
#' @param max_net Minimum net displacement to be considered a valid trajectory
#' @param duration Minimum duration to be considered a valid trajectory
#' @param detect Minimum detection rate to be considered a valid trajectory
#' @return Returns two datasets: one containing the artefacts, one the valid trajectory data
#' @export
filter_data <- function(raw_data,max_net_filter,duration_filter,detect_filter){

# filter out single coordinate detections
data <- raw_data[!is.na(raw_data$trajectory),]
artefacts <- raw_data[is.na(raw_data$trajectory),]

#calculate movement properties
mvt_properties <- as.data.frame(data %.%
                                group_by(traj) %.%
                                summarise(duration=(max(frame, na.rm=T)-min(frame, na.rm=T)+1),
                                          N_frames=n(),
                                          max_net = max(sqrt(net_disp), na.rm=T)))
                                          # select last value of net displacement
                                          #net_disp = sqrt(net_disp[length(net_disp)]),
                                          #net_speed = sqrt(net_disp[length(net_disp)])/(max(frame)-min(frame)),
                                          #gross_disp  = max(gross_disp, na.rm=T),
                                          #max_step = max(step_length, na.rm=T),
                                          #min_step = min(step_length, na.rm=T),
                                          #sd_step = sd(step_length, na.rm=T),
                                          #sd_gross_speed = sd(step_speed, na.rm=T),
                                          #max_gross_speed = max(step_speed, na.rm=T),
                                          #min_gross_speed = min(step_speed, na.rm=T)))

mvt_properties$detect <- mvt_properties$N_frames/mvt_properties$duration


data2 <- as.data.frame(inner_join(data,mvt_properties,by="traj") %.%
                      filter(max_net >= max_net_filter  & (duration >= duration_filter | detect >= detect_filter)))
data2 <- subset(data2, select=-c(max_net,N_frames,duration,detect))

artefacts2 <- as.data.frame(inner_join(data,mvt_properties,by="traj") %.%
                           filter(max_net < max_net_filter & (duration < duration_filter | detect < detect_filter)))
artefacts2 <- subset(artefacts2, select=-c(max_net,N_frames,duration,detect))
artefacts <- rbind(artefacts,artefacts2)

# re-factor 
data2$traj <- factor(data2$traj)
artefacts$traj <- factor(artefacts$traj)

return(data2)

}