#' Function to summarize the average morphology and movement and their variability on the trajectory level
#' 
#' Takes the data comprising the information for each frame and calculates mean and sd along the trajectory
#' @param data dataframe with the information on morphology and movement for each frame
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param write logical argument to indicate whether aggregated information should be saved to disk
#' @return returns a dataframe with the aggregated morphology and movement information for each trajectory 
#' @export

summarize_trajectories2 <- function(data, write=FALSE, to.data, merged.data.folder){

data <- as.data.table(data) 

#summarize morphology
morphology <- data[, list(grey = mean(Mean),
                          sd_grey = sd(Mean),
                          area = mean(Area), 
                          sd_area = mean(Area),
                          perimeter = mean(Perimeter),
                          sd_perimeter = sd(Perimeter),
                          major = mean(Major), 
                          sd_major = sd(Major),
                          minor = mean(Minor), 
                          sd_minor = sd(Minor),
                          ar = mean(AR),
                          sd_ar = sd(AR)), by=traj]

#sumarize movement properties
turning <- data[!is.na(rel_angle), list(mean_turning=circ.mean(rel_angle),sd_turning=sd.circular(rel_angle)), by=traj]

mvt_properties <- data[,list(duration=(max(frame, na.rm=T)-min(frame, na.rm=T)+1),
                            N_frames=length(frame),
                            max_net = max(sqrt(net_disp), na.rm=T),
                            # select last value of net displacement
                            net_disp = sqrt(net_disp[length(net_disp)]),
                            net_speed = sqrt(net_disp[length(net_disp)])/(max(frame)-min(frame)),
                            gross_disp  = max(gross_disp, na.rm=T),
                            max_step = max(step_length, na.rm=T),
                            min_step = min(step_length, na.rm=T),
                            sd_step = sd(step_length, na.rm=T),
                            sd_gross_speed = sd(step_speed, na.rm=T),
                            max_gross_speed = max(step_speed, na.rm=T),
                            min_gross_speed = min(step_speed, na.rm=T)), by=traj]

# merge movement characteristics into dataframe
setkey(turning, traj)
setkey(mvt_properties, traj)
setkey(morphology, traj)

mvt_complete <- merge(turning,mvt_properties,by=c("traj"), all=T)
morph_mvt <- merge(morphology,mvt_complete,by=c("traj"), all=T)

#output summary data
if (write==TRUE){save(morph_mvt, file = paste0(to.data, merged.data.folder,"Morph_mvt.RData"))}
return(as.data.frame(morph_mvt))

}
