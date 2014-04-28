#' Function to summarize the mean morphology and movement and its variability on a trajectory level
#' 
#' Takes the data comprising the information for each frame and calculates mean and sd along the trajectory
#' @param data Dataframe with the information on morphology and movement for each frame
#' @return Saves the aggregated information on each trajectory to the disk
#' @export
summarize_trajectories <- function(data){

#summarize morphology
morphology <- as.data.frame(data %.%
                              group_by(traj) %.%
                              summarise(grey = mean(Mean),
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
                                        sd_ar = sd(AR)))

#summarize the movement
turning <- as.data.frame(data %.%
                           filter(!is.na(rel_angle)) %.%
                           group_by(traj) %.%
                           summarise(mean_turning=circ.mean(rel_angle),
                                     sd_turning=sd.circular(rel_angle)))

mvt_properties <- as.data.frame(data %.%
                                  group_by(traj) %.%
                                  summarise(duration=(max(frame, na.rm=T)-min(frame, na.rm=T)+1),
                                            N_frames=n(),
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
                                            min_gross_speed = min(step_speed, na.rm=T)))

# merge movement characteristics into dataframe
mvt_complete <- merge(turning,mvt_properties,by=c("traj"), all=T)
morph_mvt <- merge(morphology,mvt_complete,by=c("traj"), all=T)

#output summary data
write.csv(morph_mvt, file = paste(paste0(to.data, merged.data.folder), "Morph_mvt.csv", sep = "/"), row.names = F)
}