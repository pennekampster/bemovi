#' Function to summarize the morphology, movement and their variability on the trajectory level
#' 
#' Takes the data comprising the information for each frame and calculates mean and sd along the trajectory
#' @param data dataframe with the information on morphology and movement for each frame
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param write logical argument to indicate whether aggregated information should be saved to disk
#' @return returns a dataframe with the aggregated morphology and movement information for each trajectory 
#' @export

summarize_trajectories <- function(data, write=FALSE, to.data, merged.data.folder){

data <- as.data.table(data)
data[,id_:=id]

#summarize morphology
morphology <- data[, list(grey = mean(Mean),
                          sd_grey = sd(Mean),
                          area = mean(Area), 
                          sd_area = sd(Area),
                          perimeter = mean(Perimeter),
                          sd_perimeter = sd(Perimeter),
                          major = mean(Major), 
                          sd_major = sd(Major),
                          minor = mean(Minor), 
                          sd_minor = sd(Minor),
                          ar = mean(AR),
                          sd_ar = sd(AR)), by=id_]

#sumarize movement properties
turning <- data[!is.na(rel_angle), list(mean_turning= round(circ.mean(rel_angle),2), sd_turning=round(sd.circular(rel_angle),2)), by=id_]

mvt_properties <- data[,list(duration=(max(frame, na.rm=T)-min(frame, na.rm=T)+1)*25,
                            N_frames=length(frame),
                            max_net = round(max(sqrt(net_disp), na.rm=T), digits=2),
                            # select last value of net displacement
                            net_disp = round(sqrt(net_disp[length(net_disp)]),0),
                            net_speed = round(sqrt(net_disp[length(net_disp)])/(max(frame)-min(frame)),2),
                            gross_disp  = round(max(gross_disp, na.rm=T),2),
                            max_step = round(max(step_length, na.rm=T),2),
                            min_step = round(min(step_length, na.rm=T),2),
                            sd_step = round(sd(step_length, na.rm=T),2),
                            sd_gross_speed = round(sd(step_speed, na.rm=T),2),
                            max_gross_speed = round(max(step_speed, na.rm=T),2),
                            min_gross_speed = round(min(step_speed, na.rm=T),2)), 
                            by=id_]

# merge movement characteristics into dataframe
setkey(turning, id_)
setkey(mvt_properties, id_)
setkey(morphology, id_)

mvt_complete <- merge(turning,mvt_properties,by=c("id_"), all=T)
morph_mvt <- merge(morphology,mvt_complete,by=c("id_"), all=T)
morph_mvt$id <- morph_mvt$id_
morph_mvt$id_ <- NULL

#output summary data
if (write==TRUE){save(morph_mvt, file = paste0(to.data, merged.data.folder,"Morph_mvt.RData"))}
return(as.data.frame(morph_mvt))

}
