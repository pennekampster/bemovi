#' Function to summarize the morphology, movement and their variability on the trajectory level
#' 
#' Takes the data comprising the information for each frame and calculates summary statistics such as mean and sd (for all morphology metrics) and mean, sd and min/max 
#' for some of the movement metrics along the trajectory. Values are rounded to the second decimal. 
#' @param data dataframe with the information on morphology and movement for each frame
#' @param calculate.median logical value to indicate whether the median/IQR or the mean/SD summaries should be calculated for the morphology
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param write logical argument to indicate whether aggregated information should be saved to disk
#' @param fps Frames Per Second of the video
#' 
#' @return returns a data.table with the aggregated morphology and movement information for each trajectory
#' @import circular
#' @export

summarize_trajectories <- function(
  data, 
  calculate.median = TRUE, 
  write = FALSE, 
  to.data = par_to.data(), 
  merged.data.folder = par_merged.data.folder(),
  fps = par_fps()
){

#id_<-id<-Mean<-Area<-Perimeter<-Major<-Minor<-AR<-rel_angle<-fps<-net_disp<-gross_disp<-step_speed<-video.description.folder<-video.description.file<-NULL  

# checks whether frames per second are specified
if(!exists("fps") ) stop("frames per second not specified (fps)")

data <- as.data.table(data)
data[,id_:=id]

#summarize morphology
morphology <- if(calculate.median){
						data[, list(
                    median_grey = median(Mean),
                		median_area = median(Area),
             		    median_perimeter = median(Perimeter),
             		    median_major = median(Major),
             		    median_minor = median(Minor),
             		    median_ar = median(AR),
                		IQR_grey = IQR(Mean, na.rm = FALSE, type = 7),
                		IQR_area = IQR(Area, na.rm = FALSE, type = 7),
                		IQR_perimeter = IQR(Perimeter, na.rm = FALSE, type = 7),
                		IQR_major = IQR(Major, na.rm = FALSE, type = 7),
                		IQR_minor = IQR(Minor, na.rm = FALSE, type = 7),
                		IQR_ar = IQR(AR, na.rm = FALSE, type = 7)), 
                    by=id_]
              			} else {
                 			data[, 
                      list(
                      mean_grey = mean(Mean),
                 			sd_grey = sd(Mean),
                 			mean_area = mean(Area),
                 			sd_area = sd(Area),
                 			mean_perimeter = mean(Perimeter),
	                    sd_perimeter = sd(Perimeter),
	                        mean_major = mean(Major), 
	                        sd_major = sd(Major),
	                        mean_minor = mean(Minor), 
	                        sd_minor = sd(Minor),
	                        mean_ar = mean(AR),
	                        sd_ar = sd(AR)
                       ), by=id_]	
                 			}

#sumarize movement properties
turning <- data[!is.na(rel_angle), list(mean_turning= round(as.numeric(mean.circular(as.circular(rel_angle, control.circular=list(type='angles', units="radians", template='none', modulo='asis',zero=0, rotation='counter')))),2), 
                                        sd_turning=round(sd.circular(as.circular(rel_angle,control.circular=list(type='angles', units="radians", template='none', modulo='asis',zero=0, rotation='counter'))),2)), by=id_]

mvt_properties <- data[,list(duration=(max(frame, na.rm=T)-min(frame, na.rm=T)+1)/fps,
                             N_frames=length(frame),
                             max_net = round(max(sqrt(net_disp), na.rm=T), digits=2),
                             # select last value of net displacement
                             net_disp = round(sqrt(net_disp[length(net_disp)]),0),
                             net_speed = round(sqrt(net_disp[length(net_disp)])/((max(frame, na.rm=T)-min(frame, na.rm=T)+1)/fps),2),
                             gross_disp  = round(max(gross_disp, na.rm=T),2),
                             gross_speed = round(max(gross_disp, na.rm=T)/((max(frame, na.rm=T)-min(frame, na.rm=T)+1)/fps),2),
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

# extract morph_mvt$file from morph_mvt$id
morph_mvt$file <- lapply(strsplit(as.character(morph_mvt$id), "\\-"), "[", 1)

# Load video.description.file:
col_classes <- vector(mode = "character")
col_classes[1] <- "character"
names(col_classes) <- "file"
video.descr.file <- read.delim(file.path(to.data, video.description.folder, video.description.file), colClasses = col_classes, stringsAsFactors=F)

#morph_mvt is not normal data.frame it's list of lists. Make it a "regular" data.frame:
morph_mvt <- as.data.frame(lapply(morph_mvt, function(X) unname(unlist(X))))

# Add the information contained in video.descr.file
morph_mvt <- merge(morph_mvt, video.descr.file, by = "file")
morph_mvt$id <- as.character(morph_mvt$id)
morph_mvt$file <- as.character(morph_mvt$file)

  
#output summary data
if (write==TRUE){saveRDS(morph_mvt, file = file.path(to.data, merged.data.folder,"Morph_mvt.rds"))}
return(as.data.frame(morph_mvt))

}