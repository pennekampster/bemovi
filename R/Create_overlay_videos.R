#' Function to create a new video with the extracted trajectories (filtered or raw) overlayed onto the original video
#' 
#' A function to overlay the extracted trajectories onto the original video, using plots created in R and then processed in 
#' ImageJ; two visualization types are available
#' 
#' @param traj.data dataframe with the information on morphology and movement for each frame (either "trajectory.data" or "traj.data.filtered")
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param raw.video.folder directory with the raw video files 
#' @param temp.overlay.folder temporary directory to save the overlay created with R
#' @param overlay.folder directory where the overlay videos are saved
#' @param width width of the raw video
#' @param height height of the raw video
#' @param difference.lag numeric value specifying the offset between two video frames to compute the difference image
#' @param type string indicating the visualization type (i.e. 'label' or 'traj'): either the overlay
#' is showing the trajectory ID and outlines the detected particle (type='label') or the whole trajectory
#' remains plotted (type='traj').
#' @param predict_spec logical If TRUE, the Master.RData file must have a column called predict_spec, indicating the species to which the trajectory belongs
#' @param contrast.enhancement numeric value to increase the contrast of the original video
#' @param IJ.path path to ImageJ folder, containing the 'ij.jar' executable
#' @param memory numeric value specifying the amount of memory available to ImageJ (defaults to 512)
#' @export

create_overlays <- function(traj.data, to.data, merged.data.folder, raw.video.folder, temp.overlay.folder, overlay.folder, 
                                  width, height, difference.lag, type = "traj",  predict_spec=F, contrast.enhancement = 0, IJ.path, memory = 512) {
  
  #traj.data<-trajectory<-ijmacs.folder<-NULL
  
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  
  load(file = paste(to.data,merged.data.folder, "Master.RData", sep = "/")) 
  
  trajectory.data <- as.data.table(trajectory.data)
  setkey(trajectory.data,file,frame)
  
  file_names <- unique(trajectory.data$file)

  
  ## change path for output
  dir.create(paste0(to.data, temp.overlay.folder), showWarnings = F)
    for (i in 1:length(file_names)) {
    dir.create(paste0(to.data, temp.overlay.folder, file_names[i]), showWarnings = F)
    traj.data_tmp <- subset(traj.data, file == file_names[i])
    j <- 1
    
    if (type == "traj") {
      while (j <= max(traj.data$frame)) {
        jpeg(paste(to.data, temp.overlay.folder, file_names[i], "/", "frame_", j, ".jpg", sep = ""), width = as.numeric(width), height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        
        if (predict_spec==F){
        
        print <- subset(traj.data_tmp, traj.data_tmp$frame <= j, select = c("X", "Y", "trajectory"))

        
        ## plot the particle(s) so long as there are some
        if (length(print[, X]) != 0) {
          plot(print$X, print$Y, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 15, cex = 1, asp = 1)
        }
        
        ## otherwise just plot the empty frame
        if (length(print[, X]) == 0) {
          plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 6, asp = 1)
        }
        }
        
        if (predict_spec==T){
          
          print <- subset(traj.data_tmp,traj.data_tmp$frame <= j, select=c("X","Y","trajectory","predict_spec"))

          
          ## plot the particle(s) so long as there are some
          if (length(print[, X]) != 0) {
            plot(print$X, print$Y, xlim=c(0, as.numeric(width)), ylim=c(as.numeric(height), 0),  col=as.factor(print$predict_spec), pch=15, cex=1, asp=1)
          }
          
          ## otherwise just plot the empty frame
          if (length(print[, X]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 1, asp = 1)
          }
        }
        
        dev.off()
        j <- j + 1
      }
    }
    
    if (type == "label") {
      while (j <= max(traj.data$frame)) {
        jpeg(paste(to.data, temp.overlay.folder, file_names[i], "/", "frame_", 
                   j, ".jpg", sep = ""), width = as.numeric(width), height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        
        if (predict_spec==F){
        
        print <- subset(traj.data_tmp, traj.data_tmp$frame == j, select = c("X", "Y", "trajectory"))
        
        ## plot the particle(s) so long as there are some
        if (length(print[, X, ]) != 0) {
          plot(print$X, print$Y, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 6, asp = 1)
          text(print$X, print$Y - 20, print$trajectory, cex = 2, col = "red")
        }
        
        ## otherwise just plot the empty frame
        if (length(print[, X,]) == 0) {
          plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 6, asp = 1)
        }
        }
        
        if (predict_spec==T){
          

          print <- subset(traj.data_tmp,traj.data_tmp$frame == j, select=c("X","Y","trajectory","predict_spec"))
                   
          ## plot the particle(s) so long as there are some
          if (length(print[, X, ]) != 0) {
            plot(print$X, print$Y, xlim=c(0,as.numeric(width)), ylim=c(as.numeric(height), 0), col=as.factor(print$predict_spec), pch=1, cex=6, asp=1)
            text(print$X, print$Y-20,print$trajectory,cex=2,col=as.numeric(print$predict_spec))
            }
          
          ## otherwise just plot the empty frame
          if (length(print[, X, ]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, 
                 cex = 6, asp = 1)
          }
        }
        
        dev.off()
        j <- j + 1
      }
    }
  }
  
  ## copy master copy of ImageJ macro there for treatment
  if (.Platform$OS.type == "windows") 
    text <- readLines(paste0(system.file(package="bemovi"), "/","ImageJ_macros/Video_overlay.ijm"),warn = FALSE)
  if (.Platform$OS.type == "unix") 
    text <- readLines(paste0(system.file(package="bemovi"), "/","ImageJ_macros/Video_overlay.ijm"))
  
  text[grep("video_input = ", text)] <- paste("video_input = ", "'", paste0(to.data, raw.video.folder), "';", sep = "")
  text[grep("overlay_input = ", text)] <- paste("overlay_input = ", "'", paste0(to.data, temp.overlay.folder), "';", sep = "")
  text[grep("overlay_output = ", text)] <- paste("overlay_output = ", "'", paste0(to.data, overlay.folder), "';", sep = "")
  text[grep("lag =", text)] <- paste("lag = ", difference.lag, ";", sep = "") 
  text[grep("Enhance Contrast", text)] <- paste("run(\"Enhance Contrast...\", \"saturated=", contrast.enhancement, " process_all\");", sep = "")
  if (predict_spec==T){text[grep("RGB Color", text)] <- paste('run(\"RGB Color\");')}

    ## re-create ImageJ macro for batch processing of video files with ParticleTracker
  if (.Platform$OS.type == "windows") 
    writeLines(text, con = paste(to.data, ijmacs.folder, "Video_overlay_tmp.ijm", sep = ""))
  if (.Platform$OS.type == "unix") {
   # ijmacs.folder1 <- sub(raw.video.folder, ijmacs.folder, video.dir)
    writeLines(text, con = paste(to.data, ijmacs.folder, "/Video_overlay_tmp.ijm", sep = ""))
  }
  
  ## create directory to store overlays
  dir.create(paste0(to.data, overlay.folder), showWarnings = F)
  
  ## call IJ macro to merge original video with the trajectory data
  if (.Platform$OS.type == "unix") 
    cmd <- paste0("java -Xmx", memory, "m -jar ", IJ.path, "/ij.jar", " -ijpath ", IJ.path, " -macro ", 
                  paste0("'", paste0(to.data, ijmacs.folder), "Video_overlay_tmp.ijm", "'"))
  
  if (.Platform$OS.type == "windows") 
    cmd <- paste0("\"", IJ.path, "\""," -macro ","\"", paste0(gsub("/", "\\\\", paste0(to.data, ijmacs.folder))), "Video_overlay_tmp.ijm", "\"")
    
  ## run ImageJ macro
  system(cmd)
    
}