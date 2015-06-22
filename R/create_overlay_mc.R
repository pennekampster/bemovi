#' Multicore version of the function to create a new video with the extracted trajectories overlayed 
#' onto the original video (modifications of original function by Jason Griffiths)
#' 
#' A function to overlay the extracted trajectories onto the original video, using plots created in R and then processed in 
#' ImageJ; two visualization types are available. The parallel package is used to allow multiple cores to be used
#' when R writes out the stack of images used for overlaying the original video files. The actual blending of image
#' files in ImageJ is not parallelized.
#' 
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
#' @param IJ.path path to ImageJ executable 
#' @param memory numeric value specifying the amount of memory available to ImageJ (defaults to 512)
#' @param n.cores either numeric value specifying the number of cores to use or character string "detect" to trigger automatic specification of cores available on machine
#' @import parallel
#' @export

create_overlays_mc <-  function (to.data, merged.data.folder, raw.video.folder, temp.overlay.folder, 
          overlay.folder, width, height, difference.lag, type = "traj", 
          predict_spec = F, contrast.enhancement = 0, IJ.path, memory = 512, n.cores="detect") {
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  load(file = paste(to.data, merged.data.folder, "Master.RData", sep = "/"))
  file_names <- unique(trajectory.data$file)
  dir.create(paste0(to.data, temp.overlay.folder), showWarnings = F)

  #the makeCluster function is used to set up clusters
  #the clusterExport function is used to transfer objects from function environments to cluster environments
  #the clusterApplyLB function is used to perform function in parallel;its syntax is similar to other apply loops but browser cant be used and objects must be specially supplied
  #LB stands for load balancing : useful for when jobs are of differing sizes
  
  if (n.cores=="detect"){
  # detect number of cores to use
  all.cores<-detectCores(all.tests = FALSE, logical = TRUE)
  # leave at least one core for system tasks
  n.cores<- all.cores-1
  }
  
 # set up cluster
 cl <- makeCluster(n.cores)
 # which objects do all clusters need to know???
 clusterExport(cl, varlist=c("to.data", "merged.data.folder", "raw.video.folder", "temp.overlay.folder", "overlay.folder", 
                             "width", "height","difference.lag", "type", "predict_spec", "contrast.enhancement", "IJ.path", 
                            "memory", "trajectory.data", "file_names", "n.cores"), environment())
 
  # run the cluster
  #clusterApplyLB(cl,1:length(file_names), fun=function(i){
 	parLapplyLB(cl,1:length(file_names), fun=function(i){

    dir.create(paste0(to.data, temp.overlay.folder, file_names[i]),  showWarnings = F)
    trajectory.data_tmp <- subset(trajectory.data, file == file_names[i])
    j <- 1
    if (type == "traj") {
      while (j <= max(trajectory.data$frame)) {
        jpeg(paste(to.data, temp.overlay.folder, file_names[i], 
                   "/", "frame_", j, ".jpg", sep = ""), width = as.numeric(width), 
             height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        if (predict_spec == F) {
          print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame <= 
                            j, select = c("X", "Y", "trajectory"))
          if (length(print[, 1]) != 0) {
            plot(print$X, print$Y, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = "blue", 
                 pch = 15, cex = 1, asp = 1)
          }
          if (length(print[, 1]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = "blue", 
                 pch = 1, cex = 6, asp = 1)
          }
        }
        if (predict_spec == T) {
          print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame <= 
                            j, select = c("X", "Y", "trajectory", "predict_spec"))
          if (length(print[, 1]) != 0) {
            plot(print$X, print$Y, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = as.factor(print$predict_spec), 
                 pch = 15, cex = 1, asp = 1)
          }
          if (length(print[, 1]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = "blue", 
                 pch = 1, cex = 1, asp = 1)
          }
        }
        dev.off()
        j <- j + 1
      }
    }
    if (type == "label") {
      while (j <= max(trajectory.data$frame)) {
        jpeg(paste(to.data, temp.overlay.folder, file_names[i], 
                   "/", "frame_", j, ".jpg", sep = ""), width = as.numeric(width), 
             height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        if (predict_spec == F) {
          print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame == 
                            j, select = c("X", "Y", "trajectory"))
          if (length(print[, "trajectory" ]) != 0) {
            plot(print$X, print$Y, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = "blue", 
                 pch = 1, cex = 6, asp = 1)
            text(print$X, print$Y - 20, print$trajectory, 
                 cex = 2, col = "red")
          }
          if (length(print[, "trajectory"]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = "blue", 
                 pch = 1, cex = 6, asp = 1)
          }
        }
        if (predict_spec == T) {
          print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame == 
                            j, select = c("X", "Y", "trajectory", "predict_spec"))
          if (length(print[, "trajectory" ]) != 0) {
            plot(print$X, print$Y, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = as.factor(print$predict_spec), 
                 pch = 1, cex = 6, asp = 1)
            text(print$X, print$Y - 20, print$trajectory, 
                 cex = 2, col = as.numeric(print$predict_spec))
          }
          if (length(print[, "trajectory" ]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), 
                 ylim = c(as.numeric(height), 0), col = "blue", 
                 pch = 1, cex = 6, asp = 1)
          }
        }
        dev.off()
        j <- j + 1
      }
    }
  })  #end cluster function
  stopCluster(cl)
  
  
  if (.Platform$OS.type == "windows") 
    text <- readLines(paste0(system.file(package = "bemovi"), 
                             "/", "ImageJ_macros/Video_overlay.ijm"), warn = FALSE)
  if (.Platform$OS.type == "unix") 
    text <- readLines(paste0(system.file(package = "bemovi"), 
                             "/", "ImageJ_macros/Video_overlay.ijm"))
  text[grep("avi_input = ", text)] <- paste("avi_input = ", 
                                            "'", paste0(to.data, raw.video.folder), "';", sep = "")
  text[grep("overlay_input = ", text)] <- paste("overlay_input = ", 
                                                "'", paste0(to.data, temp.overlay.folder), "';", sep = "")
  text[grep("overlay_output = ", text)] <- paste("overlay_output = ", 
                                                 "'", paste0(to.data, overlay.folder), "';", sep = "")
  text[grep("lag =", text)] <- paste("lag = ", difference.lag, 
                                     ";", sep = "")
  text[grep("Enhance Contrast", text)] <- paste("run(\"Enhance Contrast...\", \"saturated=", 
                                                contrast.enhancement, " process_all\");", sep = "")
  if (predict_spec == T) {
    text[grep("RGB Color", text)] <- paste("run(\"RGB Color\");")
  }
  if (.Platform$OS.type == "windows") 
    writeLines(text, con = paste(to.data, ijmacs.folder, 
                                 "Video_overlay_tmp.ijm", sep = ""))
  if (.Platform$OS.type == "unix") {
    writeLines(text, con = paste(to.data, ijmacs.folder, 
                                 "/Video_overlay_tmp.ijm", sep = ""))
  }
  dir.create(paste0(to.data, overlay.folder), showWarnings = F)
  if (.Platform$OS.type == "unix") 
    cmd <- paste0("java -Xmx", memory, "m -jar ", IJ.path, 
                  " -ijpath /Applications/ImageJ -macro ", paste0("'", 
                                                                  paste0(to.data, ijmacs.folder), "Video_overlay_tmp.ijm", 
                                                                  "'"))
  if (.Platform$OS.type == "windows") 
    cmd <- paste0("\"", IJ.path, "\"", " -macro ", "\"", 
                  paste0(gsub("/", "\\\\", paste0(to.data, ijmacs.folder))), 
                  "Video_overlay_tmp.ijm", "\"")
  system(cmd)
}