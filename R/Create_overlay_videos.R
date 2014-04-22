#' Function to create an overlay between the raw data and the extracted trajectories
#' 
#' A function to overlay the trajectories and the original video using plots created in R and then processed in 
#' ImageJ; two different visualization types are available
#' @param path Path to the output saved from the ParticleLinker and the raw video directory
#' @param temp.overlay.folder A temporary directory to save the overlay created with R
#' @param overlay.folder A folder where the overlay videos are saved
#' @param width The width of the raw video
#' @param height The height of the raw video
#' @param difference_lag Numeric value specifying the offset between two video frames to 
#' compute the difference image
#' @param type A character string indicating the visualization type (i.e. 'label' or 'traj'): either the overlay
#' is showing the trajectory ID and outlines the detected particle (type='label') or the whole trajectory
#' remains plotted (type='traj').
#' @param original.vid.contrast.enhancement A numeric value to increase the contrast of the raw video
#' @param memory Numeric value specifying the amount of memory available to ImageJ
#' @export
Create_overlay_videos <- function(to.data, trajectory.data.folder, raw.video.folder, temp.overlay.folder, overlay.folder, 
                                  width, height, difference.lag, type = "traj", original.vid.contrast.enhancement = 1, memory = memory.alloc) {
  
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  
  trackdata.dir <- paste(to.data, trajectory.data.folder, sep = "")
  
  trajectory.data <- as.data.frame(read.table(paste(trackdata.dir, "trajectory.data.txt", sep = ""), header = TRUE, sep = "\t"))
  file_names <- unique(trajectory.data$file)
  
  ## change path for output
  dir.create(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir), showWarnings = F)
  for (i in 1:length(file_names)) {
    dir.create(paste0(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir), file_names[i]), showWarnings = F)  #sub('Traj_','',filename[1]),sep='/'))
    trajectory.data_tmp <- subset(trajectory.data, file == file_names[i])
    j <- 0
    if (type == "traj") {
      while (j < max(trajectory.data$frame) + 1) {
        jpeg(paste(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir), file_names[i], "/", "frame_", 
                   j, ".jpg", sep = ""), width = as.numeric(width), height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame <= j, select = c("X", "Y", "trajectory"))
        
        ## plot the particle(s) so long as there are some
        if (length(print[, 1]) != 0) {
          plot(print$Y, print$X + as.numeric(height), xlim = c(0, as.numeric(width)), ylim = c(0, as.numeric(height)), 
               col = "#FFFF00", pch = 15, cex = 1, asp = 1)
        }
        
        ## otherwise just plot the empty frame
        if (length(print[, 1]) == 0) {
          plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(0, as.numeric(height)), col = "blue", pch = 1, 
               cex = 6, asp = 1)
        }
        dev.off()
        j <- j + 1
      }
    }
    
    if (type == "label") {
      while (j < max(trajectory.data$frame) + 1) {
        jpeg(paste(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir), file_names[i], "/", "frame_", 
                   j, ".jpg", sep = ""), width = as.numeric(width), height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame == j, select = c("X", "Y", "trajectory"))
        
        ## plot the particle(s) so long as there are some
        if (length(print[, 1]) != 0) {
          plot(print$Y, print$X + as.numeric(height), xlim = c(0, as.numeric(width)), ylim = c(0, as.numeric(height)), 
               col = "blue", pch = 1, cex = 6, asp = 1)
          text(print$Y, print$X + as.numeric(height) - 20, print$traject, cex = 2, col = "red")
        }
        
        ## otherwise just plot the empty frame
        if (length(print[, 1]) == 0) {
          plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(0, as.numeric(height)), col = "blue", pch = 1, 
               cex = 6, asp = 1)
        }
        dev.off()
        j <- j + 1
      }
    }
  }
  
  ## copy master copy of ImageJ macro there for treatment
  if (.Platform$OS.type == "windows") 
    text <- readLines(paste0(to.code,"ImageJ macros/Video_overlay.ijm"),warn = FALSE)
  if (.Platform$OS.type == "unix") 
    text <- readLines(paste(to.code, "ImageJ macros/Video_overlay.ijm", sep = ""))
  
  
  ## use regular expression to insert input and output directory and contrast enhancement of original video
  text[grep("avi_input = ", text)] <- paste("avi_input = ", "'", sub(trajectory.data.folder, raw.video.folder, trackdata.dir), 
                                            "';", sep = "")
  text[grep("overlay_input = ", text)] <- paste("overlay_input = ", "'", sub(trajectory.data.folder, temp.overlay.folder, 
                                                                             trackdata.dir), "';", sep = "")
  text[grep("overlay_output = ", text)] <- paste("overlay_output = ", "'", sub(trajectory.data.folder, overlay.folder, 
                                                                               trackdata.dir), "';", sep = "")
  text[grep("lag =", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("Enhance Contrast", text)] <- paste("run(\"Enhance Contrast...\", \"saturated=", original.vid.contrast.enhancement, 
                                                " process_all\");", sep = "")
  
  
  ## re-create ImageJ macro for batch processing of video files with ParticleTracker
  if (.Platform$OS.type == "windows") 
    writeLines(text, con = paste("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm", sep = ""), sep = "\n")
  if (.Platform$OS.type == "unix") {
    ijmacs.folder1 <- sub(raw.video.folder, ijmacs.folder, video.dir)
    writeLines(text, con = paste(ijmacs.folder1, "/Video_overlay_tmp.ijm", sep = ""))
  }
  
  ## create directory to store overlays
  dir.create(sub(trajectory.data.folder, overlay.folder, trackdata.dir), showWarnings = F)
  
  ## call IJ macro to merge original video with the trajectory data
  if (.Platform$OS.type == "unix") 
    cmd <- paste0("java -Xmx", memory, "m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", 
                  paste0(sub(raw.video.folder, ijmacs.folder, video.dir), "Video_overlay_tmp.ijm"))
  
  if (.Platform$OS.type == "windows") 
    cmd <- c("\"C:/Program Files/FIJI.app/fiji-win64.exe\" -macro Video_overlay_tmp.ijm")
  
  ## run ImageJ macro
  system(cmd)
  
  ## delete temporary file after execution
  if (.Platform$OS.type == "windows") 
    file.remove("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm")
  
}