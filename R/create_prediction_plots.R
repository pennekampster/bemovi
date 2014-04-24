#' Function to create an overlay between the raw data and the trajectories; different colours are used to indicate the different
#' species identities
#'  
#' A function to overlay the trajectories of different species with different colours with the original video 
#' using plots created in R and then processed in ImageJ
#' @param data A dataframe containing the X- and Y-coordinates, the frame, the filename from which the data was extracted, 
#' the unique trajectory ID and a column (predict_spec) with the species labels
#' @param path Path to the output saved from the ParticleLinker and the raw video directory
#' @param width The width of the raw video
#' @param height The height of the raw video
#' @param difference_lag Numeric value specifying the offset between two video frames to 
#' compute the difference image
#' @param memory Numeric value specifying the amount of memory available to ImageJ
#' @export

create_prediction_plots <- function(data,path,width,height,difference.lag,original.vid.contrast.enhancement = 1, memory = memory.alloc){

  trajectory.data <- data
  video.dir <- paste(to.data, raw.video.folder, sep="")
  file_names <- unique(trajectory.data$file)  
  ## change path for output
  dir.create(paste0(path,prediction.folder))
  for (i in seq(1:length(file_names))){
    dir.create(paste0(path,prediction.folder,file_names[i],"/"))
    trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
    j<- 0
    while(j < max(trajectory.data$frame)+1){
      jpeg(paste(path,prediction.folder,as.character(file_names[i]),"/","frame_",j,".jpg",sep=""), width = as.numeric(width), height = as.numeric(height), quality = 100)
      par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
      print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame == j, select=c("X","Y","trajectory","predict_spec"))
      plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col=print$predict_spec, pch=1, cex=6, asp=1)
      text(print$Y, print$X+as.numeric(height)-20,print$traject,cex=2,col=as.numeric(print$predict_spec))
      dev.off()
      j <- j+1}}
  
  ## copy master copy of ImageJ macro there for treatment
  if(.Platform$OS.type == "windows")
    text <- readLines(paste0(to.code,"ImageJ macros/Video_overlay.ijm"),warn = FALSE)
  if(.Platform$OS.type == "unix")
    text <- readLines(paste0(to.code,"ImageJ macros/Video_overlay.ijm"))
    #text <- readLines(paste(to.code, "ImageJ macros/Prediction_overlay.ijm", sep=""))
    
  ## use regular expression to insert input and output directory
  text[grep("avi_input = ", text)] <- paste("avi_input = ","'", paste0(path,raw.video.folder),"';", sep = "")
  text[grep("overlay_input = ", text)] <- paste("overlay_input = ","'", paste0(path,prediction.folder),"';", sep = "")
  text[grep("overlay_output = ", text)] <- paste("overlay_output = ","'", paste0(path,prediction.folder2),"';", sep = "")
  text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
  text[grep("Enhance Contrast", text)] <- paste("run(\"Enhance Contrast...\", \"saturated=", original.vid.contrast.enhancement, 
                                                " process_all\");", sep = "")
    
  ## re-create ImageJ macro for batch processing of video files with ParticleTracker
  if(.Platform$OS.type == "windows")
    writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Prediction_overlay_tmp.ijm",sep=""),sep="\n")
  if(.Platform$OS.type == "unix") {
    ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
    writeLines(text,con=paste(ijmacs.folder, "/Prediction_overlay_tmp.ijm",sep=""))
  }
  
  ## create directory to store overlays
  dir.create(paste0(path,prediction.folder2))
  
  ## call IJ macro to merge original video with the trajectory data
  if(.Platform$OS.type == "unix"){
    cmd <- paste0("java -Xmx",memory,"m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste0(sub(raw.video.folder,"ijmacs",video.dir), "/Prediction_overlay_tmp.ijm"))
  }
  if(.Platform$OS.type == "windows"){
    cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Prediction_overlay_tmp.ijm')}
  
  ## run ImageJ macro
  system(cmd)
  
  ## delete temporary file after execution
  if(.Platform$OS.type == "windows"){
    file.remove("C:/Program Files/Fiji.app/macros/Prediction_overlay_tmp.ijm")}
}
