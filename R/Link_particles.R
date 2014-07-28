#' Function to link the particle coordinates through time
#' 
#' The function converts the XY-coordinates of the ParticleAnalyzer into a temporary structure (e.g. folder 
#' with all coordinates per frame in a separate text file) that can be read by the standalone ParticleLinker
#' @param to.data path to the working directory 
#' @param particle.data.folder directory where the ParticleAnalyzer output is saved (as text files)
#' @param trajectory.data.folder directory where the ParticleLinker is saved (as text files)
#' @param memory numeric value specifying the max amount of memory allocated to the ParticleLinker
#' @param linkrange numeric value passed to the ParticleLinker specifying the range of adjacent frames which
#' are taken into account when a trajectory is re-constructed 
#' @param disp numeric value that specifies the maximum displacement of a given particle between two frames
#' @param start_vid numeric value to indicate whether the linking should be started with a video other than the first
#' @return Returns a single text file per video containing the X- and Y-coordinates, the frame and a trajectory ID
#' @export
link_particles <- function(to.data, particle.data.folder, trajectory.data.folder, memory = memory.alloc, linkrange = 1, disp = 20, start_vid = 1) {
  
  PA_output_dir <- paste0(to.data, particle.data.folder)
  traj_out.dir <- paste0(to.data, trajectory.data.folder)
  
  dir.create(traj_out.dir, showWarnings = F)
  all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
  
  for (j in start_vid:length(all.files)) {
    
    PA_data <- read.table(paste0(PA_output_dir, "/", all.files[j]), sep = "\t", header = T)
    
    ## only attempt particle linking if particles were detected in the video note: not sure what would happen if only one
    ## particle was found in one frame
    if (length(PA_data[, 1]) > 0) {
      
      dir <- paste0(to.data, gsub(".cxd", "", sub(".ijout.txt", "", all.files[j])))
      dir.create(dir)
      
      for (i in 1:max(PA_data$Slice)) {
        frame <- subset(PA_data, Slice == i)[, c(6, 7)]
        frame$Z <- rep(0, length(frame[, 1]))
        sink(paste0(dir, "/frame_", sprintf("%04d", i - 1), ".txt"))
        cat(paste0("frame ", i - 1))
        cat("\n")
        sink()
        write.table(frame, file = paste0(dir, "/frame_", sprintf("%04d", i - 1), ".txt"), append = T, col.names = F, 
                    row.names = F)
      }
      
      ## run ParticleLinker
      if (.Platform$OS.type == "unix") {
        cmd <- paste0("java -Xmx", memory, "m -Dparticle.linkrange=", linkrange, " -Dparticle.displacement=", disp, 
                      " -jar ", to.particlelinker, "/ParticleLinker.jar ", "'", dir, "'", " \"", traj_out.dir,"/ParticleLinker_", 
                      all.files[j],"\"")
        system(cmd)
      }
      
      if (.Platform$OS.type == "windows") {
        cmd <- paste0("C:/Progra~2/java/jre7/bin/javaw.exe -Xmx", memory, "m -Dparticle.linkrange=", linkrange, 
                      " -Dparticle.displacement=", disp, " -jar ", to.particlelinker, "/ParticleLinker.jar ", "'", dir, "'", " \"", 
                      traj_out.dir, "/ParticleLinker_", all.files[j], "\"")
        system(cmd)
      }
      
      # delete working dir
      unlink(dir, recursive = TRUE)
                
    }
    
    if (length(PA_data[, 1]) == 0) {
      print(paste("***** No particles were detected in video", all.files[j], " -- check the raw video and also threshold values"))
      
    }
    
  }
  
  # merge all files into one database
  data <- organise_link_data(to.data, trajectory.data.folder) 
  
  #calculate movement metrics for each fix and save to disk
  calculate_mvt(data,to.data,trajectory.data.folder)
}