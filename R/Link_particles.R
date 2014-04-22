#' Function to link the particle coordinates through time
#' 
#' The function converts the XY-coordinates of the ParticleAnalyzer into a temporary structure (e.g. folder 
#' with all coordinates per frame in a separate text file) that can be read by the standalone ParticleLinker
#' @param path Path to the directory where the ParticleAnalyzer output is saved (as text files)
#' @param memory Numeric value specifying the amount of memory available to the ParticleLinker
#' @param linkrange Numeric value passed to the ParticleLinker specifying the range of adjacent frames which
#' are taken into account when a trajectory is assembled 
#' @param disp A numeric value that specifies the maximum displacement of a given particle between two frames
#' @return Returns a text file which contains the X- and Y-coordinates, the frame and a trajectory ID
#' @export
Link_particles <- function(to.data, particle.data.folder, trajectory.data.folder, memory = memory.alloc, linkrange = 5, 
                           disp = 20) {
  
  PA_output_dir <- paste0(to.data, particle.data.folder)
  traj_out.dir <- paste0(to.data, trajectory.data.folder)
  
  dir.create(traj_out.dir, showWarnings = F)
  all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
  
  for (j in 1:length(all.files)) {
    
    
    PA_data <- read.table(paste0(PA_output_dir, "/", all.files[j]), sep = "\t", header = T)
    
    ## only attempt particle linking if particles were detected in the video note: not sure what would happen if only one
    ## particle was found in one frame
    if (length(PA_data[, 1]) > 0) {
      
      dir <- gsub(".cxd", "", sub(".ijout.txt", "", all.files[j]))
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
                      " -jar ", to.particlelinker.owen, "/ParticleLinker.jar ", dir, " \"", traj_out.dir, "/ParticleLinker_", 
                      all.files[j], ".txt\"")
        system(cmd)
      }
      
      if (.Platform$OS.type == "windows") {
        cmd <- paste0("C:/Progra~2/java/jre7/bin/javaw.exe -Xmx", memory, "m -Dparticle.linkrange=", linkrange, 
                      " -Dparticle.displacement=", disp, " -jar ", to.particlelinker.frank, "/ParticleLinker.jar ", dir, " \"", 
                      traj_out.dir, "/ParticleLinker_", all.files[j], ".txt\"")
        system(cmd)
      }
      
      # delete working dir
      unlink(dir, recursive = TRUE)
      
    }
    
    if (length(PA_data[, 1]) == 0) {
      print(paste("***** No particles were detected in video", all.files[j], " -- check the raw video and also threshold values"))
    }
  }
}