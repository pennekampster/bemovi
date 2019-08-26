#' Function to link the particle coordinates through time
#' 
#' The function takes the XY-coordinates provided by the ImageJ ParticleAnalyzer and uses a standalone version of the ImageJ MOSAIC plugin ParticleLinker to create trajectories. 
#' This requires some creation of temporary files, which are subsequently deleted.
#' @param to.data path to the working directory 
#' @param particle.data.folder directory where the ParticleAnalyzer output is saved (as text files) (temporary)
#' @param trajectory.data.folder directory where the ParticleLinker is saved (as text files) (temporary???)
#' @param memory numeric value specifying the max amount of memory allocated to the ParticleLinker (defaults to 512)
#' @param linkrange numeric value passed to the ParticleLinker specifying the range of adjacent frames which
#' are taken into account when a trajectory is re-constructed 
#' @param disp numeric value that specifies the maximum displacement of a given particle between two frames
#' @param start_vid numeric value to indicate whether the linking should be started with a video other than the first
#' @param to.particlelinker path to ParticleLinker jar file
#' @param pixel_to_scale TODO
#' @param fps Frames Per Second of the video
#' 
#' @return Returns a single text file per video containing the X- and Y-coordinates, the frame and a trajectory ID. The files are than automatically merged into a data.table
#' with the movement metrics for each fix appended to the original data (NB: movement metrics often need two (e.g. step length), sometimes even 
#' three (e.g., turning angles) fixes; fixes for which metrics cannot be calculated are padded with NA). The movement parameters are the step length, the step duration, 
#' the step speed (step length/step duration), the gross displacement as the cumulative sum of the step lengths, the net displacement between the first fix of a given trajectory 
#' and the current fix and finally the relative angle (turning angle) and absolute angle (in radians). For details on these metrics, please refer to a dedicated textbook 
#' (e.g. Turch (1998): Quantitative Analysis of Movement: Measuring and Modeling Population Redistribution in Animals and Plants, Sinauer Associates, Sunderland).
#' @export
link_particles <- function(
  to.data, 
  particle.data.folder, 
  trajectory.data.folder, 
  linkrange = 1, 
  disp = 10, 
  start_vid = 1, 
  memory = 512,
  to.particlelinker,
  pixel_to_scale,
  fps
) {

  Slice<-java.path<-NULL
  
  if(!exists("to.particlelinker")) stop("Path to ParticleLinker not found. Please specify path in global options.")
  
  PA_output_dir <- file.path(to.data, particle.data.folder)
  traj_out.dir <- file.path(to.data, trajectory.data.folder)
  
  dir.create(traj_out.dir, showWarnings = F)
  all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
  
  for (j in start_vid:length(all.files)) {
    
    PA_data <- read.table(
      file.path(PA_output_dir, all.files[j]), 
      sep = "\t", 
      header = TRUE
    )
    
    ## only attempt particle linking if particles were detected in the video note: not sure what would happen if only one
    ## particle was found in one frame
    if (length(PA_data[, 1]) > 0) {
      
      dir <- file.path( to.data, gsub(".cxd", "", sub(".ijout.txt", "", all.files[j])) )
      dir.create(dir)
      
      for (i in 1:max(PA_data$Slice)) {
        frame <- subset(PA_data, Slice == i)[, c(6, 7)]
        frame$Z <- rep(0, length(frame[, 1]))
        sink( file.path(dir, paste0("frame_", sprintf("%04d", i - 1), ".txt")) )
        cat(paste0("frame ", i - 1))
        cat("\n")
        sink()
        write.table(frame, file = file.path(dir, paste0("frame_", sprintf("%04d", i - 1), ".txt")), append = TRUE, col.names = FALSE, 
                    row.names = FALSE)
      }
      
      ## run ParticleLinker
      if (.Platform$OS.type == "unix") {
        cmd <- paste0(
          "java",
          " -Xmx", memory, "m",
          " -Dparticle.linkrange=", linkrange, 
          " -Dparticle.displacement=", disp, 
          " -jar ", " \"", to.particlelinker, "/ParticleLinker.jar","\" ", "'", dir, "'", " \"", traj_out.dir,"/ParticleLinker_", 
                      all.files[j],"\""
        )
        system(paste0(cmd, " \\&"))
      }
      
      if (.Platform$OS.type == "windows") {
        
        if(!exists("java.path")) stop("Java path not found. Please specify path in global options.")
        
      # previously hardcoded as "C:/Progra~2/java/jre7/bin/javaw.exe"
       cmd <- paste0(java.path, " -Xmx", memory,"m -Dparticle.linkrange=", linkrange, " -Dparticle.displacement=", disp," -jar",
                      gsub("/","\\\\", paste0(" \"" ,to.particlelinker,"/ParticleLinker.jar")),"\" ",
                      gsub("/","\\\\", paste0(" ","\"" ,dir,"\"")),
                      gsub("/","\\\\", paste0(" ","\"", traj_out.dir, "/ParticleLinker_", all.files[j], "\"")))
       
      system(cmd)
      }
      
      #delete working dir
      unlink(dir, recursive = TRUE)
                
    }
    
    if (length(PA_data[, 1]) == 0) {
      print(paste("***** No particles were detected in video", all.files[j], " -- check the raw video and also threshold values"))
      
    }
    
  }
  
  # merge all files into one database
  data <- organise_link_data(to.data, trajectory.data.folder) 
  
  #calculate movement metrics for each fix and save to disk
  calculate_mvt(data,to.data,trajectory.data.folder,pixel_to_scale,fps)
}
