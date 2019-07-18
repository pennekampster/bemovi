#' Multicore version of function to link the particle coordinates through time (modifications of original function 
#' by Jason Griffiths)
#' 
#' The function takes the XY-coordinates provided by the ImageJ ParticleAnalyzer and uses a standalone version of the ImageJ MOSAIC plugin ParticleLinker to create trajectories.
#' The parallel package is used to allow multiple cores to be used to for executing the particlelinker executable
#' The function requires the creation of temporary files, which are subsequently deleted. 
#' @param to.data path to the working directory 
#' @param particle.data.folder directory where the ParticleAnalyzer output is saved (as text files) (temporary)
#' @param trajectory.data.folder directory where the ParticleLinker is saved (as text files) (temporary???)
#' @param memory numeric value specifying the max amount of memory allocated to the ParticleLinker (defaults to 512)
#' @param linkrange numeric value passed to the ParticleLinker specifying the range of adjacent frames which
#' are taken into account when a trajectory is re-constructed 
#' @param disp numeric value that specifies the maximum displacement of a given particle between two frames
#' @param start_vid numeric value to indicate whether the linking should be started with a video other than the first
#' @return Returns a single text file per video containing the X- and Y-coordinates, the frame and a trajectory ID. The files are than automatically merged into a data.table
#' with the movement metrics for each fix appended to the original data (NB: movement metrics often need two (e.g. step length), sometimes even 
#' three (e.g., turning angles) fixes; fixes for which metrics cannot be calculated are padded with NA). The movement parameters are the step length, the step duration, 
#' the step speed (step length/step duration), the gross displacement as the cumulative sum of the step lengths, the net displacement between the first fix of a given trajectory 
#' and the current fix and finally the relative angle (turning angle) and absolute angle (in radians). For details on these metrics, please refer to a dedicated textbook 
#' (e.g. Turch (1998): Quantitative Analysis of Movement: Measuring and Modeling Population Redistribution in Animals and Plants, Sinauer Associates, Sunderland).
#' @import parallel
#' @export

link_particles_mc<-function(to.data, particle.data.folder, trajectory.data.folder, linkrange = 1, disp = 10, start_vid = 1, memory = 512){
  
  PA_output_dir <- paste0(to.data, particle.data.folder)
  traj_out.dir <- paste0(to.data, trajectory.data.folder)
  dir.create(traj_out.dir, showWarnings = F)
  all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
  PA_output.file <- dir(PA_output_dir, pattern = "particle.RData")
    
  load(file=paste0(PA_output_dir,PA_output.file))
  #morphology.data <- data.table(morphology.data)
  #setkey(morphology.data,file)
  file_code <- unlist(strsplit(all.files ,split=".ijout.txt"))

#the makeCluster function is used to set up clusters
#the clusterExport function is used to transfer objects from function environments to cluster environments
#the clusterApplyLB function is used to perform function in parallel;its syntax is similar to other apply loops but browser cant be used and objects must be specially supplied
#LB stands for load balancing : useful for when jobs are of differing sizes

    # detect number of cores to use
    n.cores<-detectCores(all.tests = FALSE, logical = TRUE)

    ## set up cluster
    cl <- makeCluster(n.cores)
    ## which objects do all clusters need to know???
    clusterExport(cl, varlist=c("morphology.data",
                                "file_code",
                                "to.data",
                                "all.files",  
                                "raw.video.folder",
                                "trajectory.data.folder", 
                                "linkrange", "disp", "start_vid", "memory",
                                "to.particlelinker",
                                 "java.path", "PA_output_dir","traj_out.dir"),
                                 environment())
 
    # run the cluster job
    clusterApplyLB(cl, start_vid:length(all.files),fun=function(j){
    	#lapply(start_vid:length(all.files),FUN=function(j){
        PA_data <- morphology.data[morphology.data$file==paste0(file_code[j]),]

        if (length(PA_data[, "obs"]) > 0 & is.na(PA_data[1, "obs"])==F) {
            dir <- paste0(to.data, gsub(".cxd", "", sub(".ijout.txt", "", all.files[j])))
            dir.create(dir)
            for (i in 1:max(PA_data$Slice)) {
	      	    frame <- PA_data[PA_data$Slice==i,names(PA_data)%in% c("X","Y")] 
	      	    
	      	    #may cause failure as Slice may be expected as object
                frame$Z <- 0
                sink(paste0(dir, "/frame_", sprintf("%04d", i - 
                  1), ".txt"))
                cat(paste0("frame ", i - 1))
                cat("\n")
                sink()
                write.table(frame, file = paste0(dir, "/frame_", 
                  sprintf("%04d", i - 1), ".txt"), append = T, 
                  col.names = F, row.names = F)
            }
            if (.Platform$OS.type == "unix") {
                cmd <- paste0("java -Xmx", memory, "m -Dparticle.linkrange=", 
                  linkrange, " -Dparticle.displacement=", disp, 
                  " -jar ", " \"", to.particlelinker, "/ParticleLinker.jar", 
                  "\" ", "'", dir, "'", " \"", traj_out.dir, 
                  "/ParticleLinker_", all.files[j], "\"")
                system(paste0(cmd, " \\&"))
            }
            if (.Platform$OS.type == "windows") {
                cmd <- paste0(java.path, " -Xmx", 
                  memory, "m -Dparticle.linkrange=", linkrange, 
                  " -Dparticle.displacement=", disp, " -jar", 
                  gsub("/", "\\\\", paste0(" \"", to.particlelinker, 
                    "/ParticleLinker.jar")), "\" ", gsub("/", 
                    "\\\\", paste0(" ", "\"", dir, "\"")), gsub("/", 
                    "\\\\", paste0(" ", "\"", traj_out.dir, "/ParticleLinker_", 
                      all.files[j], "\"")))
                system(cmd)
            }
            unlink(dir, recursive = TRUE)
        }
        if (length(PA_data[, "obs"]) == 0 | is.na(PA_data[1, "obs"]) ){
            print(paste("***** No particles were detected in video", 
                all.files[j], " -- check the raw video and also threshold values"))
        }
        return(print(head(PA_data)))
    })  
    stopCluster(cl)
  
   #collect and organize the output & calculate movement measures
   data <- organise_link_data(to.data, trajectory.data.folder)
   out <- calculate_mvt(data, to.data, trajectory.data.folder, pixel_to_scale, fps)
}