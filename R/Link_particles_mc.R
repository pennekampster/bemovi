### Modification of F.Pennekamps link_particle function from the bemovi package by Jason Griffiths.

# the parallel package is used to allow multiple cores to be used to simultaneously perform a particle linker executable
# function does not use forking and so is cross platform

#the makeCluster function is used to set up clusters
#the clusterExport function is used to transfer objects from function environments to cluster environments
#the clusterApplyLB function is used to perform function in parallel;its syntax is similar to other apply loops but browser cant be used and objects must be specially supplied
#LB stands for load balancing : useful for when jobs are of differing sizes



require(parallel)

n.cores<-detectCores(all.tests = FALSE, logical = TRUE)

link_particles_mc<-function (to.data, particle.data.folder, trajectory.data.folder, 
    linkrange = 1, disp = 10, start_vid = 1, memory = 512) 
{
    PA_output_dir <- paste0(to.data, particle.data.folder)
  traj_out.dir <- paste0(to.data, trajectory.data.folder)
  dir.create(traj_out.dir, showWarnings = F)
  all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
  PA_output.file <- dir(PA_output_dir, pattern = ".RData")
    
  load(file=paste0(PA_output_dir,PA_output.file))
 # morphology.data <- data.table(morphology.data)
#  setkey(morphology.data,file)
  file_code <- unlist(strsplit(all.files ,split=".ijout.txt"))


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
                                "to.particlelinker","PA_output_dir","traj_out.dir"),
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
                cmd <- paste0("C:/Progra~2/java/jre7/bin/javaw.exe -Xmx", 
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
   calculate_mvt(data, to.data, trajectory.data.folder, pixel_to_scale,fps)
}