## Check to see if there are any unsupported file types, or file names with two periods
Check_video_file_names <- function(to.data, raw.video.folder)
{
	video.dir <- paste(to.data, raw.video.folder, sep="")
    files <- dir(video.dir)
    ## check for unsupported video file format
    unsupported.files <- files[-c(grep("\\.avi", files), grep("\\.cxd", files))]
    if(length(unsupported.files)>0)
        print(paste("Unsupported video file:", unsupported.files))
    ## check for file with more than one period
    ## I think this previously caused me a problem
    bad.filenames <- files[unlist(lapply(lapply(strsplit(files, "\\."), length), function(x) x>2))]
    if(length(bad.filenames)>0)
        print(paste("Bad video filename (no periods please, except before extension:", bad.filenames))
}

## This function creates a image j macro that can be helpful for checking the thresholds
## used to "threshold" the video and the lag
## It used to do more, but Owen hashed out this functionality.
Check_threshold_values <- function(to.data, raw.video.folder, difference.lag, thresholds=c(10,255)) {
    
    video.dir <- paste(to.data, raw.video.folder, sep="")
    ## generate the folders if not already existing
    ijmacs.folder <- sub(raw.video.folder, ijmacs.folder, video.dir)
    dir.create(ijmacs.folder, showWarnings = FALSE)	
    ##checkthresh.folder <- sub(raw.video.folder,raw.checkthreshold.folder,video.dir)
    ##dir.create(checkthresh.folder, showWarnings = FALSE)
      
    ## copy master copy of ImageJ macro there for treatment
    text <- readLines(paste(to.code, "ImageJ macros/Check_threshold.ijm", sep=""))
    
    ## use regular expression to insert input and output directory
    text[grep("avi_input =", text)] <- paste("avi_input = ","'", video.dir,"';", sep = "")
    ##text[grep("avi_output =", text)] <- paste("avi_output = ","'",checkthresh.folder,"';", sep = "")
    text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
    text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")

    ## re-create ImageJ macro for batch processing of video files with ParticleTracker
    ## perhaps put this in a subdirectory of the data folder?
    ## This is implemented in OSX but not windows, which is as you wrote it
    if(.Platform$OS.type == "windows") 
        writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Check_threshold_tmp.ijm",sep=""),sep="\n")
    if(.Platform$OS.type == "unix") 
        writeLines(text,con=paste(ijmacs.folder, "Check_threshold_tmp.ijm",sep=""))

    ## run to process video files by calling ImageJ / needs fixing for Mac
    ## if(.Platform$OS.type == "unix")
    ##     cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(ijmacs.folder, "Check_threshold_tmp.ijm",sep=""))
    ## if(.Platform$OS.type == "windows")
    ##     cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Check_threshold_tmp.ijm')
    ##system(cmd)

}




## Function to get morphological measurements using the particle analyser in imagej
Locate_and_measure_particles <- function(to.data, raw.video.folder, particle.data.folder,
										 difference.lag, thresholds=c(0,1000)) {

    video.dir <- paste(to.data, raw.video.folder, sep="")

    ## copy master copy of ImageJ macro there for treatment
    text <- readLines(paste(to.code, "ImageJ macros/Video_to_morphology.ijm", sep=""))
    
    ## use regular expression to insert input & output directory as well as difference lag
    text[grep("avi_input = ", text)] <- paste("avi_input = ","'", video.dir,"';", sep = "")
    text[grep("avi_output = ", text)] <- paste("avi_output = ", "'", 
    						sub("1 - raw/", particle.data.folder, video.dir),"';", sep = "")
    text[grep("lag = ", text)] <- paste("lag = ",difference.lag,";", sep = "")
    text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")
    
    
    
    ## re-create ImageJ macro for batch processing of video files with Particle Analyzer
    if(.Platform$OS.type == "windows") 
        writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm",sep=""),sep="\n")
    if(.Platform$OS.type == "unix") {
        dir.create(sub(raw.video.folder, ijmacs.folder, video.dir), showWarnings=F)	
        writeLines(text, con=paste(sub(raw.video.folder, ijmacs.folder, video.dir),
        			"Video_to_morphology_tmp.ijm",sep=""))
    }
    
    ## create directory to store Particle Analyzer data
    dir.create(sub(raw.video.folder, particle.data.folder, video.dir), showWarnings = FALSE)
    
    ## run to process video files by calling ImageJ
    if(.Platform$OS.type == "unix")
        cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub("1 - raw","ijmacs",video.dir), "Video_to_morphology_tmp.ijm",sep=""))
    if(.Platform$OS.type == "windows")
        cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_morphology_tmp.ijm')
    system(cmd)
    
    ## delete temporary file after execution
    if(.Platform$OS.type == "windows")
        file.remove("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm")
    
}



## This function puts all the morphology files (one for each video) into one dataset, and saves it
Organise_particle_data <- function(to.data, particle.data.folder) {
    
    IJ_output.dir <- paste(to.data, particle.data.folder, sep="")
    
    ## the macro file names
    all.files <- dir(path=IJ_output.dir)
    ijout.files <- all.files[grep("ijout", all.files)]
    
    dd <- read.delim(paste(IJ_output.dir, ijout.files[1], sep="//"))
    dd$file <- rep(gsub(".ijout.txt","",ijout.files[1]), length(dd[,1]))
    
    ## change column names because R is replacing missing header
    ## with X causing confusion with real X and Y positions
    colnames(dd) <- c("obs", "Area", "Mean", "Min", "Max", "X", "Y", "Perimeter",
    	"Major", "Minor", "Angle", "Circ.", "Slice", "AR", "Round", "Solidity", "file")
    
    if(length(ijout.files)>2) {
		for(i in 2:length(ijout.files)) {
    		dd.t <- read.delim(paste(IJ_output.dir, ijout.files[i], sep="//"))
        	dd.t$file <- rep(gsub(".ijout.txt","",ijout.files[i]), length(dd.t[,1]))
        	## change column names because R is replacing missing header
        	## with X causing confusion with real X and Y positions
        	colnames(dd.t) <-c("obs", "Area", "Mean", "Min", "Max", "X", "Y", "Perimeter", "Major",
            		"Minor", "Angle", "Circ.", "Slice", "AR", "Round", "Solidity", "file")
        	dd <- rbind(dd, dd.t)
		}
    }
  
	assign("particle.data",dd,envir = .GlobalEnv)
	write.table(particle.data, file = paste(IJ_output.dir,"particle.data.txt", sep = "/"),
				sep = "\t")
}



## Function to convert XY coordinates of the ParticleAnalyzer into a structure
## (e.g. folder with coordinates per frame) 
## that can be read by the standalone ParticleLinker
Link_particles <- function(to.data, particle.data.folder, trajectory.data.folder) {
	
	PA_output_dir <- paste0(to.data, particle.data.folder)
	traj_out.dir <- paste0(to.data, trajectory.data.folder)
	
	dir.create(traj_out.dir, showWarnings=F)
	all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
 
 	for (j in 1:length(all.files)) {
   		PA_data <- read.table(paste0(PA_output_dir,"/",all.files[j]),sep="\t",header=T)
 
   		## only attempt particle linking if particles were detected in the video
   		## note: not sure what would happen if only one particle was found in one frame
   		if(length(PA_data[,1])>0) {
 
   			dir <- gsub(".cxd","",sub(".ijout.txt","",all.files[j]))
   			dir.create(dir)
   
   			for (i in 1:max(PA_data$Slice)){
         		frame <- subset(PA_data, Slice == i)[,c(6,7)]
         		frame$Z <- rep(0.00, length(frame[,1]))
         		sink(paste0(dir,"/frame_",i-1,".txt"))
         		cat(paste0("frame ",i-1))
         		cat("\n")
         		sink()
         		write.table(frame,file=paste0(dir,"/frame_",i-1,".txt"),append=T,col.names=F,row.names=F)
    		}
       
  			## run ParticleLinker
  			if(.Platform$OS.type == "unix") {
    			cmd <- paste0('java -Xmx512m -Dparticle.linkrange=5 -Dparticle.displacement=20 -jar ',to.particlelinker.owen,'/ParticleLinker.jar ',dir,' "',traj_out.dir,'/ParticleLinker_',all.files[j],'.txt"')   
    			system(cmd)
  			}
       
  			if(.Platform$OS.type == "windows") {
    			cmd <- paste0('C:/Progra~2/java/jre7/bin/javaw.exe -Xmx512m -Dparticle.linkrange=5 -Dparticle.displacement=20 -jar ',to.particlelinker.frank,'/ParticleLinker.jar ',dir,' "',traj_out.dir,'/ParticleLinker_',all.files[j],'.txt"')
    			system(cmd)
  			}
       
  			# delete working dir
  			unlink(dir, recursive = TRUE) 

		}

   		if(length(PA_data[,1])==0) {
 			print(paste("***** No particles were detected in video", all.files[j], " -- check the raw video and also threshold values"))
       	}
	}  
}



   
## Merge the trajectory data from the ParticleLinker into one data file
## which corresponds to what we got before from the ParticleTracker.
## Provide directory where ParticleLinker output is stored and 
## where merged trajectory.data should be saved
Organise_link_data <- function(to.data, trajectory.data.folder) {
	
	here <- paste0(to.data, trajectory.data.folder)
	
	dir.create(here, showWarnings=F)
	df <- data.frame(frame=numeric(), X=numeric(), Y=numeric(), trajectory=numeric(), file=character())
	files <- dir(here)
	for (i in 1:length(files)){
    	file <- gsub(".ijout.txt.txt", "", gsub("ParticleLinker_", "", files[i]))
    	if (file.info(paste(here, files[i], sep="/"))$size > 0) {
    		data <- read.table(paste(here, files[i], sep="/"), header=T, sep=",")
    		data$file <- rep(file, length(data$x))
    		data$y <- -data$y
    		if (i == 1) data.full <- rbind(data, df)
    		if (i > 1) data.full <- rbind(data.full, data)
		}
	}
	data.full <- data.full[, c(2, 4, 3, 1, 5)]
	colnames(data.full) <- c("frame", "X", "Y", "trajectory", "file")
	write.table(data.full,file=paste(here, "trajectory.data.txt", sep="/"), sep="\t")
}   
   






## function to plot trajectories for overlay (must be merged with original video by ImageJ macro)
## creates a folder containing one jpeg plot containing all the positions till the respective frame
## for the moment colour not assigned by species identity but that's easy to add 
## provide path of " 2 - trajectory data", and the width and height of the original video 
## (I use cropped videos to increase speed while troubleshooting)
Create_overlay_videos <- function(to.data, trajectory.data.folder, raw.video.folder,
								  temp.overlay.folder, overlay.folder, 
								  width, height, difference.lag,
								  type='traj', original.vid.contrast.enhancement=1.0) {
	
	video.dir <- paste(to.data, raw.video.folder, sep="")	
								 
	trackdata.dir <- paste(to.data, trajectory.data.folder, sep="")								 
								  	 
    trajectory.data <- as.data.frame(read.table(paste(trackdata.dir,"trajectory.data.txt", sep = ""),													header = TRUE, sep = "\t"))
    file_names <- unique(trajectory.data$file)  

    ## change path for output
    dir.create(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir), showWarnings=F)
    for (i in 1:length(file_names)){
        ##split filename into name and ending for creating directories according to video name
        ##filename_split <- strsplit(paste(file_names[i]),"\\.")
        ##filename <- filename_split[[1]]
        dir.create(paste0(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir),
        		file_names[i]), showWarnings=F) #sub("Traj_","",filename[1]),sep="/"))
        trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
        j<- 0
        if (type == 'traj'){
            while(j < max(trajectory.data$frame)+1){
				jpeg(paste(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir),
					file_names[i], "/", "frame_", j, ".jpg", sep=""),
					width = as.numeric(width), height = as.numeric(height), quality = 100)
                par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
                print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame <= j,
                				select=c("X","Y","trajectory"))
                				
                ## plot the particle(s) so long as there are some	
                if(length(print[,1])!=0) {
	                plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)),
    	            	ylim=c(0,as.numeric(height)), col="#FFFF00", pch=15, cex=1, asp=1)
        	    }
        	    
        	     ## otherwise just plot the empty frame
                if(length(print[,1])==0) {
                	plot(NA, NA, xlim=c(0,as.numeric(width)),
                		ylim=c(0,as.numeric(height)), col="blue", pch=1, cex=6, asp=1)
                }
				dev.off()
                j <- j+1
			}
		}
		
        if (type == 'label'){
            while(j < max(trajectory.data$frame)+1){
                jpeg(paste(sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir),
                	file_names[i], "/", "frame_", j, ".jpg", sep=""),
                	width = as.numeric(width), height = as.numeric(height), quality = 100)
                par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
                print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame == j,
                	select=c("X","Y","trajectory"))
                
                ## plot the particle(s) so long as there are some	
                if(length(print[,1])!=0) {
                	plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)),
                		ylim=c(0,as.numeric(height)), col="blue", pch=1, cex=6, asp=1)
                	text(print$Y, print$X+as.numeric(height)-20,print$traject,cex=2,col="red")
                }
                
                ## otherwise just plot the empty frame
                if(length(print[,1])==0) {
                	plot(NA, NA, xlim=c(0,as.numeric(width)),
                		ylim=c(0,as.numeric(height)), col="blue", pch=1, cex=6, asp=1)
                }
                dev.off()       	
                j <- j+1
			}
		}
    }
    
    ## copy master copy of ImageJ macro there for treatment
    if(.Platform$OS.type == "windows")
        text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_overlay.ijm",warn = FALSE)
    if(.Platform$OS.type == "unix")
		##text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_overlay.ijm")
        text <- readLines(paste(to.code, "ImageJ macros/Video_overlay.ijm", sep=""))
    
    
    ## use regular expression to insert input and output directory
    ## and contrast enhancement of original video
    text[grep("avi_input = ", text)] <- paste("avi_input = ", "'",
    	sub(trajectory.data.folder, raw.video.folder, trackdata.dir), "';", sep = "")
    text[grep("overlay_input = ", text)] <- paste("overlay_input = ", "'",
    	sub(trajectory.data.folder, temp.overlay.folder, trackdata.dir), "';", sep = "")
    text[grep("overlay_output = ", text)] <- paste("overlay_output = ","'",
    	sub(trajectory.data.folder, overlay.folder, trackdata.dir), "';", sep = "")
    text[grep("lag =", text)] <- paste("lag = ", difference.lag, ";", sep = "")
    text[grep("Enhance Contrast", text)] <- paste("run(\"Enhance Contrast...\", \"saturated=",
    	original.vid.contrast.enhancement, " process_all\");", sep = "")
    
    
    ## re-create ImageJ macro for batch processing of video files with ParticleTracker
    if(.Platform$OS.type == "windows")
        writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm", sep=""), sep="\n")
    if(.Platform$OS.type == "unix") {
        ijmacs.folder1 <- sub(raw.video.folder, ijmacs.folder, video.dir)
        writeLines(text,con=paste(ijmacs.folder1, "/Video_overlay_tmp.ijm", sep=""))
        ##writeLines(text,con=paste(sub("1 - raw","ijmacs",video.dir), "/Video_overlay_tmp.ijm",sep=""))
    }
    
    ## create directory to store overlays
    dir.create(sub(trajectory.data.folder, overlay.folder, trackdata.dir), showWarnings=F)
    
    ## call IJ macro to merge original video with the trajectory data
    if(.Platform$OS.type == "unix")
        cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub(raw.video.folder, ijmacs.folder, video.dir), "Video_overlay_tmp.ijm", sep=""))
    
    if(.Platform$OS.type == "windows")
		cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_overlay_tmp.ijm')
	
    ## run ImageJ macro
    system(cmd)
    
    ## delete temporary file after execution
    if(.Platform$OS.type == "windows")
        file.remove("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm")

}




### does what the function name suggests...
Merge_particle_link_experiment_data <- function(to.data,
												particle.data.folder,
												trajectory.data.folder,
												video.description.folder,
												video.description.file,
												merged.data.folder) {

	# read the file that gives the important information about each video
	file.sample.info <- read.table(paste(to.data, video.description.folder, video.description.file, sep=""),
								   sep= "\t", header = TRUE)
	## put junk in the time variable
	##file.sample.info$time <- -999

	## load the two datasets
	morphology.data <- read.table(paste0(to.data, particle.data.folder, "particle.data.txt"), row.names=1)
	trajectory.data <- read.table(paste0(to.data, trajectory.data.folder, "trajectory.data.txt"), header=TRUE, sep="\t")

	## Prep for merging the trajectory data
	## Note that the next lines also swap the x and y 
	trajectory.data$Y1 <- -trajectory.data$X
	trajectory.data$X1 <- trajectory.data$Y
	trajectory.data$X  <- trajectory.data$X1
	trajectory.data$Y  <- trajectory.data$Y1
	## trajectory frame starts with 0, therefore add one to adjust to morphology data
	trajectory.data$frame <- trajectory.data$frame + 1
	#trajectory.data <- trajectory.data[,-c(6,7)]
	##unique(trajectory.data$frame)
	##with(trajectory.data, aggregate(frame, list(frame), length))


	## Prep for merging the morphology data
	morphology.data$frame <- morphology.data$Slice   ## I have no idea why add 8 here!!!
	morphology.data$Slice <- NULL
	morphology.data$X <- morphology.data$X
	morphology.data$Y <- morphology.data$Y
	morphology.data$file <- sub(".cxd", "", morphology.data$file)
	##unique(morphology.data$frame)
	##with(morphology.data, aggregate(Area, list(frame=frame), function(x) length(x[!is.na(x)]))) 

	
	## subsample to do visual control
	# subset_m <- subset(morphology.data, file == "Data00160")# & trajectory==2)
	# subset_t <- subset(trajectory.data, file == "Data00160")# & trajectory==2)
	# ## check why X and Y differ between trajectories and morphology
	# xlims <- c(840,950)
	# ylims <- c(50, 200)
	# #xlims <- c(0,2100)
	# #ylims <- c(0, 2100)	
	# plot(subset_m$X, subset_m$Y, pch=subset_m$frame, cex=0.5, asp=1, xlim=xlims, ylim=ylims)
	# par(new=T)
	# plot(subset_t$X+10, subset_t$Y, pch=subset_t$frame, cex=0.5, asp=1, type="p", xlim=xlims, ylim=ylims)
	# par(new=F)


	# dim(morphology.data)
	# dim(trajectory.data)

	## merge the two datasets
	merged1 <- merge(morphology.data, trajectory.data,
					by.x=c("X", "Y", "frame", "file"),
					by.y=c("X", "Y", "frame", "file"),
					all=T)
	##dim(merged1)		
	##dim(na.omit(merged1))						
					
	## make the merge of the file names case insensitive
	merged1$file <- tolower(merged1$file)
	file.sample.info$video <- tolower(file.sample.info$video)
	
							
	merged2 <- merge(merged1, file.sample.info, by.x="file", by.y="video", all=F)	
	##dim(merged2)			
	##dim(na.omit(merged2))

	merged2 <- merged2[,is.na(match(names(merged2), c("X1","Y1")))]

	dir.create(paste0(to.data, merged.data.folder), showWarnings=F)

	write.csv(merged2, file = paste(paste0(to.data, merged.data.folder),"MasterData.csv", sep = "/"), row.names=F)

}






create_prediction_plots <- function(path,width,height,difference.lag){ 
  ## function that produces labelled overlays based on the classification and the original tracks:
  ## different species are coloured
  ## numbered objects without halo where filtered out before classification and are therefore artefacts,
  ## though sometimes valid trajects are filtered out
  
  video.dir <- paste(to.data, raw.video.folder, sep="")
  
  trajectory.data <- predict_visual
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
    text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_overlay.ijm",warn = FALSE)
  if(.Platform$OS.type == "unix")
   # text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_overlay.ijm")
    text <- readLines("/Users/Frank/franco/automation/ImageJ macros/Video_overlay.ijm")
    text <- readLines(paste(to.code, "ImageJ macros/Prediction_overlay.ijm", sep=""))
  
  
  ## use regular expression to insert input and output directory
  text[grep("avi_input = ", text)] <- paste("avi_input = ","'", paste0(path,raw.video.folder),"';", sep = "")
  text[grep("overlay_input = ", text)] <- paste("overlay_input = ","'", paste0(path,prediction.folder),"';", sep = "")
  text[grep("overlay_output = ", text)] <- paste("overlay_output = ","'", paste0(path,prediction.folder2),"';", sep = "")
  text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
  
  
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
    cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub(raw.video.folder,"ijmacs",video.dir), "/Prediction_overlay_tmp.ijm",sep=""))
  }
  if(.Platform$OS.type == "windows"){
    cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Prediction_overlay_tmp.ijm')}
  
  ## run ImageJ macro
  system(cmd)
  
  ## delete temporary file after execution
  if(.Platform$OS.type == "windows"){
    file.remove("C:/Program Files/Fiji.app/macros/Prediction_overlay_tmp.ijm")}
}
