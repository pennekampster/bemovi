## Check to see if there are any unsupported file types, or file names with two periods
Check.video.files <- function(video.dir)
{
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
Check_threshold <- function(video.dir,difference.lag, thresholds=c(10,255)) {
    
    ## generate the folders if not already existing
    ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
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
        writeLines(text,con=paste(ijmacs.folder, "/Check_threshold_tmp.ijm",sep=""))

    ## run to process video files by calling ImageJ / needs fixing for Mac
    ## if(.Platform$OS.type == "unix")
    ##     cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(ijmacs.folder, "Check_threshold_tmp.ijm",sep=""))
    ## if(.Platform$OS.type == "windows")
    ##     cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Check_threshold_tmp.ijm')
    ##system(cmd)

}




## Function to get morphological measurements using the particle analyser in imagej
video_to_morphology <- function(video.dir, difference.lag, thresholds=c(0,1000)) {

    ## copy master copy of ImageJ macro there for treatment
    text <- readLines(paste(to.code, "ImageJ macros/Video_to_morphology.ijm", sep=""))
    
    ## use regular expression to insert input & output directory as well as difference lag
    text[grep("avi_input = ", text)] <- paste("avi_input = ","'", video.dir,"';", sep = "")
    text[grep("avi_output = ", text)] <- paste("avi_output = ","'",sub("1 - raw/","5 - Particle Analyzer data/",video.dir),"';", sep = "")
    text[grep("lag = ", text)] <- paste("lag = ",difference.lag,";", sep = "")
    text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")
    
    
    
    ## re-create ImageJ macro for batch processing of video files with Particle Analyzer
    if(.Platform$OS.type == "windows") 
        writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm",sep=""),sep="\n")
    if(.Platform$OS.type == "unix") {
        dir.create(sub(raw.video.folder,"ijmacs",video.dir), showWarnings=F)	
        writeLines(text,con=paste(sub(raw.video.folder,"ijmacs",video.dir), "/Video_to_morphology_tmp.ijm",sep=""))
    }
    
    ## create directory to store Particle Analyzer data
    dir.create(sub(raw.video.folder,"5 - Particle Analyzer data",video.dir),showWarnings = FALSE)
    
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



# function to plot trajectories for overlay (must be merged with original video by ImageJ macro)
# creates a folder containing one jpeg plot containing all the positions till the respective frame
# for the moment colour not assigned by species identity but that's easy to add 
# provide path of " 2 - trajectory data", and the width and height of the original video 
# (I use cropped videos to increase speed while troubleshooting)
create_overlay_plots <- function(path,width,height,difference.lag,type='traj'){ 
trajectory.data <- as.data.frame(read.table(paste(path,"trajectory.data.txt", sep = ""), header = TRUE, sep = "\t"))
file_names <- unique(trajectory.data$file)  

# change path for output
dir.create(sub(trajectory.data.folder,overlay.folder,path))
for (i in 1:length(file_names)){
   dir.create(paste0(sub(trajectory.data.folder,overlay.folder,path), file_names[i]))
   trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
   j<- 0
   if (type == 'traj'){
   while(j < max(trajectory.data$frame)+1){
      jpeg(paste(sub(trajectory.data.folder,overlay.folder,path),file_names[i],"/","frame_",j,".jpg",sep=""), width = as.numeric(width), height = as.numeric(height), quality = 100)
      par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
      print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame <= j, select=c("X","Y","trajectory"))
      plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col="#FFFF00", pch=15, cex=1, asp=1)
      dev.off()
      j <- j+1}}
   if (type == 'label'){
   while(j < max(trajectory.data$frame)+1){
     jpeg(paste(sub(trajectory.data.folder,overlay.folder,path),file_names[i],"/","frame_",j,".jpg",sep=""), width = as.numeric(width), height = as.numeric(height), quality = 100)
     par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
     print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame == j, select=c("X","Y","trajectory"))
     plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col="blue", pch=1, cex=6, asp=1)
     if (length(print$trajectory)>0) text(print$Y, print$X+as.numeric(height)-20,as.numeric(print$trajectory),cex=2,col="red")
     dev.off()
     j <- j+1}}
   
}

# copy master copy of ImageJ macro there for treatment
if(.Platform$OS.type == "windows")
  text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_overlay.ijm",warn = FALSE)
if(.Platform$OS.type == "unix")
  text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_overlay.ijm")

text <- readLines(paste(to.code, "ImageJ macros/Video_overlay.ijm", sep=""))


# use regular expression to insert input and output directory
text[grep("avi_input = ", text)] <- paste("avi_input = ","'", sub(trajectory.data.folder,raw.video.folder,path),"';", sep = "")
text[grep("overlay_input = ", text)] <- paste("overlay_input = ","'", sub(trajectory.data.folder,overlay.folder,path),"';", sep = "")
text[grep("overlay_output = ", text)] <- paste("overlay_output = ","'", sub(trajectory.data.folder,overlay.folder2,path),"';", sep = "")
text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")


# re-create ImageJ macro for batch processing of video files with ParticleTracker
if(.Platform$OS.type == "windows")
  writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") {
  ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
  writeLines(text,con=paste(ijmacs.folder, "/Video_overlay_tmp.ijm",sep=""))
  ##writeLines(text,con=paste(sub("1 - raw","ijmacs",video.dir), "/Video_overlay_tmp.ijm",sep=""))
}

# create directory to store overlays
dir.create(sub(trajectory.data.folder,overlay.folder2,path))

#call IJ macro to merge original video with the trajectory data
if(.Platform$OS.type == "unix"){
  cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub(raw.video.folder,"ijmacs",video.dir), "/Video_overlay_tmp.ijm",sep=""))
}
if(.Platform$OS.type == "windows"){
  cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_overlay_tmp.ijm')}

# run ImageJ macro
system(cmd)

# delete temporary file after execution
if(.Platform$OS.type == "windows")
  file.remove("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm")
}



## This function puts all the morphology files (one for each video) into one dataset, and saves it
LoadIJ_morph_outs <- function(IJ_output.dir) {
    
    ## the macro file names
    all.files <- dir(path=IJ_output.dir)
    ijout.files <- all.files[grep("ijout", all.files)]
    
    dd <- read.delim(paste(IJ_output.dir, ijout.files[1], sep="//"))
    dd$file <- rep(gsub(".ijout.txt","",ijout.files[1]), length(dd[,1]))
    
    ## change column names because R is replacing missing header with X causing confusion with real X and Y positions
    colnames(dd) <- c("obs","Area","Mean","Min","Max","X","Y","Perimeter","Major","Minor","Angle","Circ.","Slice","AR","Round","Solidity","file")
    
    if(length(ijout.files)>2) {
	for(i in 2:length(ijout.files)) {
            
            dd.t <- read.delim(paste(IJ_output.dir, ijout.files[i], sep="//"))
            dd.t$file <- rep(gsub(".ijout.txt","",ijout.files[i]), length(dd.t[,1]))
                                        # change column names because R is replacing missing header with X causing confusion with real X and Y positions
            colnames(dd.t) <- c("obs","Area","Mean","Min","Max","X","Y","Perimeter","Major","Minor","Angle","Circ.","Slice","AR","Round","Solidity","file")
            dd <- rbind(dd, dd.t)
	}
    }
  
assign("morphology.data",dd,envir = .GlobalEnv)
write.table(morphology.data, file = paste(IJ_output.dir,"morphology.data.txt", sep = "/"), sep = "\t")
}

   
   
# Function to convert XY coordinates of the ParticleAnalyzer into a structure (e.g. folder with coordinates per frame) 
# that can be read by the standalone ParticleLinker
convert_PA_to_traject <- function(PA_output_dir,traj_out.dir){

 dir.create(traj_out.dir)
 all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
 for (j in 1:length(all.files)){
   PA_data <- read.table(paste0(PA_output_dir,"/",all.files[j]),sep="\t",header=T)
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
       
# run ParticleLinker
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
       
#increase file counter
j+1
}
}

   
#merge the trajectory data from the ParticleLinker into one data file which corresponds to what we got before from the ParticleTracker
#provide directory where ParticleLinker output is stored and where merged trajectory.data should be saved
merge_PA_results <- function(PA_dir,traj_out.dir){
dir.create(traj_out.dir)
df <- data.frame(frame=numeric(),X=numeric(),Y=numeric(),trajectory=numeric(),file=character())
files <- dir(paste0(to.data,particle.linker.out))
for (i in 1:length(files)){
    file <- gsub(".ijout.txt.txt","",gsub("ParticleLinker_","",files[i]))
    if (file.info(paste(PA_dir,files[i],sep="/"))$size > 0){
    data <- read.table(paste(PA_dir,files[i],sep="/"),header=T,sep=",")
    data$file <- rep(file,length(data$x))
    data$y <- -data$y
    if (i == 1) data.full <- rbind(data,df)
    if (i > 1) data.full <- rbind(data.full,data)
}}
data.full <- data.full[, c(2,4,3,1,5)]
colnames(data.full) <- c("frame","X","Y","trajectory","file")
write.table(data.full,file=paste(traj_out.dir,"trajectory.data.txt",sep="/"),sep="\t")
}   
   

# code to batch process videos by ImageJ and ParticleTracker plugin
# provide directory where raw videos are stored
video_to_trajectory <- function(video.dir, difference.lag, thresholds=c(10,255), stackmax.background="false"){

    ## generate the folders...
    ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
    dir.create(ijmacs.folder, showWarnings = FALSE)	
    tmp.raw.folder <- sub(substr(raw.video.folder, 1, nchar(raw.video.folder)-1),"1 - raw tmp",video.dir)
    dir.create(tmp.raw.folder, showWarnings = FALSE)
    trajdata.folder <- sub(raw.video.folder,"2 - trajectory data/",video.dir)
    dir.create(trajdata.folder, showWarnings = FALSE)
    
    ## copy master copy of ImageJ macro there for treatment
    text <- readLines(paste(to.code, "ImageJ macros/Video_to_trajectory.ijm", sep=""))
    
    ## use regular expression to insert input and output directory
    text[grep("dir_input =", text)] <- paste("dir_input = ","'", video.dir,"';", sep = "")
    text[grep("dir_output =", text)] <- paste("dir_output = ","'",sub(raw.video.folder, paste(substr(raw.video.folder, 1, nchar(raw.video.folder)-1),  "tmp/"),video.dir),"';", sep = "")
    text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
    text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")
    if(stack.max.background=="light")
	text[grep("light =", text)] = paste("  light = ", stackmax.background, ";", sep="")
      
    ## re-create ImageJ macro for batch processing of video files with ParticleTracker
    ## perhaps put this in a subdirectory of the data folder?
    ## This is implemented in OSX but not windows, which is as you wrote it
    if(.Platform$OS.type == "windows") 
	writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm",sep=""),sep="\n")
    if(.Platform$OS.type == "unix") 
        writeLines(text,con=paste(ijmacs.folder, "/Video_to_trajectory_tmp.ijm",sep=""))
    
    ## run to process video files by calling ImageJ / needs fixing for Mac
    if(.Platform$OS.type == "unix")
        cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(ijmacs.folder, "Video_to_trajectory_tmp.ijm",sep=""))
    if(.Platform$OS.type == "windows")
        cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_trajectory_tmp.ijm')
    system(cmd)
    
    ## delete temporary file after execution
    if(.Platform$OS.type == "windows")
        file.remove("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm")
    
    ## copy files produced by ParticleTracker to "2 - trajectory data" directory
    all.files <- dir(tmp.raw.folder)
    ijout.files <- all.files[grep("Traj_", all.files)]
    file.copy(paste(sub("1 - raw/","1 - raw tmp/",video.dir),ijout.files, sep = ""),sub("1 - raw","2 - trajectory data",video.dir))
    
}




## This function gets the output files produced by the Imagej ParticleTracker
## specify the path where the files which contain the trajectory data are stored
LoadIJ_Traj_Outs <- function(trajdata.dir)
{
  
    ## the macro file names
    all.files <- dir(path=trajdata.dir)
    ijout.files <- all.files[grep("Traj_", all.files)]
    
    for (i in 1:length(ijout.files)){
        text <- readLines(paste(trajdata.dir, ijout.files[i], sep="/"))
        
        string <- "(^[[:digit:]]+)"
        ## find lines which match the string
        txtsubset <-  grep(string, text) 
        ##subset the original file by only retaining lines that matched the string
        out <- text[txtsubset]
        
        ## find lines that mark start of trajectories
        pattern <- c("%% Trajectory [0_9]*")
        list <- grep(pattern,text)
        
        ## add the end line of the last block and increase the line number by one to match the pattern of the other records
        list_tmp <- append(list,length(text))
        list_tmp[length(list)+1]<-as.integer(list_tmp[length(list)+1]+1)
        
        ## produce vector with unique trajectory ids
        difference <- numeric(0)
        for (k in 1:length(list))
            {diff <- rep(k,(list_tmp[k+1]-3-list_tmp[k]+1))
             difference <- append(difference,diff)}
        
        myfun <- function(x) deparse(substitute(x)) 
        assign(paste(myfun(ijout.files[i])),ijout.files[i],envir = .GlobalEnv)
        
        
        if (i == 1){
            dd <- as.data.frame(read.table(textConnection(out)))
            dd$file <- rep(gsub(".avi.txt","",gsub("Traj_","",ijout.files[i])), length(dd$V1))
            ## merge unique trajectory_ID with the original data
            dd <- cbind(dd,difference)
        }
        
        if (i > 1){
            dd.t <- as.data.frame(read.table(textConnection(out)))
            dd.t$file <- rep(gsub(".avi.txt","",gsub("Traj_","",ijout.files[i])), length(dd.t$V1))
            ## merge unique trajectory_ID with the original data
            dd.t <- cbind(dd.t,difference)
            
            dd <- rbind(dd, dd.t)}
    }
    
    ## invert Y-axis to account for origin in upper-left corner
    dd$V2 <- -dd$V2
    
    ## rename drop unused information and rename variables
    dd <- dd[c(-4,-5,-6,-7,-8,-9,-10)]
    names(dd) <- c("frame","X","Y","file","trajectory")
    
    assign("trajectory.data",dd,envir = .GlobalEnv)
    write.table(trajectory.data, file = paste(trajdata.dir,"trajectory.data.txt", sep = "/"), sep = "\t")
}


## function to plot trajectories for overlay (must be merged with original video by ImageJ macro)
## creates a folder containing one jpeg plot containing all the positions till the respective frame
## for the moment colour not assigned by species identity but that's easy to add 
## provide path of " 2 - trajectory data", and the width and height of the original video 
## (I use cropped videos to increase speed while troubleshooting)
create_overlay_plots <- function(trackdata.dir, width, height, difference.lag, type='traj',original.vid.contrast.enhancement=1.0){ 
    trajectory.data <- as.data.frame(read.table(paste(trackdata.dir,"trajectory.data.txt", sep = ""), header = TRUE, sep = "\t"))
    file_names <- unique(trajectory.data$file)  

    ## change path for output
    dir.create(sub(trajectory.data.folder, overlay.folder, trackdata.dir), showWarnings=F)
    for (i in 1:length(file_names)){
        ##split filename into name and ending for creating directories according to video name
        ##filename_split <- strsplit(paste(file_names[i]),"\\.")
        ##filename <- filename_split[[1]]
        dir.create(paste0(sub(trajectory.data.folder,overlay.folder,trackdata.dir), file_names[i]), showWarnings=F) #sub("Traj_","",filename[1]),sep="/"))
        trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
        j<- 0
        if (type == 'traj'){
            while(j < max(trajectory.data$frame)+1){
                jpeg(paste(sub(trajectory.data.folder,overlay.folder,trackdata.dir),file_names[i],"/","frame_",j,".jpg",sep=""), width = as.numeric(width), height = as.numeric(height), quality = 100)
                par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
                print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame <= j, select=c("X","Y","trajectory"))
                plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col="#FFFF00", pch=15, cex=1, asp=1)
                dev.off()
                j <- j+1}}
        if (type == 'label'){
            while(j < max(trajectory.data$frame)+1){
                jpeg(paste(sub(trajectory.data.folder,overlay.folder,trackdata.dir),file_names[i],"/","frame_",j,".jpg",sep=""), width = as.numeric(width), height = as.numeric(height), quality = 100)
                par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
                print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame == j, select=c("X","Y","trajectory"))
                plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col="blue", pch=1, cex=6, asp=1)
                text(print$Y, print$X+as.numeric(height)-20,print$traject,cex=2,col="red")
                dev.off()
                j <- j+1}}
        
    }
    
    ## copy master copy of ImageJ macro there for treatment
    if(.Platform$OS.type == "windows")
        text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_overlay.ijm",warn = FALSE)
    if(.Platform$OS.type == "unix")
		##text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_overlay.ijm")
        text <- readLines(paste(to.code, "ImageJ macros/Video_overlay.ijm", sep=""))
    
    
    ## use regular expression to insert input and output directory
    text[grep("avi_input = ", text)] <- paste("avi_input = ","'", sub(trajectory.data.folder,raw.video.folder,trackdata.dir),"';", sep = "")
    text[grep("overlay_input = ", text)] <- paste("overlay_input = ","'", sub(trajectory.data.folder,overlay.folder,trackdata.dir),"';", sep = "")
    text[grep("overlay_output = ", text)] <- paste("overlay_output = ","'", sub(trajectory.data.folder,overlay.folder2,trackdata.dir),"';", sep = "")
    text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
    text[grep("Enhance Contrast", text)] <- paste("run(\"Enhance Contrast...\", \"saturated=", original.vid.contrast.enhancement, " process_all\");", sep = "")
    
    
    ## re-create ImageJ macro for batch processing of video files with ParticleTracker
    if(.Platform$OS.type == "windows")
        writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm",sep=""),sep="\n")
    if(.Platform$OS.type == "unix") {
        ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
        writeLines(text,con=paste(ijmacs.folder, "/Video_overlay_tmp.ijm",sep=""))
        ##writeLines(text,con=paste(sub("1 - raw","ijmacs",video.dir), "/Video_overlay_tmp.ijm",sep=""))
    }
    
    ## create directory to store overlays
    dir.create(sub(trajectory.data.folder,overlay.folder2,trackdata.dir), showWarnings=F)
    
    ## call IJ macro to merge original video with the trajectory data
    if(.Platform$OS.type == "unix"){
        cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub(raw.video.folder,"ijmacs",video.dir), "/Video_overlay_tmp.ijm",sep=""))
    }
    if(.Platform$OS.type == "windows"){
		cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_overlay_tmp.ijm')}
    
    ## run ImageJ macro
    system(cmd)
    
    ## delete temporary file after execution
    if(.Platform$OS.type == "windows")
        file.remove("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm")
}




### does what the function name suggests...
merge_morphology_trajectory_expt_data <- function(to.data, particle.analyzer.folder, trajectory.data.folder, merge.folder,
											sample.dir, sample.description.file, difference.lag, rounder) {

	# read the file that gives the important information about each video
	file.sample.info <- read.table(paste(sample.dir, sample.description.file, sep=""), sep= "\t", header = TRUE)
	## put junk in the time variable
	file.sample.info$time <- -999

	## load the two datasets
	morphology.data <- read.table(paste0(to.data, particle.analyzer.folder, "morphology.data.txt"), row.names=1)
	trajectory.data <- read.table(paste0(to.data, trajectory.data.folder, "trajectory.data.txt"), header=TRUE, sep="\t")

	## Prep for merging the trajectory data
	## Note that the next lines also swap the x and y 
	trajectory.data$Y1 <- round_any(-trajectory.data$X, rounder)
	trajectory.data$X1 <- round_any(trajectory.data$Y, rounder)
	trajectory.data$X  <- trajectory.data$X1
	trajectory.data$Y  <- trajectory.data$Y1
	## trajectory frame starts with 0, therefore add one to adjust to morphology data
	trajectory.data$frame <- trajectory.data$frame 
	#trajectory.data <- trajectory.data[,-c(6,7)]
	##unique(trajectory.data$frame)
	##with(trajectory.data, aggregate(frame, list(frame), length))


	## Prep for merging the morphology data
	morphology.data$frame <- morphology.data$Slice + 8   ## I have no idea why add 8 here!!!
	morphology.data$Slice <- NULL
	morphology.data$X <- round_any(morphology.data$X, rounder)
	morphology.data$Y <- round_any(morphology.data$Y, rounder)
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
							
	merged2 <- merge(merged1, file.sample.info, by.x="file", by.y="video", all=F)	
	##dim(merged2)			
	##dim(na.omit(merged2))

	dir.create(paste0(to.data, merge.folder), showWarnings=F)

	write.csv(merged2, file = paste(paste0(to.data,merge.folder),"MorphTrajExptData.csv", sep = "/"), row.names=F)


}






create_prediction_plots <- function(path,width,height,difference.lag){ 
  ## function that produces labelled overlays based on the classification and the original tracks:
  ## different species are coloured
  ## numbered objects without halo where filtered out before classification and are therefore artefacts,
  ## though sometimes valid trajects are filtered out
  
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
    text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_overlay.ijm")
  
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
