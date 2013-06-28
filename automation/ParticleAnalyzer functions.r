video_to_morphology <- function(video.dir, difference.lag, thresholds=c(0,1000)) {

# copy master copy of ImageJ macro there for treatment
text <- readLines(paste(to.code, "ImageJ macros/Video_to_morphology.ijm", sep=""))

# use regular expression to insert input & output directory as well as difference lag
text[grep("avi_input = ", text)] <- paste("avi_input = ","'", video.dir,"';", sep = "")
text[grep("avi_output = ", text)] <- paste("avi_output = ","'",sub("1 - raw/","5 - Particle Analyzer data/",video.dir),"';", sep = "")
text[grep("lag = ", text)] <- paste("lag = ",difference.lag,";", sep = "")
text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")



# re-create ImageJ macro for batch processing of video files with Particle Analyzer
if(.Platform$OS.type == "windows") 
  writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") {
  dir.create(sub(raw.video.folder,"ijmacs",video.dir))	
  writeLines(text,con=paste(sub(raw.video.folder,"ijmacs",video.dir), "/Video_to_morphology_tmp.ijm",sep=""))
}

#create directory to store Particle Analyzer data
dir.create(sub(raw.video.folder,"5 - Particle Analyzer data",video.dir),showWarnings = FALSE)

# run to process video files by calling ImageJ
if(.Platform$OS.type == "unix")
  cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub("1 - raw","ijmacs",video.dir), "Video_to_morphology_tmp.ijm",sep=""))
if(.Platform$OS.type == "windows")
  cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_morphology_tmp.ijm')
system(cmd)

# delete temporary file after execution
if(.Platform$OS.type == "windows")
  file.remove("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm")

}


## This function gets the output files produced by the imagej macros previously created (by function MakeIJMacros)
## and run by function RunIJMacros
LoadIJ_morph_outs <- function(IJ_output.dir) {

## the macro file names
all.files <- dir(path=IJ_output.dir)
ijout.files <- all.files[grep("ijout", all.files)]
	
dd <- read.delim(paste(IJ_output.dir, ijout.files[1], sep="//"))
dd$file <- rep(gsub(".ijout.txt","",ijout.files[1]), length(dd[,1]))

# change column names because R is replacing missing header with X causing confusion with real X and Y positions
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
  all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
  for (j in 1:length(all.files)){
    PA_data <- read.table(paste0(PA_output_dir,"/",all.files[j]),sep="\t",header=T)
    dir <- gsub(".ijout.txt","",all.files[j])
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
      cmd <- "java -Xmx512m -Dparticle.linkrange=5 -Dparticle.displacement=20 -jar ~/Desktop/shawntest/ParticleLinker.jar ~/Desktop/shawntest/input_data ~/Desktop/shawntest/output.txt"
    }
    
    if(.Platform$OS.type == "windows") {
      cmd <- paste0('C:/Progra~2/java/jre7/bin/javaw.exe -Xmx512m -Dparticle.linkrange=5 -Dparticle.displacement=20 -jar C:/Users/Frank/Dropbox/shawntest/ParticleLinker.jar ',dir,' "',traj_out.dir,'/ParticleLinker_',all.files[j],'.txt"')
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
df <- data.frame(frame=numeric(),X=numeric(),Y=numeric(),trajectory=numeric(),file=character())
files <- dir(paste0(to.data.frank,particle.linker.out))
for (i in 1:length(files)){

file <- gsub(".ijout.txt.txt","",gsub("ParticleLinker_","",files[i]))
data <- read.table(paste(PA_dir,files[i],sep="/"),header=T,sep=",")
data$file <- rep(file,length(data$x))
data$y <- -data$y
if (i == 1) data.full <- rbind(data,df)
if (i > 1) data.full <- rbind(data.full,data)
}
data.full <- data.full[, c(2,4,3,1,5)]
colnames(data.full) <- c("frame","X","Y","trajectory","file")
write.table(data.full,file=paste(traj_out.dir,"trajectory.data.txt",sep="/"),sep="\t")
}

