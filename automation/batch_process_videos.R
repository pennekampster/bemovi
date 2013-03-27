# code to batch process videos by ImageJ and ParticleTracker plugin
# provide directory where raw videos are stored
video_to_trajectory <- function(video.dir, difference.lag, thresholds=c(10,255), stackmax.background="false")
{

## generate the folders...
ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
dir.create(ijmacs.folder, showWarnings = FALSE)	
tmp.raw.folder <- sub(substr(raw.video.folder, 1, nchar(raw.video.folder)-1),"1 - raw tmp",video.dir)
dir.create(tmp.raw.folder, showWarnings = FALSE)
trajdata.folder <- sub(raw.video.folder,"2 - trajectory data/",video.dir)
dir.create(trajdata.folder, showWarnings = FALSE)


# copy master copy of ImageJ macro there for treatment
text <- readLines(paste(to.code, "ImageJ macros/Video_to_trajectory.ijm", sep=""))

# use regular expression to insert input and output directory
text[grep("dir_input =", text)] <- paste("dir_input = ","'", video.dir,"';", sep = "")
text[grep("dir_output =", text)] <- paste("dir_output = ","'",sub(raw.video.folder, paste(substr(raw.video.folder, 1, nchar(raw.video.folder)-1),  "tmp/"),video.dir),"';", sep = "")
text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")
if(stack.max.background=="light")
	text[grep("light =", text)] = paste("  light = ", stackmax.background, ";", sep="")



# re-create ImageJ macro for batch processing of video files with ParticleTracker
## perhaps put this in a subdirectory of the data folder?
## This is implemented in OSX but not windows, which is as you wrote it
if(.Platform$OS.type == "windows") 
	writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") 
    writeLines(text,con=paste(ijmacs.folder, "/Video_to_trajectory_tmp.ijm",sep=""))


# run to process video files by calling ImageJ / needs fixing for Mac
if(.Platform$OS.type == "unix")
    cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(ijmacs.folder, "Video_to_trajectory_tmp.ijm",sep=""))
if(.Platform$OS.type == "windows")
    cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_trajectory_tmp.ijm')
system(cmd)

# delete temporary file after execution
if(.Platform$OS.type == "windows")
    file.remove("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm")

#copy files produced by ParticleTracker to "2 - trajectory data" directory
all.files <- dir(tmp.raw.folder)
ijout.files <- all.files[grep("Traj_", all.files)]
file.copy(paste(sub("1 - raw/","1 - raw tmp/",video.dir),ijout.files, sep = ""),sub("1 - raw","2 - trajectory data",video.dir))
}




## This function gets the output files produced by the Imagej ParticleTracker
# specify the path where the files which contain the trajectory data are stored
LoadIJ_Traj_Outs <- function(trajdata.dir)
{
  
## the macro file names
all.files <- dir(path=trajdata.dir)
ijout.files <- all.files[grep("Traj_", all.files)]

for (i in 1:length(ijout.files)){
text <- readLines(paste(trajdata.dir, ijout.files[i], sep="/"))
  
string <- "(^[[:digit:]]+)"
#find lines which match the string
txtsubset <-  grep(string, text) 
#subset the original file by only retaining lines that matched the string
out <- text[txtsubset]

# find lines that mark start of trajectories
pattern <- c("%% Trajectory [0_9]*")
list <- grep(pattern,text)

# add the end line of the last block and increase the line number by one to match the pattern of the other records
list_tmp <- append(list,length(text))
list_tmp[length(list)+1]<-as.integer(list_tmp[length(list)+1]+1)

# produce vector with unique trajectory ids
difference <- numeric(0)
for (k in 1:length(list))
{diff <- rep(k,(list_tmp[k+1]-3-list_tmp[k]+1))
 difference <- append(difference,diff)}

myfun <- function(x) deparse(substitute(x)) 
assign(paste(myfun(ijout.files[i])),ijout.files[i],envir = .GlobalEnv)


if (i == 1){
dd <- as.data.frame(read.table(textConnection(out)))
dd$file <- rep(gsub(".avi.txt","",gsub("Traj_","",ijout.files[i])), length(dd$V1))
# merge unique trajectory_ID with the original data
dd <- cbind(dd,difference)
}

if (i > 1){
dd.t <- as.data.frame(read.table(textConnection(out)))
dd.t$file <- rep(gsub(".avi.txt","",gsub("Traj_","",ijout.files[i])), length(dd.t$V1))
# merge unique trajectory_ID with the original data
dd.t <- cbind(dd.t,difference)

dd <- rbind(dd, dd.t)}
}

# invert Y-axis to account for origin in upper-left corner
dd$V2 <- -dd$V2

#rename drop unused information and rename variables
dd <- dd[c(-4,-5,-6,-7,-8,-9,-10)]
names(dd) <- c("frame","X","Y","file","trajectory")

assign("trajectory.data",dd,envir = .GlobalEnv)
write.table(trajectory.data, file = paste(trajdata.dir,"trajectory.data.txt", sep = "/"), sep = "\t")
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
   #split filename into name and ending for creating directories according to video name
   #filename_split <- strsplit(paste(file_names[i]),"\\.")
   #filename <- filename_split[[1]]
   dir.create(paste0(sub(trajectory.data.folder,overlay.folder,path), file_names[i])) #sub("Traj_","",filename[1]),sep="/"))
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
     plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col="blue", pch=1, cex=3, asp=1)
     text(print$Y, print$X+2048-20,print$traject,cex=2,col="red")
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



# code to batch process videos by ImageJ and ParticleTracker plugin
# provide directory where raw videos are stored
Check_threshold <- function(video.dir,difference.lag, thresholds=c(10,255)) {

## generate the folders...
ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
dir.create(ijmacs.folder, showWarnings = FALSE)	
checkthresh.folder <- sub(raw.video.folder,raw.checkthreshold.folder,video.dir)
dir.create(checkthresh.folder, showWarnings = FALSE)


# copy master copy of ImageJ macro there for treatment
text <- readLines(paste(to.code, "ImageJ macros/Check_threshold.ijm", sep=""))

# use regular expression to insert input and output directory
text[grep("avi_input =", text)] <- paste("avi_input = ","'", video.dir,"';", sep = "")
text[grep("avi_output =", text)] <- paste("avi_output = ","'",checkthresh.folder,"';", sep = "")
text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")



# re-create ImageJ macro for batch processing of video files with ParticleTracker
## perhaps put this in a subdirectory of the data folder?
## This is implemented in OSX but not windows, which is as you wrote it
if(.Platform$OS.type == "windows") 
	writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Check_threshold_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") 
    writeLines(text,con=paste(ijmacs.folder, "/Check_threshold_tmp.ijm",sep=""))


# run to process video files by calling ImageJ / needs fixing for Mac
if(.Platform$OS.type == "unix")
    cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(ijmacs.folder, "Check_threshold_tmp.ijm",sep=""))
if(.Platform$OS.type == "windows")
    cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Check_threshold_tmp.ijm')
system(cmd)
}

