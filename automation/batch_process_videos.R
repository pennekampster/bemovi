# code to batch process videos by ImageJ and ParticleTracker plugin
# provide directory where raw videos are stored
video_to_trajectory <- function(video.dir) {

# copy master copy of ImageJ macro there for treatment
if(.Platform$OS.type == "unix")
	text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_to_trajectory.ijm")
if(.Platform$OS.type == "windows")
	text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_to_trajectory.ijm")

# use regular expression to insert input and output directory
text[3] <- sub(text, "dir_input = ", paste("dir_input = ","'", video.dir,"';", sep = ""))
text[4] <- sub(text, "dir_output = ", paste("dir_output = ","'",sub("1 - raw/","1 - raw tmp/",video.dir),"';", sep = ""))

# re-create ImageJ macro for batch processing of video files with ParticleTracker
## perhaps put this in a subdirectory of the data folder?
## This is implemented in OSX but not windows, which is as you wrote it
if(.Platform$OS.type == "windows") 
	writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") {
	dir.create(sub("1 - raw","ijmacs",video.dir))	
    writeLines(text,con=paste(sub("1 - raw","ijmacs",video.dir), "/Video_to_trajectory_tmp.ijm",sep=""))
}

#create directory to temporarily store information
dir.create(sub("1 - raw","1 - raw tmp",video.dir),showWarnings = FALSE)

# run to process video files by calling ImageJ / needs fixing for Mac
if(.Platform$OS.type == "unix")
    cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub("1 - raw","ijmacs",video.dir), "Video_to_trajectory_tmp.ijm",sep=""))
if(.Platform$OS.type == "windows")
    cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_trajectory_tmp.ijm')
system(cmd)

# delete temporary file after execution
if(.Platform$OS.type == "windows")
    file.remove("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm")

#copy files produced by ParticleTracker to "2 - trajectory data" directory
dir.create(sub("1 - raw","2 - trajectory data",video.dir))
all.files <- dir(sub("1 - raw/","1 - raw tmp/",video.dir))
ijout.files <- all.files[grep("Traj_", all.files)]
file.copy(paste(sub("1 - raw/","1 - raw tmp/",video.dir),ijout.files, sep = ""),sub("1 - raw","2 - trajectory data",video.dir))
#file.remove(sub("1 - raw/","1 - raw tmp/",video.dir))
}

video_to_trajectory("C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/")

OR

## Windows directory
video.dir <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/"
## OSX / Unix directory (note: imageJ does not like ~)
video.dir <- "/Users/owenpetchey/work/git/franco/data/1 - raw/"
video_to_trajectory(video.dir)



## This function gets the output files produced by the Imagej ParticleTracker
# specify the path where the files which contain the trajectory data are stored
LoadIJ_Traj_Outs <- function(trajdata.dir) {
  
# function to create a unique trajectory ID with each processed result file of the ParticleTracker
unique_traj_ID <- function(dataset){
    
lag <- dataset[2:nrow(dataset),1]
lag <- append(lag,1)
dataset <- cbind(dataset,lag)

# assign a counter creating a unique trajectory
trajectory <- 1
for (i in 1:nrow(dataset)){
    if (dataset[i,12]>dataset[i,1]){
        dataset[i,"trajectory"] <- trajectory}
      
    if (dataset[i,12]<=dataset[i,1]){
        dataset[i,"trajectory"] <- trajectory
        trajectory <- trajectory+1}
    }

dataset$lag <- NULL
# function to convert object name into character string
myfun <- function(x) deparse(substitute(x)) 
assign(paste(myfun(dataset)),dataset,envir = .GlobalEnv)
}
  
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
  
if (i == 1){
dd <- as.data.frame(read.table(textConnection(out)))
dd$file <- rep(ijout.files[1], length(dd$V1))
dd <- unique_traj_ID(dd)
}
  
if (i > 1){
for(j in 2:length(ijout.files)) {
    
dd.t <- as.data.frame(read.table(textConnection(out)))
dd.t$file <- rep(ijout.files[i], length(dd.t$V1))
dd.t <- unique_traj_ID(dd.t)
dd <- rbind(dd, dd.t)}
}}

# invert Y-axis to account for origin in upper-left corner
dd$V2 <- -dd$V2

#rename drop unused information and rename variables
dd <- dd[c(-4,-5,-6,-7,-8,-9,-10)]
names(dd) <- c("frame","X","Y","file","trajectory")

assign("trajectory.data",dd,envir = .GlobalEnv)
write.table(trajectory.data, file = paste(trajdata.dir,"trajectory.data.txt", sep = "/"), sep = "\t")
}

LoadIJ_Traj_Outs("C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/")

trajdata.dir <- "/Users/owenpetchey/work/git/franco/data/2 - trajectory data/"
LoadIJ_Traj_Outs(trackdata.dir)



# function to plot trajectories for overlay (must be merged with original video by ImageJ macro)
# creates a folder containing one jpeg plot containing all the positions till the respective frame
# for the moment colour not assigned by species identity but that's easy to add 
# provide path of " 2 - trajectory data", and the width and height of the original video (I use cropped videos to increase speed while troubleshooting)
create_overlay_plots <- function(path,width,height){ 
trajectory.data <- as.data.frame(read.table(paste(path,"trajectory.data.txt", sep = ""), header = TRUE, sep = "\t"))
file_names <- unique(trajectory.data$file)  
# change path for output
dir.create(sub("2 - trajectory data/","3 - overlay plots/",path))
for (i in 1:length(file_names)){
   #split filename into name and ending for creating directories according to video name
   filename_split <- strsplit(paste(file_names[i]),"\\.")
   filename <- filename_split[[1]]
   dir.create(paste(sub("2 - trajectory data/","3 - overlay plots/",path),sub("Traj_","",filename[1]),sep="/"))
   trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
   j<- 0
   while(j < max(trajectory.data$frame)+1){
      jpeg(paste(sub("2 - trajectory data/","3 - overlay plots/",path),sub("Traj_","",filename[1]),"/frame_",j,".jpg",sep=""), width = as.numeric(width), height = as.numeric(height), quality = 100)
      par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
      print <- subset(trajectory.data_tmp,trajectory.data_tmp$frame <= j, select=c("X","Y","trajectory"))
      plot(print$Y, print$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col="#FFFF00", pch=15, cex=1, asp=1)
      dev.off()
      j <- j+1}
}
# copy master copy of ImageJ macro there for treatment
if(.Platform$OS.type == "windows")
  text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_overlay.ijm",warn = FALSE)
if(.Platform$OS.type == "unix")
	text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_overlay.ijm")

# use regular expression to insert input and output directory
text[3] <- sub(text, "avi_input = ", paste("avi_input = ","'", sub("2 - trajectory data/","1 - raw/",path),"';", sep = ""))
text[4] <- sub(text, "overlay_input = ", paste("overlay_input = ","'", sub("2 - trajectory data/","3 - overlay plots/",path),"';", sep = ""))
text[5] <- sub(text, "overlay_output = ", paste("overlay_output = ","'", sub("2 - trajectory data/","4 - overlays/",path),"';", sep = ""))
  
# re-create ImageJ macro for batch processing of video files with ParticleTracker
if(.Platform$OS.type == "windows")
  writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") {
  writeLines(text,con=paste(sub("1 - raw","ijmacs",video.dir), "/Video_overlay_tmp.ijm",sep=""))
}
  
# create directory to store overlays
dir.create(sub("2 - trajectory data/","4 - overlays/",path))
  
#call IJ macro to merge original video with the trajectory data
if(.Platform$OS.type == "unix"){
  cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub("1 - raw","ijmacs",video.dir), "Video_overlay_tmp.ijm",sep=""))}
if(.Platform$OS.type == "windows"){cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_overlay_tmp.ijm')}

# run ImageJ macro
system(cmd)

# delete temporary file after execution
file.remove("C:/Program Files/Fiji.app/macros/Video_overlay_tmp.ijm")
}

trajdata.dir <- "/Users/owenpetchey/work/git/franco/data/2 - trajectory data/"
path <- trajdata.dir
width <- 2048
height <- 2048

create_overlay_plots("C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/",735,690)
