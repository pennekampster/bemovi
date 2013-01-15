# code to batch process videos by ImageJ and ParticleTracker plugin
# provide directory where raw videos are stored
video_to_trajectory <- function(video.dir) {
# copy master copy of ImageJ macro there for treatment
text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_to_trajectory.ijm")

# use regular expression to insert input and output directory
text[3] <- sub(text, "dir_input = ", paste("dir_input = ","'", video.dir,"';", sep = ""))
text[4] <- sub(text, "dir_output = ", paste("dir_output = ","'",sub("1 - raw/","1 - raw tmp/",video.dir),"';", sep = ""))

# re-create ImageJ macro for batch processing of video files with ParticleTracker
writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm",sep=""),sep="\n")

#create directory to temporarily store information
dir.create(sub("1 - raw","1 - raw tmp",video.dir))

# run to process video files by calling ImageJ / needs fixing for Mac
if(.Platform$OS.type == "unix"){
cmd <- c("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ")}
if(.Platform$OS.type == "windows"){
cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_trajectory_tmp.ijm')}
system(cmd)

# delete temporary file after execution
#file.remove("C:/Program Files/Fiji.app/macros/Video_to_trajectory_tmp.ijm")

#copy files produced by ParticleTracker to "2 - trajectory data" directory
dir.create(sub("1 - raw","2 - trajectory data",video.dir))
all.files <- dir(sub("1 - raw/","1 - raw tmp/",video.dir))
ijout.files <- all.files[grep("Traj_", all.files)]
file.copy(paste(sub("1 - raw/","1 - raw tmp/",video.dir),ijout.files, sep = ""),sub("1 - raw","2 - trajectory data",video.dir))
#file.remove(sub("1 - raw/","1 - raw tmp/",video.dir))
}

video_to_trajectory("C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/")




## This function gets the output files produced by the Imagej ParticleTracker
# specify the path where the files which contain the trajectory data are stored
LoadIJ_Traj_Outs <- function(video.dir) {
  
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
all.files <- dir(path=video.dir)
ijout.files <- all.files[grep("Traj_", all.files)]

for (i in 1:length(ijout.files)){
text <- readLines(paste(video.dir, ijout.files[i], sep="/"))
  
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
write.table(trajectory.data, file = paste(video.dir,"trajectory.data.txt", sep = "/"), sep = "\t")
}

LoadIJ_Traj_Outs("C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/")

