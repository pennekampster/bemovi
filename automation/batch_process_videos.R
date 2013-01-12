# code to batch process videos by ImageJ and ParticleTracker plugin
# better provide function specifying the path where videos are

# specify directory where videos are
video.dir = "C:/Users/Frank/Desktop/test/"

# copy master copy of ImageJ macro there for treatment
text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_to_trajectory.ijm")

# use regular expression to insert input and output directory
text[3] <- sub(text, "dir_input = ", paste("dir_input = ","'", video.dir,"';", sep = ""))
text[4] <- sub(text, "dir_output = ", paste("dir_output = ","'", video.dir,"';", sep = ""))

# re-create ImageJ macro for batch processing of video files with ParticleTracker
writeLines(text,con=paste("C:/Program Files/Fiji.app/Video_to_trajectory_tmp.ijm",sep=""),sep="\n")

# run to process video files by calling ImageJ 
if(.Platform$OS.type == "unix"){
cmd <- c("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ")}
if(.Platform$OS.type == "windows"){
cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe -macro Video_to_trajectory_tmp.ijm"')}
system(cmd)

#delete temporary file after execution
file.remove("C:/Program Files/FIJI.app/Video_to_trajectory_tmp.ijm")