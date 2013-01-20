MakeIJMacros <- function(video.dir, difference.lag) {

# copy master copy of ImageJ macro there for treatment
if(.Platform$OS.type == "unix")
  text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_to_morphology.ijm")
if(.Platform$OS.type == "windows")
  text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_to_morphology.ijm")

# use regular expression to insert input & output directory as well as difference lag
text[3] <- sub(text, "avi_input = ", paste("avi_input = ","'", video.dir,"';", sep = ""))
text[4] <- sub(text, "avi_output = ", paste("avi_output = ","'",sub("1 - raw/","5 - Particle Analyzer data/",video.dir),"';", sep = ""))
text[5] <- sub(text, "lag = ", paste("lag = ",difference.lag,";", sep = ""))


# re-create ImageJ macro for batch processing of video files with Particle Analyzer
if(.Platform$OS.type == "windows") 
  writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_morphology.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") {
  dir.create(sub("1 - raw","ijmacs",video.dir))	
  writeLines(text,con=paste(sub("1 - raw","ijmacs",video.dir), "/Video_to_morphology.ijm",sep=""))
}

#create directory to store Particle Analyzer data
dir.create(sub("1 - raw","5 - Particle Analyzer data",video.dir),showWarnings = FALSE)

# run to process video files by calling ImageJ / needs fixing for Mac
if(.Platform$OS.type == "unix")
  cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub("1 - raw","ijmacs",video.dir), "Video_to_morphology.ijm",sep=""))
if(.Platform$OS.type == "windows")
  cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_morphology.ijm')
system(cmd)

# delete temporary file after execution
if(.Platform$OS.type == "windows")
  file.remove("C:/Program Files/Fiji.app/macros/Video_to_morphology.ijm")

}

## This function gets the output files produced by the imagej macros previously created (by function MakeIJMacros)
## and run by function RunIJMacros
LoadIJOuts <- function(IJ_output.dir) {

## the macro file names
all.files <- dir(path=IJ_output.dir)
ijout.files <- all.files[grep("ijout", all.files)]
	
dd <- read.delim(paste(IJ_output.dir, ijout.files[1], sep="//"))
dd$file <- rep(ijout.files[1], length(dd$X))
	
for(i in 2:length(ijout.files)) {

	dd.t <- read.delim(paste(IJ_output.dir, ijout.files[i], sep="//"))
	dd.t$file <- rep(ijout.files[i], length(dd.t$X))
	dd <- rbind(dd, dd.t)
}

assign("morphology.data",dd,envir = .GlobalEnv)
write.table(morphology.data, file = paste(IJ_output.dir,"morphology.data.txt", sep = "/"), sep = "\t")
}

