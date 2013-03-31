library(plyr)
library(sqldf)
library(randomForest)

# code to do classification based on both morphology and movement data
# will integrate all of OWen's previous ideas and re-use available codes

## Owen's paths
to.code.owen <- "/Users/owenpetchey/work/git/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Desktop/hard.test/"

## Frank's paths
to.code.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/automation/"
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"

## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
  to.code <- to.code.frank
  to.data <- to.data.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
  to.code <- to.code.owen
  to.data <- to.data.owen}

# specify folder for saving the predictions (jpeg plots) and the video overlays
raw.video.folder <- "1 - raw/"
trajectory.data.folder <- "2 - trajectory data/"
class.data <- "6 - Classification/"
prediction.folder <- "7 - Prediction/"
prediction.folder2 <- "8 - Prediction overlay/"

# import morphology data
morphology.data <- read.table(paste0(to.data,class.data,"morphology.data.summary.txt"), sep = "\t")

# import trajectory data
trajectory.data <- read.table(paste0(to.data,class.data,"trajectory.data.summary.txt"), sep = "\t")

#merge morphology data with meaningful trajectory data (meaningful = show properties used for filter trajectories)
all_data <- sqldf("select *
                  from 'trajectory.data' t, 'morphology.data' m
                  where t.file=m.file AND t.trajectory=m.trajectory
                  ")

# labelling files according to species
all_data$species <- factor(all_data$file,
                    levels = c("Data34","Data38","Data45","Data49","Data52","Data57"),
                    labels = c("Colpidium", "Paramecium", "Colpidium&Paramecium","Loxocephalus","Loxocephalus&Colpidium","Colpidium&Paramecium&Loxocephalus")) 

# only take monocultures
species_subsets <- sqldf("select *
                         from all_data
                         where species in ('Colpidium', 'Paramecium', 'Loxocephalus')")


# take monocultures of three species and classify by morphology (excluding the grey value for the moment)
three_moncultures <- sqldf("select *
                       from all_data
                       where species in ('Colpidium','Paramecium', 'Loxocephalus')")

# define classification table function
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}

# test randomForest classification for species recognition
# use factor function to only classify species comprised in the dataset
train_rf <- cbind(three_moncultures[, c(4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21)])
samp <- sample(1:length(train_rf[,1]), length(train_rf[,1])/2)
no.samp <- c(1:length(train_rf[,1]))[-samp]
# train randomForest
rf_fit <- randomForest(factor(species) ~ net_speed + period + NGDR + sd_turning + grey + area + perimeter + major + minor + AR , data=train_rf[samp,], importance=TRUE, proximity=TRUE)
print(rf_fit)
plot(rf_fit)
varImpPlot(rf_fit)
MDSplot(rf_fit, train_rf[samp,]$species)
table(factor(train_rf[no.samp,]$species), predict(rf_fit, train_rf[no.samp,]))

# use monoculture to predic species identity in communities
three_monocult <- sqldf("select *
                         from all_data
                         where species in ('Colpidium','Paramecium','Loxocephalus')")

mixed_cult <- sqldf("select *
                    from all_data
                    where species in ('Colpidium&Paramecium', 'Loxocephalus&Colpidium', 'Colpidium&Paramecium&Loxocephalus')")

# classification by randomForest
train_rf <- cbind(three_monocult[, c(4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21)])
rf_fit <- randomForest(factor(species) ~ net_speed + period + NGDR + sd_turning + grey + area + perimeter + major + minor + AR , data=train_rf, importance=TRUE, proximity=TRUE)
predict_spec <- predict(rf_fit, mixed_cult[, c(4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21)])
predict <- cbind(mixed_cult,predict_spec)

# visualize prediction
trajectory_raw <- read.table(paste0("C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/trajectory.data.txt"), header=TRUE, sep="\t")
trajectory_raw$id <- paste(trajectory_raw$file,trajectory_raw$trajectory,sep="-")

predict_visual <- sqldf("select t.*, p.predict_spec 
                        from trajectory_raw t
                        left join predict p 
                        on p.id=t.id
                        where t.file in ('Data45','Data52','Data57')")

predict_visual$predict_spec <- factor(predict_visual$predict_spec)

create_prediction_plots <- function(path,width,height,difference.lag){ 
# function that produces labelled overlays based on the classification and the original tracks:
# different species are coloured
# numbered objects without halo where filtered out before classification and are therefore artefacts,
# though sometimes valid trajects are filtered out
  
trajectory.data <- predict_visual
file_names <- unique(trajectory.data$file)  
# change path for output
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

# copy master copy of ImageJ macro there for treatment
if(.Platform$OS.type == "windows")
  text <- readLines("C:/Users/Frank/Documents/PhD/Programming/franco/automation/ImageJ macros/Video_overlay.ijm",warn = FALSE)
if(.Platform$OS.type == "unix")
  text <- readLines("/Users/owenpetchey/work/git/franco/automation/ImageJ macros/Video_overlay.ijm")
  
text <- readLines(paste(to.code, "ImageJ macros/Prediction_overlay.ijm", sep=""))
  
  
# use regular expression to insert input and output directory
text[grep("avi_input = ", text)] <- paste("avi_input = ","'", paste0(path,raw.video.folder),"';", sep = "")
text[grep("overlay_input = ", text)] <- paste("overlay_input = ","'", paste0(path,prediction.folder),"';", sep = "")
text[grep("overlay_output = ", text)] <- paste("overlay_output = ","'", paste0(path,prediction.folder2),"';", sep = "")
text[grep("lag =", text)] <- paste("lag = ",difference.lag,";", sep = "")
  

# re-create ImageJ macro for batch processing of video files with ParticleTracker
if(.Platform$OS.type == "windows")
  writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Prediction_overlay_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") {
  ijmacs.folder <- sub(raw.video.folder,"ijmacs/",video.dir)
  writeLines(text,con=paste(ijmacs.folder, "/Prediction_overlay_tmp.ijm",sep=""))
}
  
# create directory to store overlays
dir.create(sub(trajectory.data.folder,prediction.folder2,path))
  
#call IJ macro to merge original video with the trajectory data
if(.Platform$OS.type == "unix"){
  cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub(raw.video.folder,"ijmacs",video.dir), "/Prediction_overlay_tmp.ijm",sep=""))
}
if(.Platform$OS.type == "windows"){
  cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Prediction_overlay_tmp.ijm')}
  
# run ImageJ macro
system(cmd)
  
# delete temporary file after execution
if(.Platform$OS.type == "windows")
   file.remove("C:/Program Files/Fiji.app/macros/Prediction_overlay_tmp.ijm")
}

create_prediction_plots(to.data,735,690,10)




