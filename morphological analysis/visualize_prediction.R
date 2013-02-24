library(sqldf)

## Owen's paths
to.code.owen <- "/Users/owenpetchey/work/git/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Desktop/hard.test/"

## Frank's paths
to.code.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/automation/"
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"


## General folders
trajectory.data.folder <- "2 - trajectory data/"
predict.out <- "6 - prediction/"

## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
  to.data <- to.data.frank
  to.code <- to.code.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
  to.data <- to.data.owen
  to.code <- to.code.owen}

path <- trajectory.data.folder
width <- 2048
height <- 2048
difference.lag <- 25

# load raw trajectory data with all positions
trajectory.data <- read.table(paste0(to.data,trajectory.data.folder,"trajectory.data.txt"), header=TRUE, sep="\t")

#merge prediction from NN with trajectory data for plotting
predict_trajects <- sqldf("select *
                           from 'trajectory.data' t, predict p 
                           where p.file=t.file and p.trajectory=t.trajectory")

# plot classification result (different colours)
create_prediction_plots <- function(path,width,height,difference.lag){ 
  trajectory.data <- predict_trajects
  file_names <- unique(trajectory.data$file)  
  
  # change path for output
  dir.create(to.data,predict.out,showWarnings = FALSE)
  for (i in 1:length(file_names)){
    dir.create(paste0(to.data,predict.out)) 
    trajectory.data_tmp <- subset(trajectory.data,file == file_names[i])
    jpeg(paste(to.data,predict.out,"/",file_names[1],".jpg",sep=""), width = as.numeric(width), height = as.numeric(height), quality = 100)
    par(mar = rep(0, 4), xaxs=c("i"), yaxs=c("i"))
    plot(trajectory.data_tmp$Y, trajectory.data_tmp$X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), col=trajectory.data_tmp$predict_spec, pch=15, cex=1, asp=1)
    dev.off()
  }
}

create_prediction_plots(path,width,height,difference.lag)
