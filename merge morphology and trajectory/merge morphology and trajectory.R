rm(list=ls())

## Owen's paths
to.code.owen <- "/Users/owenpetchey/work/git/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Desktop/franco.test.vids/"

## Frank's paths
to.code.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/automation/"
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"

## General folders
trajectory.data.folder <- "2 - trajectory data/"
particle.analyzer.folder <- "5 - Particle Analyzer data/"

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

# load trajectory.data
trajectory.data <- read.table(paste0(to.data,trajectory.data.folder,"trajectory.data.txt"), row.names=1)
trajectory.data$file <- gsub("Traj_" ,"",trajectory.data$file)
trajectory.data$file <- gsub(".avi.txt" ,"",trajectory.data$file)
trajectory.data$X <- round(-trajectory.data$X, 0)
trajectory.data$Y <- round(trajectory.data$Y, 0)

#load morphological.data
morphology.data <- read.table(paste0(to.data,particle.analyzer.folder,"morphology.data.txt"), row.names=1)
morphology.data$frame <- morphology.data$Slice
morphology.data$Slice <- NULL
morphology.data$file <- gsub(".ijout.txt" ,"",morphology.data$file)
morphology.data$X <- round(morphology.data$X, 0)
morphology.data$Y <- round(morphology.data$Y, 0)
# subset morphology.data to first 101 frames
morphology.data <- subset(morphology.data,morphology.data$frame < 101)

subset_m <- subset(morphology.data, file == "Data34")
subset_t <- subset(trajectory.data, file == "Data34")

plot(subset_m$X,subset_m$Y,asp=1,col="red")
par(new=T)
plot(subset_t$Y,subset_t$X,col="blue",asp=1)

# merge morphological and trajectory data based on frame, file and X and Y
