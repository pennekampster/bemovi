# Code to summarize the morphology of particles
# 1. For all moving particles morphology is extracted by means of the Particle Analyzer function
# 2. Morphological information is merged with the trajectories extracted by the Particle tracker on X,Y,frame and file
# 3. Mean morphology is calculated by aggregating, data stored for classification

rm(list=ls())

library(sqldf)
library(plyr)

## Owen's paths
to.data.owen <- "/Users/owenpetchey/Desktop/franco.test.vids/"

## Frank's paths
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"

## General folders
trajectory.data.folder <- "2 - trajectory data/"
particle.analyzer.folder <- "5 - Particle Analyzer data/"
merge.folder <- "merge morphology and trajectory/"

## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
  to.data <- to.data.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
  to.data <- to.data.owen}

# load trajectory.data
trajectory.data <- read.table(paste0(to.data,trajectory.data.folder,"trajectory.data.txt"), row.names=1)
trajectory.data$X <- round_any(-trajectory.data$X, 5)
trajectory.data$Y <- round_any(trajectory.data$Y, 5)
# trajectory frame starts with 0, therefore add one to adjust to morphology data
trajectory.data$frame <- trajectory.data$frame+1

#load morphological.data
morphology.data <- read.table(paste0(to.data,particle.analyzer.folder,"morphology.data.txt"), row.names=1)
morphology.data$frame <- morphology.data$Slice
morphology.data$Slice <- NULL
morphology.data$X <- round_any(morphology.data$X, 5)
morphology.data$Y <- round_any(morphology.data$Y, 5)

# subsample to do visual control
#subset_m <- subset(morphology.data, file == "Data34")
#subset_t <- subset(trajectory.data, file == "Data34")
# check why X and Y differ between trajectories and morphology
#plot(subset_m$X,subset_m$Y,pch=16,asp=1,col="red")
#par(new=T)
#plot(subset_t$Y,subset_t$X,col="blue",pch=1,asp=1, type="p")

# merge morphological and trajectory data based on frame, file and X and Y
# X and Y are changed between data sources, still have to find where confounding takes place
merge <- sqldf("select *  
                from 'trajectory.data' t
                left join 
                'morphology.data' m
                on t.file=m.file AND t.X=m.Y AND t.Y=m.X AND t.frame=m.frame;
               ")

morphology_summary <- sqldf("select 
                            file,
                            trajectory,
                            avg(area) as area,
                            avg(mean) as grey,
                            avg(perimeter) as perimeter,
                            avg(major) as major,
                            avg(minor) as minor,
                            avg(AR) as AR,
                            avg(circ_) as circularity
                            from merge
                            group by file, trajectory
                            ")

# export aggregated data on morphology
write.table(morphology_summary, file = paste(gsub("data/",merge.folder,to.data),"morphology.data.summary.txt", sep = "/"), sep = "\t")