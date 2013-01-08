## This is the code that one runs in order to do the following:
## 1. Create and run an imageJ macro for each of the video files in a specified directory/folder.
##	  At present imageJ runs its particle analyser and saves the data to disk. 
## 2. Load into R the data produced by imagej.
## 3. Some analyses of that data.


rm(list=ls())


source('~/work/git/franco/automation/ij.functions.r')

## lag for image subtraction
difference.lag <- 10

## the directory in which the videos are stored
vid.dir <- "/Users/owenpetchey/Desktop/sub.test1"

## read the file that gives the important information about each video
file.sample.info <- read.csv(paste(vid.dir, "frank.video.description.txt", sep="/"))

## make the imagej macros
MakeIJMacros(vid.dir, difference.lag=difference.lag)

## run the imagej macros... this can take some time.
## ensure imagej is closed first
RunIJMacros(vid.dir)

## load into R the data files created by imagej
dd <- LoadIJOuts(vid.dir)
str(dd)
unique(dd$X)




## NOT WORKING PROPERLY FROM HERE ONWARDS
## put the sample info into the dataset
dd$magnification <- file.sample.info$magnification[match(dd$X, file.sample.info$filename)]
dd$volume <- file.sample.info$volume[match(dd$X, file.sample.info$filename)]
dd$real.density <- file.sample.info$real.density[match(dd$X, file.sample.info$filename)]
dd1 <- aggregate(dd$X, list(file=dd$X, Slice=dd$Slice, volume=dd$volume,
							magnification=dd$magnification,
							real.density=dd$real.density), length)
str(dd1)
dd1
dd1$est <- dd1$x * dd1$magnification
dd1$est <- dd1$est / dd1$volume
boxplot(est ~ as.numeric(as.factor(dd1$file)), dd1, cex.axis=0.4)
points(real.density ~ as.numeric(as.factor(dd1$file)), dd1, pch=21, bg="red")
dd1$real.density


