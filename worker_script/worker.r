## This is example script for running bemovi
## It somewhat duplicates the more detailed example here:
## http://bemovi.info/pages/analysis.html

rm(list=ls())

## For details of installation requirements, please see here:
## http://bemovi.info/pages/installation.html
## Included there is a link to the particle linker application, for example.

## get the bemovi package from github
#library(devtools)
#install_github("pennekampster/bemovi")

library(bemovi)
library(tidyverse)
library(data.table)

## Specify the path of your imagej installation
IJ.path <- "/Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/"

## Specify the path to your particle linker
to.particlelinker <- "/Users/owenpetchey/bemovi.linker"
## Get this here: http://bemovi.info/pages/installation.html

## Specify the project folder (you create this one yourself)
## Use the full path
## All other folders below should be in this
to.data <- "/Users/owenpetchey/Desktop/josh_vid/"

## All folders / paths below are relative to the above specified project folder.

## Specify the location and name of video description folder and file you create yourself
## The / at the end is required here, and below, when giving folder names (sorry!)
video.description.folder <- "0 - video description/"
video.description.file <- "video.description.txt"

## Give location of your raw videos.
## Should be a folder in the above specified project folder
raw.video.folder <- "1 - raw/"

## Some other folders R automatically creates
particle.data.folder <- "2 - particle data/"
trajectory.data.folder <- "3 - trajectory data/"
temp.overlay.folder <- "4a - temp overlays/"
overlay.folder <- "4 - overlays/"
merged.data.folder <- "5 - merged data/"
ijmacs.folder <- "ijmacs/"

## Check various things about the names of the videos
check_video_file_names(to.data, raw.video.folder,
                       video.description.folder, video.description.file)


####### Some parameters #########

## Amount of memory available to various routines
memory <- c(20000)
## video frame rate (in frames per second)
fps <- 30

## size of a pixel in micrometer
pixel_to_scale <- 1000/240

## lag for differencing
difference.lag <- 10

## thesholds for segmentation
thresholds = c(10,255)

## video dimensions in pixels
video_width <- 3840
video_height <- 2160

## particle linking
linkrange <- 1
displacement <- 25



####### Start the work #########

## From here is bespoke for Josh, due to his preprocessing requiring
## insertion of vids into the bemovi pipeline

## Locate particles and organise data
cmd <- paste0("java -Xmx", memory, "m -jar ", IJ.path, 
              "/ij.jar", " -ijpath ", IJ.path, " -macro ", "'", 
              to.data, ijmacs.folder, "analyse_particles.ijm'")
system(cmd)
organise_particle_data(to.data, particle.data.folder)

## View the located particles
dd <- read_delim("~/Desktop/josh_vid/2 - particle data/test.ijout.txt", 
                 "\t", escape_double = FALSE, trim_ws = TRUE)
ggplot(dd, aes(X, Y)) +
  geom_point(size=0.1) +
  xlim(500, 1000) + ylim(500, 1000)

## Link the particles
link_particles(to.data, particle.data.folder, trajectory.data.folder, linkrange = linkrange, 
               disp = displacement, start_vid = 1, memory = memory)

## View the trajectories
dd <- read_csv("~/Desktop/josh_vid/3 - trajectory data/ParticleLinker_test.ijout.txt")
ggplot(filter(dd, traj<300), aes(x, y, col=as.factor(traj))) +
  geom_line() + geom_point() +
  scale_colour_discrete(guide=FALSE)

## Merge the trajectory and particle info
merge_data(to.data, particle.data.folder, trajectory.data.folder, 
           video.description.folder, video.description.file, merged.data.folder)




###### Start from here, once the code above is run once or more
load(paste0(to.data,merged.data.folder,"Master.RData"))

## filter the trajectories -- need to tune the filter settings
trajectory.data.filtered <- filter_data(trajectory.data,
                                        net_filter=50,
                                        duration_filter=1,
                                        detect_filter=0.1,
                                        median_step_filter=5)

## Create overlays of trajectories on the raw vids
create_overlays(to.data, merged.data.folder, raw.video.folder, temp.overlay.folder, 
                overlay.folder, video_width, video_height, difference.lag, type = "label", predict_spec = F, 
                IJ.path, contrast.enhancement = 1, memory = memory)

