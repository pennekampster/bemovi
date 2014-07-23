\name{locate_and_measure_particles}
\alias{locate_and_measure_particles}
\title{Function to extract morphological measurements and X- and Y-coordinates for moving particles}
\usage{
locate_and_measure_particles(to.data, raw.video.folder, particle.data.folder,
  difference.lag, min_size = 0, max_size = 10000, thresholds = c(0, 1000),
  memory = memory.alloc)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{raw.video.folder}{directory with the raw video
  files}

  \item{particle.data.folder}{directory to which the data
  is saved as a text file}

  \item{difference_lag}{numeric value specifying the offset
  between two video frames to compute the difference image}

  \item{min_size}{minimum size for detection of particles}

  \item{max_size}{maximum size for detection of particles}

  \item{thresholds}{vector containing the min and max
  threshold values (e.g. c(10,255))}

  \item{memory}{numeric value specifying the amount of
  memory available to ImageJ}
}
\value{
saves the output of the ParticleAnalyzer function of ImageJ
as a text file in the output directory
}
\description{
Function calls ImageJ software and its ParticleAnalyzer
function to extract for each frame of the video several
morphological descriptors and the X- and Y-coordinates of
all moving particles
}

