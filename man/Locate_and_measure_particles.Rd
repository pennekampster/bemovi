\name{Locate_and_measure_particles}
\alias{Locate_and_measure_particles}
\title{Function to extract morphological measurements and X- and Y-coordinates for moving particles}
\usage{
Locate_and_measure_particles(to.data, raw.video.folder, particle.data.folder,
  difference.lag, min_size = 0, max_size = 10000, thresholds = c(0, 1000),
  memory = memory.alloc)
}
\arguments{
  \item{path}{Path to the raw videos to be treated}

  \item{particle.data.folder}{Directory to which the data
  is saved as a text file}

  \item{difference_lag}{Numeric value specifying the offset
  between two video frames to compute the difference image}

  \item{min_size}{Minimum size for detection of particles}

  \item{max_size}{Maximum size for detection of particles}

  \item{thresholds}{Numeric vector containing the min and
  max threshold values}

  \item{memory}{Numeric value specifying the amount of
  memory available to ImageJ}
}
\value{
Saves the output of the ParticleAnalyzer function of ImageJ
as a text file in the output directory
}
\description{
Function calls ImageJ software and its ParticleAnalyzer
function to extract for each frame of the video several
morphological descriptors and the X- and Y-coordinates of
all moving particles
}

