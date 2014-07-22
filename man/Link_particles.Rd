\name{link_particles}
\alias{link_particles}
\title{Function to link the particle coordinates through time}
\usage{
link_particles(to.data, particle.data.folder, trajectory.data.folder,
  memory = memory.alloc, linkrange = 1, disp = 20, start_vid = 1)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{particle.data.folder}{directory where the
  ParticleAnalyzer output is saved (as text files)}

  \item{trajectory.data.folder}{directory where the
  ParticleLinker is saved (as text files)}

  \item{memory}{numeric value specifying the max amount of
  memory allocated to the ParticleLinker}

  \item{linkrange}{numeric value passed to the
  ParticleLinker specifying the range of adjacent frames
  which are taken into account when a trajectory is
  re-constructed}

  \item{disp}{numeric value that specifies the maximum
  displacement of a given particle between two frames}

  \item{start_vid}{numeric value to indicate whether the
  linking should be started with a video other than the
  first}
}
\value{
Returns a single text file per video containing the X- and
Y-coordinates, the frame and a trajectory ID
}
\description{
The function converts the XY-coordinates of the
ParticleAnalyzer into a temporary structure (e.g. folder
with all coordinates per frame in a separate text file)
that can be read by the standalone ParticleLinker
}

