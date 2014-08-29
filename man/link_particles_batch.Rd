\name{link_particles_batch}
\alias{link_particles_batch}
\title{Function to link the particle coordinates through time}
\usage{
link_particles_batch(to.data, particle.data.folder, trajectory.data.folder,
  linkrange = 1, disp = 10, start_vid = 1, memory = 512,
  batch_size = 25, overlap = 5)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{particle.data.folder}{directory where the
  ParticleAnalyzer output is saved (as text files)
  (temporary)}

  \item{trajectory.data.folder}{directory where the
  ParticleLinker is saved (as text files) (temporary???)}

  \item{memory}{numeric value specifying the max amount of
  memory allocated to the ParticleLinker (defaults to 512)}

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
The function takes the XY-coordinates provided by the
ImageJ ParticleAnalyzer and uses a standalone version of
the ImageJ MOSAIC plugin ParticleLinker to create
trajectories. This requires some creation of temporary
files, which are subsequently deleted.
}

