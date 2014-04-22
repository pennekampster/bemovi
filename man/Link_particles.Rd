\name{Link_particles}
\alias{Link_particles}
\title{Function to link the particle coordinates through time}
\usage{
Link_particles(to.data, particle.data.folder, trajectory.data.folder,
  memory = memory.alloc, linkrange = 5, disp = 20)
}
\arguments{
  \item{path}{Path to the directory where the
  ParticleAnalyzer output is saved (as text files)}

  \item{memory}{Numeric value specifying the amount of
  memory available to the ParticleLinker}

  \item{linkrange}{Numeric value passed to the
  ParticleLinker specifying the range of adjacent frames
  which are taken into account when a trajectory is
  assembled}

  \item{disp}{A numeric value that specifies the maximum
  displacement of a given particle between two frames}
}
\value{
Returns a text file which contains the X- and
Y-coordinates, the frame and a trajectory ID
}
\description{
The function converts the XY-coordinates of the
ParticleAnalyzer into a temporary structure (e.g. folder
with all coordinates per frame in a separate text file)
that can be read by the standalone ParticleLinker
}

