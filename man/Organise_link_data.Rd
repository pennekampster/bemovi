\name{Organise_link_data}
\alias{Organise_link_data}
\title{Function to merge the output of the ParticleLinker into one large database}
\usage{
Organise_link_data(to.data, trajectory.data.folder)
}
\arguments{
  \item{path}{Path to the directory where ParticleLinker
  output is stored}
}
\value{
Returns a text file (trajectory.data.txt) containing the X-
and Y coordinates of a given trajectory, the frame, the
trajectory ID and the file name of the video from which the
data was extracted
}
\description{
Merge the trajectory data from the ParticleLinker into one
data file
}

