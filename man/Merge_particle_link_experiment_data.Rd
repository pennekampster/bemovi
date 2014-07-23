\name{merge_particle_link_experiment_data}
\alias{merge_particle_link_experiment_data}
\title{Function to create global database containg morphology data and trajectory information}
\usage{
merge_particle_link_experiment_data(to.data, particle.data.folder,
  trajectory.data.folder, video.description.folder, video.description.file,
  merged.data.folder)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{particle.data.folder}{directory containing the
  global morphology data}

  \item{trajectory.data.folder}{directory containing the
  global trajectory data}

  \item{video.description.folder}{directory containing the
  video description file}

  \item{video.description.file}{name of the video
  description file}

  \item{merged.data.folder}{directory where the global
  database is saved}
}
\value{
saves the global database to the merged.data.folder
}
\description{
Merges the morphology data, the trajectory data with the
description file of the experiment containing the
information on sampling units, treatments and replication.
The files are merged by the use of the video file names
}

