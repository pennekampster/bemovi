\name{summarize_trajectories}
\alias{summarize_trajectories}
\title{Function to summarize the morphology, movement and their variability on the trajectory level}
\usage{
summarize_trajectories(data, write = FALSE, to.data, merged.data.folder)
}
\arguments{
  \item{data}{dataframe with the information on morphology
  and movement for each frame}

  \item{to.data}{path to the working directory}

  \item{merged.data.folder}{directory where the global
  database is saved}

  \item{write}{logical argument to indicate whether
  aggregated information should be saved to disk}
}
\value{
returns a dataframe with the aggregated morphology and
movement information for each trajectory
}
\description{
Takes the data comprising the information for each frame
and calculates mean and sd along the trajectory
}

