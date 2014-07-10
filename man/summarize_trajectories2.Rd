\name{summarize_trajectories2}
\alias{summarize_trajectories2}
\title{Function to summarize the mean morphology and movement and its variability on a trajectory level}
\usage{
summarize_trajectories2(data, write = FALSE)
}
\arguments{
  \item{data}{Dataframe with the information on morphology
  and movement for each frame}

  \item{write}{Logical argument to indicate whether
  aggregated information should save to disk}
}
\value{
Returns a dataframe with the aggregated information for
each trajectory
}
\description{
Takes the data comprising the information for each frame
and calculates mean and sd along the trajectory
}

