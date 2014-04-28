\name{filter_data}
\alias{filter_data}
\title{Function to filter trajectories based on movement and detection}
\usage{
filter_data(raw_data, max_net_filter, duration_filter, detect_filter)
}
\arguments{
  \item{data}{Dataframe containing the rawdata on X- and
  Y-coordinates, frame, file and trajectory name,
  morphology and movement metrics}

  \item{max_net}{Minimum net displacement to be considered
  a valid trajectory}

  \item{duration}{Minimum duration to be considered a valid
  trajectory}

  \item{detect}{Minimum detection rate to be considered a
  valid trajectory}
}
\value{
Returns two datasets: one containing the artefacts, one the
valid trajectory data
}
\description{
The function will create two dataframes: one containing the
trajectories that are artefacts, one containing the valid
trajectories for analysis
}

