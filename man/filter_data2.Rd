\name{filter_data2}
\alias{filter_data2}
\title{Improved function to filter trajectories based on movement and detection}
\usage{
filter_data2(raw_data, net_filter, duration_filter, detect_filter,
  median_step_filter)
}
\arguments{
  \item{data}{Dataframe containing the rawdata on X- and
  Y-coordinates, frame, file and trajectory name,
  morphology and movement metrics}

  \item{max_net_filter}{Minimum net displacement to be
  considered a valid trajectory}

  \item{duration_filter}{Minimum duration to be considered
  a valid trajectory}

  \item{detect_filter}{Minimum detection rate to be
  considered a valid trajectory}

  \item{median_step_filter}{Threshold such that half of the
  step lengths are above the specified value}
}
\value{
Returns a dataset with all fixes of valid trajectories
}
\description{
The function will create a dataframe containing all
trajectories that are valid for further analysis by
selecting on minimum net displacement, detection rate,
trajectory length and the median step_length which needs to
surpass 2 pixels per frame
}

