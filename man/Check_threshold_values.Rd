\name{Check_threshold_values}
\alias{Check_threshold_values}
\title{Function to check the thresholds used in the ImageJ macro}
\usage{
Check_threshold_values(to.data, raw.video.folder, difference.lag,
  thresholds = c(10, 255))
}
\arguments{
  \item{path}{Path to the raw video files to be treated}

  \item{difference_lag}{Numeric value specifying the offset
  between two video frames to compute the difference image}

  \item{thresholds}{Numeric vector containing the min and
  max threshold values}
}
\description{
This function creates a image j macro that can be helpful
for checking the thresholds
}

