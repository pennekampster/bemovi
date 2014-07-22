\name{check_threshold_values}
\alias{check_threshold_values}
\title{Function to adjust the thresholds used in the ImageJ macro}
\usage{
check_threshold_values(to.data, raw.video.folder, ijmacs.folder, difference.lag,
  thresholds = c(10, 255))
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{raw.video.folder}{directory with the raw video
  files}

  \item{ijmacs.folder}{directory where the check_trehsolds
  macro is saved}

  \item{difference_lag}{numeric value specifying the offset
  between two frames of a video}

  \item{thresholds}{numeric vector containing the min and
  max threshold values (i.e. between 0 and 255 given that
  videos should be 8-bit grey scale)}
}
\description{
This function creates an ImageJ macro that can be helpful
for checking the thresholds; the macro will be saved in the
ImageJ macro directory in the working directory and has to
be opened in ImageJ manually. After running the macro, the
video stack will be opened until the thresholding step. By
playing with the treshold manually, the correct value is
determined and can be provided to the R script.
}

