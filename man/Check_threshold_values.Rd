\name{check_threshold_values}
\alias{check_threshold_values}
\title{Function to adjust the thresholds used for the segmentation of the difference video. If you run fRanco for the first time, verify with this function that all target objects are
properly identified}
\usage{
check_threshold_values(to.data, raw.video.folder, ijmacs.folder,
  vid_select = 0, difference.lag, thresholds, memory = memory.alloc)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{raw.video.folder}{directory with the raw video
  files}

  \item{ijmacs.folder}{directory where the check_trehsolds
  macro is saved}

  \item{vid_select}{video selected to find appropriate
  thresholds; default is the first video}

  \item{difference.lag}{numeric value specifying the offset
  between two frames of a video}

  \item{thresholds}{Numeric vector containing the min and
  max threshold values}

  \item{memory}{memory (in MB) allocated to ImageJ}
}
\description{
This function creates an ImageJ macro that can be helpful
for checking the thresholds specified in the user section;
the macro will be saved in the ImageJ macro directory in
the working directory and then automatically opened in
ImageJ. Depending on the video size, it might take some
time to open the video and the thresholding menu. First,
the tresholds are set to the values given in the user
section, but by playing with the treshold manually, a
correct value is determined and if need be changed in the
user section.
}

