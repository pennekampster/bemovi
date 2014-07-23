\name{check_threshold_values}
\alias{check_threshold_values}
\title{Function to adjust the thresholds used in the ImageJ macro}
\usage{
check_threshold_values(to.data, raw.video.folder, ijmacs.folder,
  vid_select = 0, difference.lag, memory)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{raw.video.folder}{directory with the raw video
  files}

  \item{ijmacs.folder}{directory where the check_trehsolds
  macro is saved}

  \item{vid_select}{video selected to find appropriate
  thresholds; default is the first video}

  \item{difference_lag}{numeric value specifying the offset
  between two frames of a video}

  \item{memory}{memory allocated to ImageJ}
}
\description{
This function creates an ImageJ macro that can be helpful
for checking the thresholds; the macro will be saved in the
ImageJ macro directory in the working directory and then
automatically opened in ImageJ. Depending on the video
size, it might take some time to open the video and the
thresholding menu. By playing with the treshold manually,
the correct value is determined and can be subsequently
provided to the appropriate R functions performing the
segmentation.
}

