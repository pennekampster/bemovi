\name{check_video_file_names}
\alias{check_video_file_names}
\title{Function to check that video files have extension cxd or avi, and that they are otherwise compatible.}
\usage{
check_video_file_names(to.data, raw.video.folder)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{raw.video.folder}{directory with the raw video
  files}
}
\value{
returns an error message and a list with unsupported files
or names
}
\description{
Checks the files in the raw data for the supported avi and
cxd file formats and that file names do not contain periods
except before the file type extension
}

