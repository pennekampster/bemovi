\name{Check_video_file_names}
\alias{Check_video_file_names}
\title{Function to check if there are any unsupported file types or names}
\usage{
Check_video_file_names(to.data, raw.video.folder)
}
\arguments{
  \item{path}{Path to the raw videos to be treated}
}
\value{
Returns an error message and a list with unsupported files
or names
}
\description{
Checks the files in the raw data for the supported avi and
cxd file formats and that file names do not contain periods
except before the file type extension
}

