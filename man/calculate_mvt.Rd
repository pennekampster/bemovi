\name{calculate_mvt}
\alias{calculate_mvt}
\title{A function to calculate the movement metrics used for species identification}
\usage{
calculate_mvt(data, to.data, merged.data.folder)
}
\arguments{
  \item{data}{Dataframe containing the X- and
  Y-coordinates, the frame and the trajectory ID}
}
\value{
Saves the original dataframe with movement metrics attached
to the disk
}
\description{
The function takes the X- and Y-coordinates for each unqiue
trajectory and calculates movement metrics such as the
gross and net displacement, absolute and relative angles
and duration
}

