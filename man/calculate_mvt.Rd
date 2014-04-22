\name{calculate_mvt}
\alias{calculate_mvt}
\title{A function to calculate the movement metrics used for species identification}
\usage{
calculate_mvt(data)
}
\arguments{
  \item{data}{Dataframe containing the X- and
  Y-coordinates, the frame and the trajectory ID}
}
\value{
Returns the original dataframe with movement metrics
attached
}
\description{
The function takes the X- and Y-coordinates for each unqiue
trajectory and calculates movement metrics such as the
gross and net displacement, absolute and relative angles
and duration
}

