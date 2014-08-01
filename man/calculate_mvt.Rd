\name{calculate_mvt}
\alias{calculate_mvt}
\title{A function to calculate movement metrics for each trajectory, which can be used to predict the species identity}
\usage{
calculate_mvt(data, to.data, merged.data.folder)
}
\arguments{
  \item{data}{dataframe containing the X- and
  Y-coordinates, the frame and the trajectory ID}

  \item{to.data}{path to the working directory}

  \item{merged.data.folder}{directory where the data is
  saved}
}
\value{
returns a data.table with the movement metrics for each fix
appended to the original data (NB: movement metrics often
need two (e.g. step length), sometimes even three (e.g.,
turning angles) fixes; fixes for which metrics cannot be
calculated are padded with NA)
}
\description{
The function takes the X- and Y-coordinates for each unqiue
trajectory and calculates movement metrics such as the
gross and net displacement, absolute and relative angles
and duration
}

