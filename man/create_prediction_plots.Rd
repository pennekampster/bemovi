\name{create_prediction_plots}
\alias{create_prediction_plots}
\title{Function to create an overlay between the raw video data and the extracted trajectories; different colours are used to indicate the different species}
\usage{
create_prediction_plots(data, path, prediction.folder, width, height,
  difference.lag, original.vid.contrast.enhancement = 1,
  memory = memory.alloc)
}
\arguments{
  \item{data}{dataframe containing the X- and
  Y-coordinates, the frame, the filename from which the
  data was extracted, the unique trajectory ID and a column
  (predict_spec) with the species labels}

  \item{path}{path to the output saved from the
  ParticleLinker and the raw video directory}

  \item{prediction.folder}{directory where the overlays are
  saved}

  \item{width}{width of the raw video}

  \item{height}{The height of the raw video}

  \item{difference_lag}{Numeric value specifying the offset
  between two video frames to compute the difference image}

  \item{memory}{Numeric value specifying the amount of
  memory available to ImageJ}
}
\description{
A function to overlay the trajectories of different species
with different colours with the original video using plots
created in R and then processed in ImageJ
}

