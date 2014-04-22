\name{create_prediction_plots}
\alias{create_prediction_plots}
\title{Function to create an overlay between the raw data and the extracted trajectories}
\usage{
create_prediction_plots(path, width, height, difference.lag,
  memory = memory.alloc)
}
\arguments{
  \item{path}{Path to the output saved from the
  ParticleLinker and the raw video directory}

  \item{width}{The width of the raw video}

  \item{height}{The height of the raw video}

  \item{difference_lag}{Numeric value specifying the offset
  between two video frames to compute the difference image}

  \item{original.vid.contrast.enhancement}{A numeric value
  to increase the contrast of the raw video}

  \item{memory}{Numeric value specifying the amount of
  memory available to ImageJ}
}
\description{
A function to overlay the trajectories and the original
video using plots created in R and then processed in
ImageJ; two different visualization types are available
}

