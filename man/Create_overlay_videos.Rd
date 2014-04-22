\name{Create_overlay_videos}
\alias{Create_overlay_videos}
\title{Function to create an overlay between the raw data and the extracted trajectories}
\usage{
Create_overlay_videos(to.data, trajectory.data.folder, raw.video.folder,
  temp.overlay.folder, overlay.folder, width, height, difference.lag,
  type = "traj", original.vid.contrast.enhancement = 1,
  memory = memory.alloc)
}
\arguments{
  \item{path}{Path to the output saved from the
  ParticleLinker and the raw video directory}

  \item{temp.overlay.folder}{A temporary directory to save
  the overlay created with R}

  \item{overlay.folder}{A folder where the overlay videos
  are saved}

  \item{width}{The width of the raw video}

  \item{height}{The height of the raw video}

  \item{difference_lag}{Numeric value specifying the offset
  between two video frames to compute the difference image}

  \item{type}{A character string indicating the
  visualization type (i.e. 'label' or 'traj'): either the
  overlay is showing the trajectory ID and outlines the
  detected particle (type='label') or the whole trajectory
  remains plotted (type='traj').}

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

