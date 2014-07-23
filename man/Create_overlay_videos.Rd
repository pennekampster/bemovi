\name{create_overlay_videos}
\alias{create_overlay_videos}
\title{Function to create an overlay between the raw data and the extracted trajectories}
\usage{
create_overlay_videos(to.data, trajectory.data.folder, raw.video.folder,
  temp.overlay.folder, overlay.folder, width, height, difference.lag,
  type = "traj", original.vid.contrast.enhancement = 0,
  memory = memory.alloc)
}
\arguments{
  \item{to.data}{path to the working directory}

  \item{trajectory.data.folder}{directory where the output
  of the ParticleLinker is saved}

  \item{raw.video.folder}{directory with the raw video
  files}

  \item{temp.overlay.folder}{temporary directory to save
  the overlay created with R}

  \item{overlay.folder}{directory where the overlay videos
  are saved}

  \item{width}{width of the raw video}

  \item{height}{height of the raw video}

  \item{difference_lag}{numeric value specifying the offset
  between two video frames to compute the difference image}

  \item{type}{string indicating the visualization type
  (i.e. 'label' or 'traj'): either the overlay is showing
  the trajectory ID and outlines the detected particle
  (type='label') or the whole trajectory remains plotted
  (type='traj').}

  \item{original.vid.contrast.enhancement}{numeric value to
  increase the contrast of the raw video}

  \item{memory}{numeric value specifying the amount of
  memory available to ImageJ}
}
\description{
A function to overlay the trajectories and the original
video using plots created in R and then processed in
ImageJ; two different visualization types are available
}

