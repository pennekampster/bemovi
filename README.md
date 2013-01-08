franco
======

R and imageJ code for video-based counting and identification of aquatic
microbes, with methods to visualising and check the results.

There are three folders:

1. Automation

We want to be able to supply a collection of videos of aquatic
microbes, and get back the number of individuals of each species, and
also some of their traits (size, speed, etc.).

We use R to write and run macros for imageJ, to read in the data
imageJ produces, and then process this data.

We use imageJ to preporcess the videos, get morphological
characteristics of individuals, and to link particles (i.e., produce
particle trajectories through time). The aim is that all of this can
be done automatically, directed by R, but at present we can only do
the morphological characteristics automatically, since the particle
linker in imageJ is not working well in batch mode, at present.

Look at the file eg.worker.R for an example of functionality.

We need an agreed folder of fixed test videos that we can develop and test the
code on.



2. Morphological analysis

Example of what one can do with morphology, using an ANN to
discriminate species.



3. Merge morphology and trajectory
Example of merging morphological data acquired with the particle
analyser of imagej, and trajector data acquired with the MOSAIC
particle tracker.

Example of making an overlay, though its really basic, and needs much
more work. This will be quite important for verifying identifications.
