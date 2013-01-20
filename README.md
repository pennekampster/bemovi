franco
======

R and imageJ code for video-based counting and identification of aquatic
microbes, with methods to visualising and check the results.

There are several folders:

==== Data ========
A folder containing several subfolders to store the sample description, the 
raw data, and folders which are created by the functions to store their 
respective results. The videos to be analyzed are stored in the subfolder 
"data/1 - raw/" which needs to be set up by the user. The folders which are
used by the functions are automatically created and the files transferred.

=== Automation ===
We want to be able to supply a collection of videos of aquatic
microbes, and get back the number of individuals of each species, and
also some of their traits (size, speed, etc.).
We use R to manipulate (change input folders etc.) and run pre-written 
ImageJ macros, store the information in the appropriate folders in "/data/"
and then to read in and process this data. The ImageJ macros are store in
the "automation/ImageJ macros" folder. We use ImageJ to preprocess the videos,
get morphological characteristics of individuals, and to link particles 
(i.e., produce particle trajectories through time). Moreover, an overlay
function merges the original video with the trajectories extracted by the
ParticleTracker plug-in for error-checking and visualization.
Both, extraction of morphology and particle tracking is automated, but the
present way of tracking maybe replaced.
To run the analysis, use the file eg.worker.R 

=== Merge morphology and trajectory ===
Example of merging morphological data acquired with the particle
analyser of ImageJ, and trajectory data acquired with the MOSAIC
particle tracker. (Obsolete if ParticleTracker can be run on coordinates from
ParticleAnalyzer; merging between morphology and tracking based on current 
approach not perfectly matching.)

=== Movement analysis ==========
A script which uses the coordinate data extracted from the videos via the
ParticleTracker and aggregates the data into movement metrics such as gross
and net displacement, average speed and linearity (net-to-gross-displacement.
Moreover, statistics of turning angle distribution and autocorrelation in 
turning angles distribution are calculated via the adehabitatLT package.

=== Morphological analysis ===
Example of what one can do with morphology, using an ANN to
discriminate species.