---
layout: page
title: bemovi installation
description: bemovi installation description
---

# bemovi installation

### Installing package dependencies

To use bemovi several programs need to be installed beforehand:

1. ImageJ (http://imagej.nih.gov/ij/) and the LOCI Bio-formats plug-in (http://loci.wisc.edu/software/bio-formats)

2. ParticleLinker, which is a standalone version of the ParticleTracker 3D plug-in developed by the [MOSAIC group](http://mosaic.mpi-cbg.de/). This ParticleLinker is provided with the bemovi package and should be placed in a directory on the computer, where the user has administrator rights. (You may also get this application from the dropbox link given above, in the folder "Software", file "ParticleLinker.jar").
Please note that both ImageJ and the ParticleLinker rely on the Java environment, which therefore is required as well, but usually is pre-installed

3. [R - The statistical computing environment](http://www.r-project.org/)

4. the bemovi package itself, which can either be installed from github or via the source package

Note that it is probably best to only use the 64-bit version of ImageJ (and that you can ensure this by deleting the 32-bit version). You may also like to use Fiji, but please note that bemovi does not use Fiji.

To install bemovi from the source package file, adapt the path and run:


```r
#UNIX example
install.packages("/Users/Frank/Dropbox/bemovi demo data/Software/bemovi_1.0.tar.gz", repos=NULL,  type="source", quiet=T)

# Windows example
#install.packages("C:/Users/Frank/Dropbox/bemovi demo data/Software/bemovi_1.0.tar.gz", repos=NULL,  type="source", quiet=T)
```

Alternatively, to install bemovi from github, the devtools package is required before running the command:

```r
library(devtools)
install_github("pennekampster/bemovi")
```

```
## Downloading github repo pennekampster/bemovi@master
## Installing bemovi
## '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla CMD INSTALL  \
##   '/private/var/folders/42/0707t5k541sg3kwq2nsy6nzr0000gn/T/Rtmp3EzdVc/devtools1b1a1e765dd5/pennekampster-bemovi-59d1be4'  \
##   --library='/Library/Frameworks/R.framework/Versions/3.1/Resources/library'  \
##   --install-tests
```



