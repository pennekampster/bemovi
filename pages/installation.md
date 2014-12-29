---
layout: page
title: Installing bemovi
description: Installing bemovi and its dependencies
---

Installing package dependencies
===============================

To use bemovi several programs need to be installed beforehand:

-   [ImageJ](http://imagej.nih.gov/ij/) and the [LOCI Bio-formats plug-in](http://loci.wisc.edu/software/bio-formats)

-   ParticleLinker, which is a standalone version of the
    ParticleTracker 3D plug-in developed by the [MOSAIC group](http://mosaic.mpi-cbg.de/). 
    This ParticleLinker is provided with
    the bemovi package and should be placed in a directory on the
    computer, where the user has administrator rights. (You may also get
    this application from the dropbox link given above, in the folder
    “Software”, file “ParticleLinker.jar”)

-   [R - The statistical computing environment](http://www.r-project.org/)

-   the bemovi package itself, which can either be installed from github
    or via the source package

Note that it is probably best to only use the 64-bit version of ImageJ
(and that you can ensure this by deleting the 32-bit version). You may
also like to use Fiji, but please note that bemovi does not use Fiji.




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
##   '/private/var/folders/42/0707t5k541sg3kwq2nsy6nzr0000gn/T/RtmpaZvy9O/devtools8967b80f227/pennekampster-bemovi-59d1be4'  \
##   --library='/Library/Frameworks/R.framework/Versions/3.1/Resources/library'  \
##   --install-tests
```

Then load the package:

```r
library(bemovi)
```

```
## Loading required package: data.table
## Loading required package: circular
## 
## Attaching package: 'circular'
## 
## The following objects are masked from 'package:stats':
## 
##     sd, var
## 
## Loading required package: CircStats
## Loading required package: MASS
## Loading required package: boot
## 
## Attaching package: 'CircStats'
## 
## The following objects are masked from 'package:circular':
## 
##     A1, A1inv, I.0, I.1, I.p, deg, plot.edf, pp.plot, rad,
##     rose.diag, rstable
```

Some messages show that the package dependencies, i.e. data.table, CircStats and circular were correctly loaded.
