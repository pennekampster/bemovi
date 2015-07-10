---
layout: page
title: Overview
description: Web site containing documentation for the bemovi R package
---

Bemovi is an R package that allows to extract abundance, behaviour and morphology of individual 
organisms from video sequences. The package relies on R - the statistical computing environment
and ImageJ, as well as the ParticleTracker plug-in developed for ImageJ. 

For a high level description of the package and its functions, as well as information to its application and validation see the following publication (or run citation("bemovi") in R): 

[Pennekamp, Frank, Nicolas Schtickzelle, and Owen L. Petchey. 2015. 
“Bemovi, Software for Extracting BEhaviour and MOrphology from VIdeos, illustrated with anlyses of microbes”, Ecology & Evolution, June 2015. DOI: 10.1002/ece3.1529](http://onlinelibrary.wiley.com/doi/10.1002/ece3.1529/abstract)

This web site provides accompanying information how to get started with bemovi, from
installing the necessary dependencies, conducting analyses and processing the data, to measuring morphological and behavioural traits and predict species identities based on these traits. 

Start by reading the [installation page](pages/installation.html), which
explains how to install the bemovi package as well as its dependencies. Then download the
example videos and  go through the [analysis](pages/analysis.html) and [processing demonstration](pages/processing.html). 

1. Setup
+ [Package and dependency installation](pages/installation.html)
+ [Technical considerations and compatibility](pages/tech_specs.html)
    
2. Worked example
+ [Example videos of microbes](pages/example.html)
+ [Video analysis](pages/analysis.html)
+ [Process data](pages/processing.html)
     
3. [Non-microbe example videos](pages/other_models.html)

4. [Bemovi related publications](pages/pubs.html)

5. [change log](pages/change_log.html)


  
If anything here or about the package is confusing (or _wrong_!), or if I've missed
important details, please [submit an issue](https://github.com/pennekampster/bemovi/issues), or (even
better) fork [the GitHub repository for this website](http://github.com/pennekampster/bemovi),
make modifications, and submit a pull request.