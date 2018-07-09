###########################################
# bemovi 1.0.7_1

## Change of user interface
# added local storage of parameter so that they can be loaded from and saved to a yaml file. These values are used as default values (par_...()) in all functions. It is still possible to specify these values when calling a function, but **the default values are not changed!** 

# Added function to create the folder structure needed

## Internal Changes
* replaceed paste() and paste0() for creation of paths to file.path()


## Incompatible changes
* replaced load() and save() with readRDS() and writeRDS() for safety reasons (unintentionl overwriting of variables)
User have to use readRDS() instead of load() to read the results

* Use of Fiji for *Darwin*
Changed requirements - STILL TO BE COMPLETED

###########################################
# bemovi 1.0.7 (2016-05-20)
## NEW FEATURES

* none

## BUG FIXES

* Important bug fixed regarding the re-scaling of the area from pixel to real dimensions: previously area was only scaled linearly, whereas after the fix, area is scaled by the the square of the pixel_to_scale factor. To correct the area variable in versions prior to 
1.0.7 please multiply the area by the conversion factor used.

## DEPRECATED AND DEFUNCT

* none
###########################################
