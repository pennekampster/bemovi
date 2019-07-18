###########################################
# bemovi 1.0.7_1

## Internal Changes
* replaceed paste() and paste0() for creation of paths to file.path()

# Incompatible changes
* replaced load() and save() with readRDS() and writeRDS() for safety reasons (unintentionl overwriting of variables)
User have to use readRDS() instead of load() to read the results

* Use of Fiji for *Darwin*
Changed requirements - in flux

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
