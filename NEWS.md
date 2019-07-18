bemovi 1.0.7 (2016-05-20)
=========================

### NEW FEATURES

* none

### BUG FIXES

* Important bug fixed regarding the re-scaling of the area from pixel to real dimensions: previously area was only scaled linearly, whereas after the fix, area is scaled by the the square of the pixel_to_scale factor. To correct the area variable in versions prior to 
1.0.7 please multiply the area by the conversion factor used.

### DEPRECATED AND DEFUNCT

* none