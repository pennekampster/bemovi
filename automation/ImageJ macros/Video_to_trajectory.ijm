setBatchMode(true);

dir_input = "C:/tmp - video convert/avi - png/";
dir_output = "C:/tmp - video convert/diff_video - png/";

list = getFileList(dir_input);
for (k=0; k<list.length; k++) {

run("AVI...", "select=["+dir_input+list[k]+"] first=1 last=125 convert");
selectWindow(list[k]);
run("Make Substack...", "  slices=1-101");
selectWindow(list[k]);
run("Make Substack...", "  slices=25-125");
selectWindow(list[k]);
close();
imageCalculator("Subtract create stack", "Substack (1-101)","Substack (25-125)");
selectWindow("Result of Substack (1-101)");
setThreshold(10, 255);
run("Convert to Mask", "  black");
run("Median...", "radius=4 stack");

// Find Stack Maxima Macro is used in modified form
// Function applied to transform cells into their centroid position (1 pixel)
// Advantage of centroid is the easier definition of the size of target objects for the tracking algorithm 

  tolerance = 5;
  type = "Single Points";
  exclude = false;
  light = false;
  options = "";
  if (exclude) options = options + " exclude";
  if (light) options = options + " light";
  input = getImageID();
  n = nSlices();
  for (i=1; i<=n; i++) {
     showProgress(i, n);
     selectImage(input);
     setSlice(i);
     run("Find Maxima...", "noise="+ tolerance +" output=["+type+"]"+options);
     if (i==1)
        output = getImageID();
    else if (type!="Count") {
       run("Select All");
       run("Copy");
       close();
       selectImage(output);
       run("Add Slice");
       run("Paste");
    }
  }

selectWindow("Result of Substack (1-101)(1) Maxima");
run("AVI... ", "compression=JPEG frame=26 save=["+dir_output+replace(list[k],".cxd",".avi")+"]");
close();
close();
close();
close();

setBatchMode(false);

// re-open image sequence after saving
run("AVI...", "select=["+dir_output+replace(list[k],".cxd",".avi")+"] first=1 last=101 convert");
//run("Image Sequence...", "open=["+dir_output+replace(list[k],".cxd",".avi")+"] number=10 starting=0 increment=1 scale=100 file=[] or=[] sort");


run("Properties...", "channels=1 slices=1 frames=101 unit=pixel pixel_width=1.0000 pixel_height=1.0000 voxel_depth=1.0000 frame=[0 sec] origin=0,0");
run("Particle Tracker 2D/3D", "radius=1 cutoff=0 percentile=0.01 link=2 displacement=10");
close();
setBatchMode(true);
}

