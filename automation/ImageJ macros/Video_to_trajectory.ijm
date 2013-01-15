setBatchMode(true);

dir_input = 'C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/';
dir_output = 'C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw tmp/';

list = getFileList(dir_input);
for (k=0; k<list.length; k++) {

run("AVI...", "select=["+dir_input+list[k]+"] first=1 last=125 convert");
original = getTitle();
run("Make Substack...", "  slices=1-101");
vid1 = getTitle();
selectWindow(original);
run("Make Substack...", "  slices=25-125");
vid2 = getTitle();
selectWindow(original);
close();
imageCalculator("Subtract create stack", vid1, vid2);
vid3 = getTitle();
selectWindow(vid3);
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
vid4 = getTitle();
  
selectWindow(vid4);
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
run("Particle Tracker 2D/3D", "radius=1 cutoff=0 percentile=0.01 link=5 displacement=20");
close();
setBatchMode(true);
}
exit();

