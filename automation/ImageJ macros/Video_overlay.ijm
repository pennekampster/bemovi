setBatchMode(true);

avi_input = "C:/tmp - video convert/avi - png/";
overlay_input = "C:/tmp - video convert/selected overlay series/"
overlay_output = "C:/tmp - video convert/selected overlays/"
list = getFileList(avi_input);

for (i=30; i<63; i++) {

// Open stack with trajectories from SAS (incrementing positions through time)
run("Image Sequence...", "open=["+overlay_input+"overlay"+i+"/] number=101 starting=1 increment=1 scale=100 file=[] or=[] sort");
run("RGB Color");

// Open stack with picture sequence from which trajectories were extracted (original gray scle images)
run("AVI...", "select=["+avi_input+"Data"+i+".avi] first=1 last=101");
run("RGB Color");


// crop sequence of SAS plots, invert, resize and merge with original grayscale picture
selectWindow(overlay_input+"overlay"+i);
makeRectangle(70, 40, 1950, 1792);
run("Crop");
run("Invert", "stack");
run("Size...", "width=2048 height=2048 interpolation=Bicubic");

overlay = overlay_input+"overlay"+i;

// merge both stacks into
imageCalculator("Add create stack", "Data"+i+".avi", ""+overlay+"");
run("AVI... ", "compression=JPEG frame=26 save=["+overlay_output+"overlay"+i+".avi]");
close();
close();
close();
}}
