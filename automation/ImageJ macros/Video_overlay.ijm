setBatchMode(true);

avi_input = "C:/Users/Frank/Documents/PhD/Programming/franco/videos/raw/";
overlay_input = "C:/Users/Frank/Documents/PhD/Programming/franco/videos/";
overlay_output = "C:/Users/Frank/Documents/PhD/Programming/franco/videos/overlays/";
list = getFileList(avi_input);

for (i=0; i<lengthOf(list); i++) {

// Open stack with trajectories from folder (incrementing positions through time)
run("Image Sequence...", "open=["+overlay_input+replace(list[i],".avi","")+"] number=101 starting=1 increment=1 scale=100 file=[] or=[] sort");
run("RGB Color");
vid1 = getTitle();

// Open stack with picture sequence from which trajectories were extracted (original gray scle images)
run("AVI...", "select=["+avi_input+list[i]+"] first=1 last=101");
run("RGB Color");
run("Invert", "stack");
vid2 = getTitle();

//overlay = overlay_input+"overlay"+i;

// merge both stacks into
imageCalculator("AND create stack", vid2, vid1);
run("Invert", "stack");
run("AVI... ", "compression=JPEG frame=26 save=["+overlay_output+list[i]+"]");
close();
close();
close();
}run("Quit");