setBatchMode(true);

avi_input = "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/";
overlay_input = "C:/Users/Frank/Documents/PhD/Programming/franco/data/3 - overlay plots/";
overlay_output = "C:/Users/Frank/Documents/PhD/Programming/franco/data/4 - overlays/";
lag = 25
list = getFileList(avi_input);

for (i=0; i<lengthOf(list); i++) {
if (endsWith(list[i],"avi")){
// Open stack with trajectories from folder (incrementing positions through time)
run("Image Sequence...", "open=["+overlay_input+replace(list[i],".avi","")+"] sort");
getDimensions(width, height, channels, slices, frames);
run("RGB Color");
vid1 = getTitle();

// Open stack with picture sequence from which trajectories were extracted (original gray scle images)
run("AVI...", "select=["+avi_input+list[i]+"] first=1 last="+slices+"");
run("RGB Color");
run("Invert", "stack");
vid2 = getTitle();

// merge both stacks into
imageCalculator("AND create stack", vid2, vid1);
run("Invert", "stack");
run("AVI... ", "compression=JPEG frame=26 save=["+overlay_output+list[i]+"]");
close();
close();
close();
}}
run("Quit");