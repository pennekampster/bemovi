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
slices=slices;
frames=frames;
width=width;
height=height;
channels=channels;
run("RGB Color");
vid1 = getTitle();

// Open stack with picture sequence from which trajectories were extracted (original gray scale images)
run("AVI...", "select=["+avi_input+list[i]+"] first=1 last="+slices+"");
run("Enhance Contrast...", "saturated=0.4 process_all");
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
}


if (endsWith(list[i],"cxd")){
// Open stack with trajectories from folder (incrementing positions through time)
run("Image Sequence...", "open=["+overlay_input+replace(list[i],".cxd","")+"] sort");
getDimensions(width, height, channels, slices, frames);
slices=slices;
frames=frames;
width=width;
height=height;
channels=channels;
run("RGB Color");
vid1 = getTitle();

// Open stack with picture sequence from which trajectories were extracted (original gray scle images)
run("Bio-Formats", "open=["+avi_input+list[i]+"] autoscale color_mode=Default view=[Standard ImageJ] stack_order=Default");
run("Enhance Contrast...", "saturated=0.4 process_all");
vidtemp = getTitle();
run("Make Substack...", "  slices="+lag+"-125");
vid2 = getTitle();
selectWindow(vidtemp);
close();
selectWindow(vid2);
run("RGB Color");
run("Invert", "stack");

// merge both stacks into
imageCalculator("AND create stack", vid2, vid1);
run("Invert", "stack");
run("AVI... ", "compression=JPEG frame=26 save=["+overlay_output+replace(list[i],".cxd",".avi")+"]");
close();
close();
close();
}




}
run("Quit");
