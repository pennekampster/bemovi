// make sure ImageJ options are set (e.g. save row numbers)
run("Input/Output...", "jpeg=100 gif=-1 file=.txt copy_row save_column save_row");

setBatchMode(true);
// input user information;
video_input = "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/";
video_output = "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/";
lag = 25

list = getFileList(video_input);

for (i=0; i<lengthOf(list); i++) {

if (endsWith(list[i],"avi")){
run("AVI...", "select=["+video_input+list[i]+"] convert");
getDimensions(width, height, channels, slices, frames);
slices=slices;
frames=frames;
width=width;
height=height;
channels=channels;
run("Properties...", "channels=1 slices=1 frames="+slices+" unit=pixel pixel_width=1.0000 pixel_height=1.0000 voxel_depth=1.0000 frame=[0 sec] origin=0,0");
vid1 = getTitle();
run("Duplicate...", "title="+replace(list[i],".avi","_copy.avi")+" duplicate");
vid2 = getTitle();
selectWindow(vid1);
setThreshold(5,255);
run("Convert to Mask", "method=Default background=Default");
run("Set Measurements...", "area mean min centroid perimeter fit shape stack redirect=["+vid2+"] decimal=3");
run("Analyze Particles...", "size=5-500 circularity=0.00-1.00 show=Nothing clear stack");
saveAs("Results", video_output+replace(list[i],".avi","")+".ijout.txt");
selectWindow(vid1);
close();
close();
}


if (endsWith(list[i],"cxd")){
run("Bio-Formats", "open=["+video_input+list[i]+"] autoscale color_mode=Default view=[Standard ImageJ] stack_order=Default");
run("8-bit");
getDimensions(width, height, channels, slices, frames);
slices=slices;
frames=frames;
width=width;
height=height;
channels=channels;
run("Properties...", "channels=1 slices=1 frames="+frames+" unit=pixel pixel_width=1.0000 pixel_height=1.0000 voxel_depth=1.0000 frame=[0 sec] origin=0,0");
vid1 = getTitle();
run("Duplicate...", "title="+replace(list[i],".cxd","_copy.cxd")+" duplicate");
vid2 = getTitle();
selectWindow(vid1);
setThreshold(5,255);
run("Convert to Mask", "method=Default background=Default");
run("Set Measurements...", "area mean min centroid perimeter fit shape stack redirect=["+vid2+"] decimal=3");
run("Analyze Particles...", "size=5-500 circularity=0.00-1.00 show=Nothing clear stack");
saveAs("Results", video_output+replace(list[i],".cxd","")+".ijout.txt");
selectWindow(vid1);
close();
close();
}
}

run("Quit");
