// make sure ImageJ options are set (e.g. save row numbers)
run("Input/Output...", "jpeg=100 gif=-1 file=.txt copy_row save_column save_row");

setBatchMode(true);

// input user information;
video_input = '/Users/Frank/Documents/Postdoc/Temporary projects/test/1 - raw/';

// choose the video to work on, remember that the numbering starts at 0 in ImageJ
i = 0;
lag = 10;

list = getFileList(video_input);

if (endsWith(list[i],"avi")){
run("AVI...", "select=["+video_input+list[i]+"] convert");
run("8-bit");
getDimensions(width, height, channels, slices, frames);
slices=slices;
frames=frames;
width=width;
height=height;
channels=channels;
run("Properties...", "channels=1 slices=1 frames="+slices+" unit=pixel pixel_width=1.0000 pixel_height=1.0000 voxel_depth=1.0000 frame=[0 sec] origin=0,0");

setBatchMode(false);
// play with the min and max threshold
setThreshold(10,255);
run("Threshold...");
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

// execute the macro till this line

setBatchMode(false);
// play with the min and max threshold
setThreshold(10,255);
run("Threshold...");
}
