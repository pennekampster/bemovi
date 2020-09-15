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
vid1 = getTitle();
run("Make Substack...", "  slices="+lag+"-"+slices+"");
vid2 = getTitle();
selectWindow(vid1);
run("Make Substack...", "  slices="+slices-2*(lag-1)+"-"+slices-lag+"");
vid3 = getTitle();
run("Concatenate...", "  image1=["+vid2+"] image2=["+vid3+"] image3=[-- None --]");
vid5 = getTitle();
imageCalculator("Subtract create stack", vid1, vid5);
vid4 = getTitle();
selectWindow(vid5);
close();
selectWindow(vid4);
close(vid1);

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
vid1 = getTitle();
run("Make Substack...", "  slices="+lag+"-"+frames+"");
vid2 = getTitle();
selectWindow(vid1);
run("Make Substack...", "  slices="+frames-2*(lag-1)+"-"+frames-lag+"");
vid3 = getTitle();
run("Concatenate...", "  image1=["+vid2+"] image2=["+vid3+"] image3=[-- None --]");
vid5 = getTitle();
imageCalculator("Subtract create stack", vid1, vid5);
vid4 = getTitle();
//selectWindow(vid2);
//close();
//selectWindow(vid3);
//close();
selectWindow(vid5);
close();
selectWindow(vid4);
close(vid1); // execute the macro till this line

setBatchMode(false);
// play with the min and max threshold
setThreshold(10,255);
run("Threshold...");
}
