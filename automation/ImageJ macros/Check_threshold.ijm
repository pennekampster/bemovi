setBatchMode(false);

// input user information;
avi_input = '/Users/owenpetchey/Desktop/experiment2/videocounts/video5/1 - raw/';
avi_output = '/Users/owenpetchey/Desktop/hard.test/1 - raw checkthresh/';
lag = 10;

list = getFileList(avi_input);


// for (i=0; i<lengthOf(list); i++) {

// choose the video to work on, remember that the numbering starts at 0 in imagej
i=0;


if (endsWith(list[i],"avi")){
run("AVI...", "select=["+avi_input+list[i]+"] convert");
getDimensions(width, height, channels, slices, frames);
run("Properties...", "channels=1 slices=1 frames="+slices+" unit=pixel pixel_width=1.0000 pixel_height=1.0000 voxel_depth=1.0000 frame=[0 sec] origin=0,0");
vid1 = getTitle();
run("Make Substack...", "  slices="+lag+"-"+slices+"");
vid2 = getTitle();
selectWindow(vid1);
run("Make Substack...", "  slices=1-"+slices-(lag-1)+"");
vid3 = getTitle();
//selectWindow(vid1);
//close();
imageCalculator("Subtract create stack", vid2, vid3);
vid4 = getTitle();
selectWindow(vid2);
close();
selectWindow(vid3);
close();
selectWindow(vid4);
setThreshold(2000,50000);
run("Convert to Mask", "method=Default background=Default");
//run("AVI... ", "compression=JPEG frame=26 save=["+avi_output+list[i]+"]");


//selectWindow(vid1);
//close();
//close();
}


if (endsWith(list[i],"cxd")){
run("Bio-Formats", "open=["+avi_input+list[i]+"] autoscale color_mode=Default view=[Standard ImageJ] stack_order=Default");
getDimensions(width, height, channels, slices, frames);
vid1 = getTitle();
run("Make Substack...", "  slices="+lag+"-"+frames+"");
vid2 = getTitle();
selectWindow(vid1);
run("Make Substack...", "  slices=1-"+frames-(lag-1)+"");
vid3 = getTitle();
//selectWindow(vid1);
//close();
imageCalculator("Subtract create stack", vid2, vid3);
vid4 = getTitle();
selectWindow(vid2);
close();
selectWindow(vid3);
close();
selectWindow(vid4);
setThreshold(2000,50000);
run("Convert to Mask", "method=Default background=Default");
//run("AVI... ", "compression=JPEG frame=26 save=["+avi_output+replace(list[i],".cxd",".avi")+"]");



//selectWindow(vid1);
//close();
//close();
}


//run("Quit");
