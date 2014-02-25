setBatchMode(true);
// input user information;
avi_input = "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/";
avi_output = "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/";
lag = 25

list = getFileList(avi_input);


for (i=0; i<lengthOf(list); i++) {

if (endsWith(list[i],"avi")){
run("AVI...", "select=["+avi_input+list[i]+"] convert");
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
//selectWindow(vid2);
//close();
//selectWindow(vid3);
//close();
selectWindow(vid5);
close();
selectWindow(vid4);
setThreshold(15, 255);
run("Convert to Mask", "method=Default background=Default");
run("Set Measurements...", "area mean min centroid perimeter fit shape stack redirect=["+vid1+"] decimal=3");
run("Analyze Particles...", "size=20-2000 circularity=0.00-1.00 show=Nothing clear stack");
saveAs("Results", avi_output+replace(list[i],".avi","")+".ijout.txt");
selectWindow(vid1);
close();
close();
}


if (endsWith(list[i],"cxd")){
run("Bio-Formats", "open=["+avi_input+list[i]+"] autoscale color_mode=Default view=[Standard ImageJ] stack_order=Default");
run("8-bit");
getDimensions(width, height, channels, slices, frames);
slices=slices;
frames=frames;
width=width;
height=height;
channels=channels;
//vid1 = getTitle();
//run("Make Substack...", "  slices="+lag+"-"+frames+"");
//vid2 = getTitle();
//selectWindow(vid1);
//run("Make Substack...", "  slices=1-"+frames-(lag-1)+"");
//vid3 = getTitle();
//selectWindow(vid1);
//close();
//imageCalculator("Subtract create stack", vid3, vid2);

vid1 = getTitle();
run("Make Substack...", "  slices="+lag+"-"+slices+"");
vid2 = getTitle();
selectWindow(vid1);
run("Make Substack...", "  slices=1-"+(lag-1)+"");
vid3 = getTitle();
run("Concatenate...", "  image1=["+vid2+"] image2=["+vid3+"] image3=[-- None --]");
vid5 = getTitle();
//selectWindow(vid1);
//close();
imageCalculator("Subtract create stack", vid1, vid5);
vid4 = getTitle();

//selectWindow(vid2);
//close();
//selectWindow(vid3);
//close();
selectWindow(vid5);
close();

selectWindow(vid4);
setThreshold(5,255);
run("Convert to Mask", "method=Default background=Default");
run("Set Measurements...", "area mean min centroid perimeter fit shape stack redirect=["+vid1+"] decimal=3");
run("Analyze Particles...", "size=20-2000 circularity=0.00-1.00 show=Nothing clear stack");
saveAs("Results", avi_output+replace(list[i],".cxd","")+".ijout.txt");
selectWindow(vid1);
close();
close();
}
}

run("Quit");
