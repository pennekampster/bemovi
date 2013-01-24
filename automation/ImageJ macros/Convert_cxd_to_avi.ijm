setBatchMode(true);

dir_input = "C:/Video analysis for species recognition/Raw data/";
dir_output = "C:/Video analysis for species recognition/Avi/";

list = getFileList(dir_input);

for (i=0; i<list.length; i++) {


run("Bio-Formats (Windowless)", "open=["+dir_input+list[i]+"]");
run("AVI... ", "compression=JPEG frame=26 save=["+dir_output+replace(list[i],".cxd",".avi")+"]");
close();
}

