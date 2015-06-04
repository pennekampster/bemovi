#' Multicore version of check_corrupted function which uses the  showinf tool from the bftools suite (provided by BIO-LOCI)
#' 
#' The function will output the names of uncorruped and corrupted files, which would make BEMOVI bug; the files get
#' transferred into a "bad files" directory in the raw.video.folder and should be either repaired or ignored for 
#' further analysis.
#' 
#' @param to.data path to the working directory 
#' @param raw.video.folder directory with the raw video files 
#' @param show_inf path to the showinf executable
#' @import parallel
#' @export

check_corrupted_mc <- function(to.data, raw.data.folder, path_showinf){
 
 # Check showinf.exe exists in specified folder
 if(file.exists(paste0(path_showinf,"showinf"))==F) stop("bftools software not found, perhaps the file path is wrong")
 
 # test the file path to the raw data is correct
 if(is.na(file.info(paste0(to.data, raw.video.folder))[1,"isdir"])) stop("the file paths provided for the raw data is wrong")
 
 # Create output directory for corrupted files
 path_bad_files <- paste0(to.data, raw.video.folder , "bad files/")
 dir.create(path_bad_files, showWarnings=FALSE)
 
 # Important test to ensure the bad files folder is made
 if(is.na(file.info(path_bad_files)[1,"isdir"])) stop("the file paths provided for bad data file do not exist")
 
 # find names of all .cdx files in raw data directory 
 files <- list.files(paste0(to.data, raw.video.folder))
 files <- files[grepl(".cxd", files)]
 
 #the makeCluster function is used to set up clusters
 #the clusterExport function is used to transfer objects from function environments to cluster environments
 #the clusterApplyLB function is used to perform function in parallel;its syntax is similar to other apply loops but browser cant be used and objects must be specially supplied
 #LB stands for load balancing : useful for when jobs are of differing sizes
 
 # detect number of cores to use
 n.cores<-detectCores(all.tests = FALSE, logical = TRUE)
 
 # set up cluster
 #system("defaults write org.R-project.R force.LANG en_US.UTF-8")
 cl <- makeCluster(n.cores)
 # which objects do all clusters need to know???
 clusterExport(cl, varlist=c("to.data", "raw.video.folder",  "path_showinf", "path_bad_files", "files"),environment())
 
 # test if image data can be retrieve for each file
  clusterApplyLB(cl,1:length(files), fun=function(i){
  	#mclapply( 1:length(files), FUN=function(i){
  
    
    if (.Platform$OS.type == "unix") {
      # Run showinf via console and print info into text file named check_file.txt
      system(paste0(path_showinf, "showinf -nopix ", "'", to.data, raw.video.folder  ,"'", files[i], " > " ,
                    "'", to.data, raw.video.folder , "check_",files[i],".txt","'")) }
    
    if (.Platform$OS.type == "windows") {
      # Run showinf via console and print info into text file named check_file.txt
      sink(paste0(to.data, raw.video.folder , "check_",files[i],".txt"))
      system(paste0(path_showinf, "showinf.bat -nopix ", '"', to.data, raw.video.folder, files[i], '"'))
      sink()
    }
   
   #read file and check it is not just a short error message
   check <- readLines(paste0(to.data, raw.video.folder , "check_",files[i],".txt")) 
   # If read-out is short, there is a problem, therefore copy this file to the bad file directory
   if (length(check) < 4){
        file.rename(from= paste0(to.data, raw.video.folder, files[i]), to= paste0(path_bad_files, files[i]))
    } else{ 
    	cat(paste0("File ",files[i]," is not corrupted"))
   }
   # delete check_file
   unlink(paste0(to.data, raw.video.folder , "check_",files[i],".txt"))
  })

 #output result
 if (length(list.files(path_bad_files)) > 0){cat(paste0("The following corrupted file(s) were found: ", "\n", list.files(path_bad_files)))}  else {cat("No corrupted files found!")}
 
 # stop cluster at end
 stopCluster(cl)
}
