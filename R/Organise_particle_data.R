#' Function to merge the morphology and data on X- and Y-coordinates into one file for further processing
#' 
#' This function merges the files containing morphology and coordinates (one for each video) into large dataset,
#' and saves it to the directory where the single files are located
#' @param to.data path to the working directory
#' @param particle.data.folder directory to which the data is saved as a text file
#' @param pixel_to_scale TODO
#' 
#' @export

organise_particle_data <- function(
  to.data = par_to.data(), 
  particle.data.folder = par_particle.data.folder(),
  pixel_to_scale = par_pixel_to_scale()
) {
  
  #pixel_to_scale<-NULL
  
  IJ_output.dir <- file.path(to.data, particle.data.folder)
  
  ## the macro file names
  all.files <- dir(path = IJ_output.dir, pattern = "ijout", full.names = TRUE)
  ijout.files <- all.files[grep("ijout", all.files)]
  mylist <- lapply(ijout.files, fread, header = TRUE)
  mylist <- mylist[lapply(mylist,length) > 0]
  dd <- rbindlist(mylist)
  dd$file <- gsub(".ijout.txt", "", rep(dir(path = IJ_output.dir, pattern = "ijout"), lapply(mylist, nrow)))
  ## change column names because R is replacing missing header with X causing confusion with real X and Y positions
  old_names <- colnames(dd) 
  new_names <- c("obs", "Area", "Mean", "Min", "Max", "X", "Y", "Perimeter", "Major", "Minor", "Angle", "Circ.", "Slice", 
                 "AR", "Round", "Solidity", "file")
  setnames(dd,old_names,new_names)
  
  morphology.data <- as.data.frame(dd)
  
  # convert morphology to real dimensions
  morphology.data$Area <-   morphology.data$Area*pixel_to_scale^2
  morphology.data$Perimeter <- morphology.data$Perimeter*pixel_to_scale
  morphology.data$Major <- morphology.data$Major*pixel_to_scale
  morphology.data$Minor <-  morphology.data$Minor*pixel_to_scale
  
  saveRDS(morphology.data, file = file.path(IJ_output.dir, "particle.rds"))
} 

