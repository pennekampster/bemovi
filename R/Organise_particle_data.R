#' Function to merge the morphology and data on X- and Y-coordinates into one file for further processing
#' 
#' This function merges the files containing morphology and coordinates (one for each video) into large dataset,
#' and saves it to the directory where the single files are located
#' @param path Path to the text files containing the morphology and coordinate data
#' @return NULL
#' @export
Organise_particle_data <- function(to.data, particle.data.folder) {
  
  IJ_output.dir <- paste(to.data, particle.data.folder, sep = "")
  
  ## the macro file names
  all.files <- dir(path = IJ_output.dir)
  ijout.files <- all.files[grep("ijout", all.files)]
  
  dd <- read.delim(paste(IJ_output.dir, ijout.files[1], sep = "//"))
  dd$file <- rep(gsub(".ijout.txt", "", ijout.files[1]), length(dd[, 1]))
  
  ## change column names because R is replacing missing header with X causing confusion with real X and Y positions
  colnames(dd) <- c("obs", "Area", "Mean", "Min", "Max", "X", "Y", "Perimeter", "Major", "Minor", "Angle", "Circ.", "Slice", 
                    "AR", "Round", "Solidity", "file")
  
  if (length(ijout.files) > 2) {
    for (i in 2:length(ijout.files)) {
      dd.t <- read.delim(paste(IJ_output.dir, ijout.files[i], sep = "//"))
      dd.t$file <- rep(gsub(".ijout.txt", "", ijout.files[i]), length(dd.t[, 1]))
      ## change column names because R is replacing missing header with X causing confusion with real X and Y positions
      colnames(dd.t) <- c("obs", "Area", "Mean", "Min", "Max", "X", "Y", "Perimeter", "Major", "Minor", "Angle", 
                          "Circ.", "Slice", "AR", "Round", "Solidity", "file")
      dd <- rbind(dd, dd.t)
    }
  }
  
  assign("particle.data", dd, envir = .GlobalEnv)
  write.table(particle.data, file = paste(IJ_output.dir, "particle.data.txt", sep = "/"), sep = "\t")
} 
