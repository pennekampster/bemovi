#' Function to summarize the morphology, movement and their variability at the population (sample) level
#' adds population density estimates; does not care about different species
#' 
#' Takes the data comprising the information for each frame and calculates summary statistics such as mean and sd (for all morphology metrics) and mean, sd and min/max 
#' for some of the movement metrics along the trajectory. Values are rounded to the second decimal. 
#' @param data dataframe with the information on morphology and movement for each frame
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param write logical argument to indicate whether aggregated information should be saved to disk
#' @param give.fps Give frames per second (in object named fps) if it is not already defined globally
#' @return returns a data.table with the aggregated morphology and movement information for each trajectory
#' @export

summarize_populations <- function(data, write=FALSE, to.data, merged.data.folder){
  
  # checks whether frames per second are specified
  if(!exists("fps") ) stop("frames per second not specified (fps)")
  # checks whether the sample volume is specified
  if(!exists("measured_volume") ) stop("measured volume not specified (measured_volume)")
  
  # results object
  #pop_output <- 
  
  #output population summary data
  if (write==TRUE){save(pop_output, file = paste0(to.data, merged.data.folder,"Population_Data.RData"))}
  return(as.data.frame(pop_output))
}
