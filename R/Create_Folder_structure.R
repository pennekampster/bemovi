#' Create folder structure based on parameter
#'
#' @param to.data path to the working directory
#' @param video.description.folder directory containing the video description file
#' @param raw.video.folder directory with the raw video files 
#' @param particle.data.folder directory to which the data is saved as a text file
#' @param trajectory.data.folder directory containing the global trajectory data
#' @param temp.overlay.folder temporary directory to save the overlay created with R
#' @param overlay.folder directory where the overlay videos are saved
#' @param merged.data.folder directory where the global database is saved
#' @param ijmacs.folder directory for the macro to for ImageJ 
#'
#' @return invisibly \code{TRUE}
#' @export
#'
#' @examples
Create_folder_structure <- function(
  to.data = par_to.data(), 
  video.description.folder = par_video.description.folder(),
  raw.video.folder = par_raw.video.folder(), 
  particle.data.folder = par_particle.data.folder(), 
  trajectory.data.folder = par_trajectory.data.folder(),
  temp.overlay.folder = par_temp.overlay.folder(),
  overlay.folder = par_overlay.folder(),
  merged.data.folder = par_merged.data.folder(),
  ijmacs.folder = par_ijmacs.folder()
) {
  dir.create( to.data )
  dir.create( file.path( to.data, video.description.folder ), showWarnings = FALSE )
  dir.create( file.path( to.data, raw.video.folder ), showWarnings = FALSE )
  dir.create( file.path( to.data, particle.data.folder ), showWarnings = FALSE )
  dir.create( file.path( to.data, trajectory.data.folder), showWarnings = FALSE )
  dir.create( file.path( to.data, temp.overlay.folder ), showWarnings = FALSE )
  dir.create( file.path( to.data, overlay.folder ), showWarnings = FALSE )
  dir.create( file.path( to.data, merged.data.folder ), showWarnings = FALSE )
  dir.create( file.path( to.data, ijmacs.folder ), showWarnings = FALSE )
  invisible(TRUE)
}
  