#' @title Launch SDraw Shiny interface
#' 
#' @description Launches the Shiny interface to SDraw which allows samples to 
#'  be drawn using a graphical user interface (UI) 
#' 
#' @details Used for side effects.  A HTML viewer is launched. Control and 
#' use of the UI should be self-explanatory.
#' 
#'
#'
#' @return None. 
#'
#' @examples 
#' \dontrun{runUI()}
#'
#' @export runUI
#'  
runUI <- function(){
  shiny::runApp(
  system.file('myshinyapp',                                                    
  package='SDraw')) 
}  
