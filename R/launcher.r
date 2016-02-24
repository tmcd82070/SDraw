#' Launch simulation
#' 
#' This is a wrapper to call the Shiny interface for simulations.
#' 
runUI <- function(){
  shiny::runApp(
  system.file('myshinyapp',                                                    
  package='SDraw')) 
}  
