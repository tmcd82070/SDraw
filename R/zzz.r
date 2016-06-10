#' @import sp spsurvey graphics methods
#' 
# A list of functions needed from some imports
#' @importFrom rgeos gArea gUnion gIntersection
#' @importFrom deldir deldir tile.list
#' @importFrom stats qnorm runif
#' @importFrom grDevices rainbow

.onAttach<-function(libname, pkgname){

    v <- utils::packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Draws (vers ", v ,")", sep=""))  

    

}


