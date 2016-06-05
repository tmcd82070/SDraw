#' @import sp spsurvey graphics methods
#' 
#' @importFrom rgdal readOGR
#' @importFrom rgeos gArea gUnion gIntersection
# @importFrom deldir deldir tile.list
#' @importFrom stats qnorm runif
#' @importFrom grDevices heat.colors rainbow

.onAttach<-function(libname, pkgname){

    v <- utils::packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Draws (vers ", v ,")", sep=""))  

    

}


