#' @import sp spsurvey graphics methods
#' 
#' @importFrom rgdal readOGR
#' @importFrom rgeos gArea

.onAttach<-function(libname, pkgname){

    v <- utils::packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Draws (vers ", v ,")", sep=""))  

    

}


