#' @import sp spsurvey
#' 
#' @importFrom rgdal readOGR
#' @importFrom rgeos gArea

.onAttach<-function(libname, pkgname){

    v <- utils::packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Draws (vers ", v ,")", sep=""))  

    

}

setMethod("spsample", c(x="SpatialPolygons", n="ANY", type="ANY"), SDraw.SpatialPolygons)
