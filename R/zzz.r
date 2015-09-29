#' @import sp spsurvey graphics methods
#' 
#' @importFrom rgdal readOGR
#' @importFrom rgeos gArea

.onAttach<-function(libname, pkgname){

    v <- utils::packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Draws (vers ", v ,")", sep=""))  

    

}

setMethod("spsample", c(x="SpatialPolygons", n="ANY", type="ANY"), SDraw.SpatialPolygons)
setMethod("spsample", c(x="SpatialLines", n="ANY", type="ANY"), SDraw.SpatialLines)
setMethod("spsample", c(x="SpatialPoints", n="ANY", type="ANY"), SDraw.SpatialPoints)

