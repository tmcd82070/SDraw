#' @rdname sdraw
#'  
#' @method sdraw SpatialPolygons 
#'  
#' @aliases sdraw,SpatialPolygons-method 



sdraw.SpatialPolygons <- function(x, n, type, ...){
  
  ans <- switch(type,
                HIP = hip.polygon( x, n, ...), 
                BAS = bas.polygon( x, n ), 
                SSS = sss.polygon( x, n, ...),
                SRS = srs.polygon( x, n, ...),
                GRTS = grts.polygon( x, n, ...), 
                stop(paste("Invalid SpatialPolygons sample type =", type))
  )
  
  ans
  
}

# This line overwrites the method in sp.
#setMethod("spsample", c(x="SpatialPolygons", n="ANY", type="ANY"), SDraw.SpatialPolygons)
