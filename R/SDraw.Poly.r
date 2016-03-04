#' 
#'  @export SDraw.SpatialPolygons
#'    
#'  @rdname spsample
#'  
#'  @method spsample SpatialPolygons 
#'  



SDraw.SpatialPolygons <- function(x, n, type, ...){
  
  ans <- switch(type,
    HAL = hal.polygon( n, x, ...), 
    BAS = bas.polygon( n, x ), 
    SSS = sss.polygon( n, x, ...),
    GRTS = grts.polygon( n, x, ...), 
    sp::spsample(x, n, type, ...)
  )
  
  ans
  
}

# This line overwrites the method in sp.
#setMethod("spsample", c(x="SpatialPolygons", n="ANY", type="ANY"), SDraw.SpatialPolygons)
