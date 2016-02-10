#'  @aliases spsample spsample.SpatialLines spsample.SpatialPoints spsample.SpatialPolygons
#'
#'  @export SDraw.SpatialLines
#'
#'  @name spsample
#'    
#'  @title Sample Draws in (or on) spatial objects.
#'  
#'  @rdname spsample
#'
#'  @method spsample SpatialLines
#'  



SDraw.SpatialLines <- function(x, n, type, ...){
  
  print("hi")
  ans <- switch(type,
                HAL = hal.line( n, x, ...), 
                BAS = bas.line( n, x ), 
                SSS = sss.line( n, x, ...),
                GRTS = grts.line( n, x, ...), 
                sp::spsample(x, n, type, ...)
  )
  
  ans
  
}
