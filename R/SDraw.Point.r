#' 
#'  @export SDraw.SpatialPoints
#'    
#'  @rdname spsample
#'
#'  
#'  @method spsample SpatialPoints
#'  



SDraw.SpatialPoints <- function(x, n, type, ...){
  
  note!! you are trying to figure out how to use setMethod with type="bas".  
  I think you need to setClass("BAS") and then setMethod(spsample, list(x=SpatialPoints, type="BAS"))
  may work. 
  
  ans <- switch(type,
                HAL = hal.point( n, x, ...), 
                BAS = bas.point( x, n ), 
                GRTS = grts.point( n, x, ...), 
                sp::spsample(x, n, type, ...)
  )
  
  ans
  
}
