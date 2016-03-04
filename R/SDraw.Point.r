#' 
#'  @export SDraw.SpatialPoints
#'    
#'  @rdname spsample
#'
#'  
#'  @method spsample SpatialPoints
#'  



SDraw.SpatialPoints <- function(x, n, type, ...){
  
  ans <- switch(type,
                HAL = hal.point( n, x, ...), 
                BAS = bas.point( n, x ), 
                GRTS = grts.point( n, x, ...), 
                sp::spsample(x, n, type, ...)
  )
  
  ans
  
}
