#' @rdname sdraw
#'  
#' @method sdraw SpatialLines
#'
#' @aliases sdraw,SpatialLines-method 

sdraw.SpatialLines <- function(x, n, type, ...){
  
  ans <- switch(type,
                HIP = hip.line( x, n, ...), 
                BAS = bas.line( x, n, ...), 
                SSS = sss.line( x, n, ...),
                SRS = srs.line( x, n, ...),
                GRTS = grts.line( x, n, ...), 
                stop( paste("Invalid SpatialLines sample type =", type))
  )
  
  ans
  
}
