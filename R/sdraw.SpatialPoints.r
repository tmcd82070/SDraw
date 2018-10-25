#' @rdname sdraw
#'  
#' @name sdraw
#' 
#' @method sdraw SpatialPoints
#'  
#' @aliases sdraw,SpatialPoints-method

sdraw.SpatialPoints <- function(x, n, type, ...){
  
  ans <- switch(type,
                HIP = hip.point( x, n, ...), 
                BAS = bas.point( x, n ), 
                GRTS = grts.point( x, n, ...),
                SSS = sss.point( x, n, ...),
                SRS = srs.point( x, n, ...),
                stop(paste("Invalid SpatialPoint sample type =", type))
  )
  
  ans
  
}
