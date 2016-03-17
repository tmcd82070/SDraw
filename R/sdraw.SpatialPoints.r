
sdraw.SpatialPoints <- function(x, n, type, ...){
  
  ans <- switch(type,
                HAL = hal.point( x, n, ...), 
                BAS = bas.point( x, n ), 
                GRTS = grts.point( x, n, ...),
                SSS = sss.point( x, n, ...),
                SRS = srs.point( x, n, ...),
                stop(paste("Invalid SpatialPoint sample type =", type))
  )
  
  ans
  
}
