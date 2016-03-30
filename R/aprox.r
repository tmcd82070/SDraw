# My approx function ===============================================
aprox <- function( x, y, x.out ){
  # use this for parameterized line functions
  y.out <- rep(NA, length(x.out))
  
  for( i in 1:length(x.out)){
    if( all(is.na(l.x <- which(x < x.out[i])))  ){
      y.out[i] <- y[1]
      next
    } 
    l.x <- max(l.x)
    
    if( all(is.na(u.x <- which(x > x.out[i])))  ){
      y.out[i] <- y[length(y)]
      next
    } 
    u.x <- min(u.x)
    
    if( any(e.x <- which(x == x.out[i]))){
      y.out[i] <- y[e.x[length(e.x)]]
      next
    }
    
    y.out[i] <- y[l.x] + (y[u.x]-y[l.x])*(x.out[i] - x[l.x])/(x[u.x]-x[l.x])
  }
  y.out
}
