





hip.part.along.d <- function(d, J, pnts, box, samp, bases){
  
  if( nrow(pnts) <= 1 | J[d] <= 0){
    return(box)
  }
  
  if( (nrow(pnts) %% bases[d]) != 0){
      # Drop rows at random so boundaries are between points 
      pnts <- pnts[-ceiling(runif(nrow(pnts) %% bases[d],0,nrow(pnts))),]
  }
  
  bounds <- quantile(pnts[,d], seq(1,bases[d]-1)/bases[d])  
  bounds <- c(box[d,1], bounds, box[d,2])
  
  #here!!! make this recursive
  
  for( j in 1:(length(bounds)-1)){
    ind1 <- (bounds[j] <= pnts[,d]) & (pnts[,d] < bounds[j+1])
    lowerBox <- box
    lowerBox[d,] <- c(bounds[j], bounds[j+1])
    lowerJ <- J - diag(D)[d,] 
    lowerBoxes <- hip.partition(pnts[ind1,], lowerBox, sample, lowerJ, bases)
    
  }
} 