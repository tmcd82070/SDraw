#' @export primes
#' 
#' @title Prime numbers
#' 
#' @description Returns the first \code{n} prime numbers (starting at 2)
#' 
#' @param n Number prime numbers requested.  Maximum is 1e8 or 100,000,000.
#' 
#' @details This routine is brute-force and works well for the low primes, i.e., 
#' for \code{n} less than a couple thousand.  It is not particularly 
#' efficient for large \code{n}. 
#' 
#' @return A vector of length \code{n} containing prime numbers, in order, 
#' starting at 2.  
#' 
#' @author Trent McDonald
#' 
#' @examples 
#' primes(4) # result is c(2,3,5,7)    
#'
#' # Prime pairs in the first 100
#' p <- primes(100)
#' p.diff <- diff(p)
#' cbind(p[-length(p)][p.diff==2], p[-1][p.diff==2])

primes = function(n)
{

  isprime<- function(v){
    return(sapply(v,function(z){sum((z/1:z) %% 1)==2}))
  }
  
  # return the first n primes
  n = as.integer(n)
  if(n > 1e8) stop("Number of primes requested too large")
  ans <- rep(NA,n)
  ans[1] <- 2
  i <- 3
  while( any(is.na(ans))){
      if( isprime(i)){
          ans[min(which(is.na(ans)))] <- i
      }
      i <- i + 2
  }
  ans
}