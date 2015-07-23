primes = function(n)
{

  isprime<- function(v){
    return(sapply(v,function(z){sum(z/1:z==z%/%1:z)})==2)
  }
  
  # return the first n primes
  n = as.integer(n)
  if(n > 1e8) stop("n too large")
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