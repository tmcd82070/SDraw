#' @export extended.gcd
#'
#' @title Extended Greatest Common Denominator (GCD) algorithm. 
#'  
#' @description Implements the extended Euclidean algorithm which 
#' computes the greatest common divisor and solves Bezout's identity.
#'  
#' @param a A vector of integers
#'  
#' @param b A vector of integers.  \code{length(a)} must equal \code{length(b)}.
#'  
#' @details This routine computes the element-wise gcd and 
#'  coefficients s and t such that a*t + b*s = d. In other words, if 
#'  \code{x = extended.gcd(a,b)}, then \code{x$a*x$t + x$b*x$s == x$gcd}
#'  
#'  The Wikipedia page, from which this algorithm was stolen, has the 
#'  following statement, 'The quotients of a and b by their greatest common divisor, 
#'  which are output, may have an incorrect sign.  this is easy to correct at the end
#'  of the computation, but has not been done here for simpligying the code. 
#'  I have absolutely no 
#'  idea what that means, but include it as a warning.  For purposes of 
#'  \code{SDraw}, elements of a and b are always positive, and I have never 
#'  observed "incorrect signs".  But, there may be some pathelogical cases 
#'  where "incorrect signs" occur, and the user should "correct" for this.  
#'  This routine does check that the output gcd is 
#'  positive, and corrects this and the signs of s and t if so.  
#'  
#' @return a data frame containing 5 columns; \code{a}, \code{t}, 
#'  \code{b}, \code{s}, and \code{gcd}.  
#'  Number of rows in output equals length of input \code{a}.  
#'
#' @references 
#'  Code is based on the following Wikipedia pseudo-code: 
#'  \url{https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm}
#'   
#' @author Trent McDonald
#'  
#' @examples 
#'  x <- extended.gcd( c(16,27,27,46), c(9,16,9,240) )
#'  
#'  #  Check
#'  cbind(x$a*x$t + x$b*x$s, x$gcd)
#'  
extended.gcd <- function( a, b ){
  
  # Check equality of lengths 
  if( length(a) != length(b) ) stop( "'a' and 'b' lengths differ")
  
  ext.gcd <- function(x){
    s <- c(0,1)
    t <- c(1,0)
    r <- x  # assume X is sorted already
    while( r[2] != 0 ){
      q <- floor(r[1]/r[2])
      r <- c(r[2], r[1] - r[2]*q)
      s <- c(s[2], s[1] - s[2]*q)
      t <- c(t[2], t[1] - t[2]*q) 
    }
    c(d=r[1], s=s[1], t=t[1])
  }
  
  

  ans <- apply( cbind(a,b), 1, ext.gcd )
  ans <- t(ans)
  ans <- data.frame( a=a, t=ans[,3], b=b, s=ans[,2], gcd=ans[,1]  )
  
  # Check for negative gcd
  if(any(ans$gcd < 0)){
    ind <- ans$gcd < 0
    ans$t[ind] <- -ans$t[ind]
    ans$s[ind] <- -ans$s[ind]
    ans$gcd[ind] <- -ans$gcd[ind]
  }
  
  ans
}


# # --------------
# 
# tmp <- extended.gcd(c(16,16,2^5),c(9,27,3^3))
# print( tmp )
# print( tmp$a*tmp$t + tmp$b*tmp$s )
# 
# 
# tmp <- extended.gcd( c(16,27,27,46), c(9,16,9,240) )
#  
# print(tmp)
#  #  Check
#  print(cbind(tmp$a*tmp$t + tmp$b*tmp$s, tmp$gcd))