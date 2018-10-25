#' @title halton.coefficients
#' 
#' @description Return the coefficients in the Halton equation 
#' for a list of Halton indicies (boxes).  
#' 
#' @param samp A vector of Halton indicies.
#' 
#' @param J A vector of powers of the bases. This determines the 
#' level of hierarchy in the Halton boxes and the number of boxes.
#' 
#' @param bases The bases of the Halton sequence.
#' 
#' @return An array of size \code{length(samp)} X \code{max(J)} X \code{length(J)} 
#' of coefficients.  Row i, column j, page k of this array is the jth coefficient
#' for the kth dimension of the ith index in \code{samp}.   
#' 
#' @details Let \code{digits = halton.coefficients(samp,J,bases)},
#'  \code{K = max(J)} and 
#' \code{places <- 1/matrix(rep(bases,each=K)^(1:K),K,length(J))}. 
#' The coordinate in [0,1) of the lower left corner of 
#' the Halton box with index \code{samp[i]} is 
#' \code{colSums(digits[i,,] * places, na.rm = T)}.
#' This is how you get the Halton sequence from this routine.
#' However, if you are interested in the Halton sequence alone,
#' not the coefficients, call function \code{halton()}.
#' 
#' @author Trent McDonald
#' 
#' @export

halton.coefficients <- function(samp, J, bases=c(2,3)){
  
  n <- length(samp)
  D <- length(J)
  K <- max(J)

  ans <- array( NA, c(n, K, D))
  for( j in 1:D){
    for( k in 0:(J[j]-1) ){
      ans[,k+1,j] <- (samp %/% bases[j]^k) %% bases[j]
    }
  }
  
  ans

}