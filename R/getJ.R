#' @title getJ 
#' 
#' @description Compute J, the depth of the Halton lattice, 
#' given a population size . 
#' 
#' @param N Population or frame size (a scaler)
#' 
#' @param bases A vector of co-prime Halton bases of length D.
#' 
#' @details This routine returns a largest Halton cycle (i.e., B) 
#' such that n <= B <= N.  The first few Halton cycles are in 
#' Robertson et al. (2018) Web table 1. 
#' 
#' @return A vector of length 2 containing the exponents 
#' of bases that produce a Halton cycle of length B such 
#' that n <= B <= N. 
#' 
#' @author Trent McDonald
#' 
#' @examples 
#' 
#' getJ( 3, 36 )  # should equal c(2,2); B=36
#' getJ( 3, 37 )  # should equal c(3,2); B=72
#' 
#' @export
getJ <- function(N, bases=c(2,3)){
  
  D <- length(bases)   # number of dimensions
  
  if(D != 2) { 
    stop("HIP point currently implemented for 2-dimensional objects only.") 
  }
  
  #if(D != nrow(box)){
  #  stop("Number of dimensions in bounding box must equal number of bases.")
  #}
  
  if(N <= 62208 & all(bases == c(2,3))){
    # because I cannot figure out an algorithm, replicate web table 1.
    # These values were hand-picked to be reasonable.
    j1 <- c(1,2,3,2,3,4,3,5,4,5,6,5,7,6,5,7,8,7,8)
    j2 <- c(1,1,1,2,2,2,3,2,3,3,3,4,3,4,5,4,4,5,5)
    B  <- bases[1]^j1 * bases[2]^j2
    BlessN <- max(which( B <= N ))
    J <- cbind(j1,j2)[BlessN,]
  } else{
    # For bigger N, do something reasonable.  
  
    # Set default values of J so Halton boxes are as close to squares as possible
    n.boxes <- N^(1/D)
  
    # compute J which gives something close to N
    J <- round( log(n.boxes)/log(bases) )
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0

    # bump the lower number of boxes until B > N    
    while ( prod(bases^J) < N ) {
      if ( bases[1]^J[1] > bases[2]^J[2] ) {
        J[2] <- J[2] + 1
      } else {
        J[1] <- J[1] + 1
      }
    }
  }
  
  J
}
