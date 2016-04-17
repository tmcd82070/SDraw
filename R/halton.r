#' @export halton
#' 
#' @title Compute points in the Halton sequence.
#' 
#' @description  Computes points in a multi-dimensional Halton sequence, beginning at
#' specified indices and using specified co-prime bases.
#' 
#' @details The Halton sequence is a sequence of \code{dim}-dimensional numbers where
#' each dimension is a (1-dimensional) co-prime van der Corput sequence. Here,
#' all van der Corput sequences use bases that are prime numbers.  See
#' references below.
#' 
#' @param n A scalar giving the number of values in the Halton points to
#' produce.
#' @param dim A scalar giving the number of dimensions, equal to the number of
#' van der Corput sequences. Technically, \code{dim==1} produces a van der
#' Corput sequence, \code{dim>=2} produces Halton sequences.
#' @param start A scalar or a length \code{dim} vector giving the starting
#' index (location) for each van der Corput sequence. Origin of each sequence
#' is 0. \code{all(start>=0)} must be true.
#' @param bases A length \code{dim} vector giving the base to use for each
#' dimension.  For a Halton sequence, bases must all be co-prime.  No check for
#' common prime factors is performed.  If \code{bases} is \code{NULL}, the
#' first \code{dim} primes starting at 2 are used as bases of the Halton
#' sequence.  For example, the 4-dimensional Halton sequence would use bases 2,
#' 3, 5, and 7.  The 6-dimensional Halton sequence would use 2, 3, 5, 7, 11,
#' and 13. Etc.
#' @return A matrix of size \code{n} X \code{dim}.  Each column corresponds to
#' a dimension.  Each row is a \code{dim}-dimenisional Halton point.
#' 
#' @author Trent McDonald
#' 
#' @seealso \code{\link{halton.indicies}}
#' 
#' @references van der Corput sequences are described here:
#' \url{http://en.wikipedia.org/wiki/Van_der_Corput_sequence}
#' 
#' Halton sequences are described here:
#' \url{http://en.wikipedia.org/wiki/Halton_sequence}
#' 
#' Robertson, B.L., J. A. Brown, T. L. McDonald, and P. Jaksons (2013) BAS:
#' "Balanced Acceptance Sampling of Natural Resources", Biometrics, v69, p.
#' 776-784.
#' @keywords design survey
#' @examples
#' 
#' halton(10,2)
#' halton(10,2, floor(runif(2,max=100000))) # A random-start 2-D Halton sequence of length 10
#' 
halton <- function( n, dim=1, start=0, bases=NULL ){

#   Get the first so many primes
#primes <- function(v){
#        return(v[sapply(v,function(z){sum(z/1:z==z%/%1:z)==2})])
#}
# the first 100 primes.  I got these by calling the above function i.e. primes(1:545). 

  first.primes <- c(  
              2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,  37,  41,  43,  47,  53,  59,  61,
             67,  71,  73,  79,  83,  89,  97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 
            157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 
            257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 
            367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 
            467, 479, 487, 491, 499, 503, 509, 521, 523, 541)
if( dim > length(first.primes) ){
    first.primes <- primes(dim)  # will get up to first 1e8 primes
}

if( length(start) == 1 ){
    start <- rep(start, dim)
}

if( length(start) != dim ){
    stop( "The start vector must either have length 1 or length equal to the number of dimensions") 
}


#    Bases of the first dim dimensions of the Halton sequence
if( is.null(bases)){
  bases <- first.primes[ 1:dim ]  
}


#   The locations of the numbers we want Halton numbers for.  First Halton number is 1, second is 2, etc.  This is the
#   position in the Halton sequence we need.  After this, pos is a dim X n matrix where columns are the Halton numbers 
#   we want. 
pos <- t(sapply(start, FUN=function(x,k){ x:(x+k-1) }, k=n ))



#   Find halton sequence numbers using the finite sum formula Blair cooked up
n.sum.terms <- max(floor( log(start + n - 1)/ log(bases) ) + 1)  # number of digits in base b needed to represent maximum pos in each dim

ans <- matrix( 0, nrow(pos), ncol(pos) )
for( j in 0:n.sum.terms ){
    ans <- ans + ((pos %/% bases^j) %% bases) / bases^(j+1)
}

t(ans)

}   
    
    
