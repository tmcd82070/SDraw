halton <- function( n, dim=1, start=1 ){
#
#   compute a Halton sequence of n numbers starting at position start.
#
#   n = number of values desired 
#   dim = number of dimensions
#   start = vector of starting positions for each dimension.
#


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
    stop(paste("A maximum of", length(first.primes), "dimensions can be requested."))
}

if( length(start) == 1 ){
    start <- rep(start, dim)
}

if( length(start) != dim ){
    stop( "The start vector must either have length 1 or length equal to the number of dimensions") 
}


#    Bases of the first dim dimensions of the Halton sequence
bases <- first.primes[ 1:dim ]


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
    
    
