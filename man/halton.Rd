\name{halton}
\alias{halton}

\title{
Compute multi-dimensional Halton sequence.
}
\description{
Computes \code{dim} co-prime van der Corput sequences, each with specified length, beginning at specified indices.
}
\usage{
halton(n, dim = 1, start = 1)
}
\arguments{
  \item{n}{(scalar) the number of values in each Halton sequence. }
  \item{dim}{(scalar) the number of dimensions, equal to the number of van der Corput sequences.}
  \item{start}{(scalar or \code{dim}-vector) starting index (location) of each van der Corput sequence.}
}
\details{
    The Halton sequence is a sequence of \code{dim}-dimensional numbers where 
    each dimension is a (1-dimensional) co-prime van der Corput sequence. Here, all 
    van der Corput sequences use bases that are prime numbers.  See references below. 
    
    The first \code{dim} primes, starting at 2, are used for the bases of the Halton sequence. 
    For example, the 4-dimensional Halton sequence uses bases 2, 3, 5, and 7. 
    The 6-dimensional Halton sequence uses bases 2, 3, 5, 7, 11, and 13. 
    
}

\value{
A matrix of size \code{n} X \code{dim}.  Each column corresponds to a dimension.  
Each row is a \code{dim}-dimenisional Halton point. }

\references{
    van der Corput sequences are described here: \url{http://en.wikipedia.org/wiki/Van_der_Corput_sequence} 
    
    Halton sequences are described here: \url{http://en.wikipedia.org/wiki/Halton_sequence}
    
    Robertson, B.L., J. A. Brown,  T. L. McDonald, and P. Jaksons (2013) BAS: "Balanced Acceptance Sampling of Natural Resources", Biometrics, v69, p. 776-784.
    }
\author{
Trent McDonald
}


\seealso{
 \code{\link{bas}}
}

\examples{
halton(10,2)
halton(10,2, floor(runif(2,max=100000))) # A random-start 2-D Halton sequence of length 10
}

\keyword{ design }
\keyword{ survey }
