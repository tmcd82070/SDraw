\name{halton}
\alias{halton}

\title{
Compute points in the Halton sequence.
}
\description{
Computes points in a multi-dimensional Halton sequence, beginning at specified indices and using specified co-prime bases.
}
\usage{
halton(n, dim = 1, start = 0, bases = NULL)
}
\arguments{
  \item{n}{A scalar giving the number of values in the Halton points to produce. }
  \item{dim}{A scalar giving the number of dimensions, equal to the number of van der Corput sequences. Technically, \code{dim==1} produces a van der Corput sequence, \code{dim>=2} produces Halton sequences. }
  \item{start}{A scalar or a length \code{dim} vector giving the starting index (location) for  each van der Corput sequence. Origin of each sequence is 0. \code{all(start>=0)} must be true.}
  \item{bases}{A length \code{dim} vector giving the base to use for each dimension.  For a Halton sequence, bases must all be co-prime.  No check for common prime factors is performed.     If \code{bases} is \code{NULL}, the first \code{dim} primes starting at 2 are used as bases of the Halton sequence. 
    For example, the 4-dimensional Halton sequence would use bases 2, 3, 5, and 7. 
    The 6-dimensional Halton sequence would use 2, 3, 5, 7, 11, and 13. Etc. 
}
}
\details{
    The Halton sequence is a sequence of \code{dim}-dimensional numbers where 
    each dimension is a (1-dimensional) co-prime van der Corput sequence. Here, all 
    van der Corput sequences use bases that are prime numbers.  See references below. 
    
    
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
