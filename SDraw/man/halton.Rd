\name{halton}
\alias{halton}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute a number in the Halton sequence.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
halton(n, dim = 1, start = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{dim}{
%%     ~~Describe \code{dim} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n, dim = 1, start = 1) 
{
    first.primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 
        37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 
        101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 
        157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 
        223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 
        277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 
        349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 
        419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 
        479, 487, 491, 499, 503, 509, 521, 523, 541)
    if (dim > length(first.primes)) {
        stop(paste("A maximum of", length(first.primes), "can be requested."))
    }
    bases <- first.primes[1:dim]
    pos <- start:(start + n - 1)
    ans <- matrix(NA, length(pos), dim)
    for (j in 1:dim) {
        xb <- dec2baseb(pos, b = bases[j])
        if (ncol(xb) > 1) {
            xb <- t(apply(xb, 1, rev))
        }
        denom <- bases[j]^ncol(xb)
        xb <- baseb2dec(xb, b = bases[j])
        ans[, j] <- xb/denom
    }
    dimnames(ans) <- list(pos, NULL)
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
