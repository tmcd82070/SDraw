\name{dec2baseb}
\alias{dec2baseb}
\title{
Decimal to Base b conversion}
\description{
Converts a decimal number (base 10) to an arbitrary base number (base b).}
\usage{
dec2baseb(x, b = 2)
}
\arguments{
  \item{x}{ A vector of base 10 numbers.  All must be integers.}
  \item{b}{The base to convert to.   Base of the output numbers.}
}
\details{This is the inverse function of \code{baseb2dec()}}
\value{
A matrix of size \code{length(x)} X \code{floor(logb(max(x), base=b))+1}.  Each 
row contains the base b digits representing the corresponding base 10 number in x.
}
\author{
Trent McDonald
}

\seealso{
\code{\link{baseb2dec}}
}
\examples{
x <- c(106, 50)
y <- dec2baseb( x, 5 )
baseb2dec( y, 5 )
}
\keyword{ modeling }

